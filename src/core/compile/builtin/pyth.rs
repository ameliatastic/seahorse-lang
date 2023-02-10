//! Seahorse Pyth includes builtin types and functions for working with the Pyth price oracle.

// why does every builtin have to start with a P

use std::collections::BTreeMap;

use crate::core::compile::builtin::*;
pub use crate::core::{
    compile::{ast::*, build::*, check::*, namespace::*, sign::*},
    util::*,
};
use crate::data::get_pyth_price_address;
use crate::match1;
use base58::FromBase58;
use quote::quote;
use regex::Regex;

#[derive(Clone, Debug, PartialEq)]
pub enum Pyth {
    // Types
    PriceAccount,
    PriceFeed,
    Price,
}

/// Create the seahorse.pyth namespace.
pub fn namespace() -> Namespace {
    let data = [
        ("PriceAccount", Pyth::PriceAccount),
        ("PriceFeed", Pyth::PriceFeed),
        ("Price", Pyth::Price),
    ];

    let mut namespace = BTreeMap::new();
    for (name, obj) in data.into_iter() {
        namespace.insert(
            name.to_string(),
            NamespacedObject::Item(Item::Builtin(Builtin::Pyth(obj))),
        );
    }

    return namespace;
}

impl BuiltinSource for Pyth {
    fn name(&self) -> String {
        match self {
            Self::PriceAccount => "PriceAccount",
            Self::PriceFeed => "PriceFeed",
            Self::Price => "Price",
        }
        .to_string()
    }

    fn ty(&self) -> Ty {
        match self {
            Self::PriceAccount | Self::PriceFeed | Self::Price => {
                Ty::Type(TyName::Builtin(Builtin::Pyth(self.clone())), None)
            }
        }
    }

    fn as_instance(&self, params: &Vec<Ty>) -> CResult<()> {
        match self {
            _ if params.len() == 0 => Ok(()),
            _ => Err(CoreError::make_raw("invalid type", "")),
        }
    }

    fn attr(&self, attr: &String) -> Option<(Ty, Ty)> {
        let ty_no_params = Ty::pyth(self.clone(), vec![]);

        match (self, attr.as_str()) {
            // Price.validate_price_feed(str) -> PriceFeed
            (Self::PriceAccount, "validate_price_feed") => Some((
                ty_no_params,
                Ty::new_function(
                    vec![(
                        "product",
                        Ty::python(Python::Str, vec![]),
                        ParamType::Required,
                    )],
                    Ty::Transformed(
                        Ty::pyth(Self::PriceFeed, vec![]).into(),
                        Transformation::new_with_context(
                            |mut expr, _| {
                                let (function, product) = match1!(expr.obj, ExpressionObj::Call { function, args } => (*function, args.into_iter().next().unwrap()));
                                let price = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                                let product = if let ExpressionObj::Literal(Literal::Str(product)) =
                                    product.obj
                                {
                                    product
                                } else {
                                    return Err(CoreError::make_raw(
                                        "Price.validate() requires a string literal",
                                        "",
                                    ));
                                };

                                let product_re =
                                    Regex::new(r"^((mainnet|devnet|testnet)-)?\w+/\w+$").unwrap();
                                let caps = product_re
                                .captures(&product)
                                .ok_or(CoreError::make_raw(
                                    "invalid Pyth product string",
                                    concat!(
                                        "Help: Pyth product strings must have the format\n\n",

                                        "    [cluster-]BASE/QUOTE\n\n",

                                        "(where brackets means optional, defaulting to mainnet). For example, the following are all valid:\n\n",

                                        "    SOL/USD\n",
                                        "    mainnet-SOL/USD\n",
                                        "    devnet-AAPL/USD\n",
                                        "    testnet-USD/JPY"
                                    )
                                ))?;

                                let product = match caps.get(1) {
                                    Some(_) => product,
                                    None => format!("mainnet-{}", product.as_str()),
                                };

                                let key = get_pyth_price_address(&product)
                                    .ok_or(CoreError::make_raw(
                                        "could not find price account for product",
                                        "",
                                    ))?
                                    .from_base58()
                                    .unwrap();

                                let msg = format!(
                                    "Pyth PriceAccount validation failed: expected {}",
                                    product
                                );

                                expr.obj = ExpressionObj::Rendered(quote! {
                                    {
                                        if #price.key() != Pubkey::new_from_array([#(#key),*]) {
                                            panic!(#msg)
                                        }
                                        load_price_feed_from_account_info(&#price).unwrap()
                                    }
                                });

                                Ok(Transformed::Expression(expr))
                            },
                            // Seed context is added to prevent the string literal from expanding into
                            // a call to .to_string()
                            Some(ExprContext::Seed),
                        ),
                    ),
                ),
            )),
            // PriceFeed.get_price() -> Price
            (Self::PriceFeed, "get_price") => Some((
                ty_no_params,
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::pyth(Self::Price, vec![]).into(),
                        Transformation::new(|mut expr| {
                            let function = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                            let price = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                #price.get_price_unchecked()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            // Price.price: i64
            (Self::Price, "price") => Some((
                ty_no_params,
                Ty::prelude(Prelude::RustInt(true, 64), vec![]),
            )),
            // Price.conf: u64
            (Self::Price, "conf") => Some((
                ty_no_params,
                Ty::prelude(Prelude::RustInt(false, 64), vec![]),
            )),
            // Price.expo: i32
            (Self::Price, "expo") => Some((
                ty_no_params,
                Ty::prelude(Prelude::RustInt(true, 32), vec![]),
            )),
            // Price.publish_time: i64
            (Self::Price, "publish_time") => Some((
                ty_no_params,
                Ty::prelude(Prelude::RustInt(true, 64), vec![]),
            )),
            // Price.num() -> f64
            (Self::Price, "num") => Some((
                ty_no_params,
                Ty::new_function(
                    vec![],
                    Ty::Transformed(
                        Ty::prelude(Prelude::RustFloat, vec![]).into(),
                        Transformation::new(|mut expr| {
                            // TODO this "extract caller and args" pattern is used a lot, should turn into a function
                            let function = match1!(expr.obj, ExpressionObj::Call { function, .. } => *function);
                            let price = match1!(function.obj, ExpressionObj::Attribute { value, .. } => *value);

                            expr.obj = ExpressionObj::Rendered(quote! {
                                {
                                    let price = #price;
                                    (price.price as f64) * 10f64.powf(price.expo as f64)
                                }
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                ),
            )),
            _ => None,
        }
    }

    fn index(&self) -> Option<(Ty, Ty)> {
        None
    }

    fn static_attr(&self, _attr: &String) -> Option<Ty> {
        None
    }

    fn casted(&self, ty: &Ty) -> Option<(Ty, Ty)> {
        let builtin = if let Ty::Generic(TyName::Builtin(builtin), _) = ty {
            builtin
        } else {
            return None;
        };

        match self {
            Self::PriceAccount => match builtin {
                Builtin::Prelude(Prelude::Account | Prelude::InitAccount) => {
                    Some((Ty::pyth(self.clone(), vec![]), ty.clone()))
                }
                Builtin::Prelude(Prelude::Seed) => Some((
                    Ty::pyth(self.clone(), vec![]).into(),
                    Ty::Transformed(
                        ty.clone().into(),
                        Transformation::new(|mut expr| {
                            let obj = expr.obj.without_borrows();
                            expr.obj = ExpressionObj::Rendered(quote! {
                                #obj.key().as_ref()
                            });

                            Ok(Transformed::Expression(expr))
                        }),
                    ),
                )),
                _ => None,
            },
            _ => None,
        }
    }
}
