# calculator
# Built with Seahorse v0.1.0
#
# Gives users their own on-chain four-function calculator!

from seahorse.prelude import *

declare_id('Fg6PaFpoGXkYsidMpWTK6W2BeZ7FEfcYkg476zPFsLnS')

class Calculator(Account):
  owner: Pubkey
  display: i64

class Operation(Enum):
  ADD = 0
  SUB = 1
  MUL = 2
  DIV = 3

@instruction
def init_calculator(owner: Signer, calculator: Empty[Calculator]):
  calculator = calculator.init(payer = owner, seeds = ['Calculator', owner])
  calculator.owner = owner.key()

@instruction
def reset_calculator(owner: Signer, calculator: Calculator):
  print(owner.key(), 'is resetting a calculator', calculator.key())

  assert owner.key() == calculator.owner, 'This is not your calculator!'

  calculator.display = 0

@instruction
def do_operation(owner: Signer, calculator: Calculator, op: Operation, num: i64):
  assert owner.key() == calculator.owner, 'This is not your calculator!'

  if op == Operation.ADD:
    calculator.display += num
  elif op == Operation.SUB:
    calculator.display -= num
  elif op == Operation.MUL:
    calculator.display *= num
  elif op == Operation.DIV:
    calculator.display //= num
