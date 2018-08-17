package net.verdagon.radonc.scout

case class Let0(pattern: Pattern1, expr: Expression0) extends Expression0
// patternId is a unique number, can be used to make temporary variables that wont
// collide with other things
case class Let1(patternId: Int, pattern: Pattern1, expr: Expression1) extends Expression1

case class If0(condition: Lambda0, thenBody: Lambda0, elseBody: Lambda0) extends Expression0
case class If1(condition: Function1, thenBody: Function1, elseBody: Function1) extends Expression1

case class Mutate0(mutatee: Expression0, expr: Expression0) extends Expression0
case class Mutate1(mutatee: Expression1, expr: Expression1) extends Expression1

case class Lend0(expr: Expression0) extends Expression0

case class Mutable0(expr: Expression0) extends Expression0


//case class CurriedFunc3(closureExpr: Expression3, funcName: String) extends Expression3

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use

case class Panic1() extends Expression1

case class Block0(elements: List[Expression0]) extends Expression0
case class Block1(elements: List[Expression1]) extends Expression1

case class Construct1(
  tyype: Type1,
  args: List[Expression1]) extends Expression1

case class ConstructArray1(
    sizeExpr: Expression1,
    generatorExpr: Expression1,
    mutability: Mutability) extends Expression1

 // These things will be separated by semicolons, and all be joined in a block
case class RepeaterBlock0(expression: Expression0) extends Expression0
case class RepeaterBlock1(expression: Expression1) extends Expression1

// Results in a pack, represents the differences between the expressions
case class RepeaterBlockIterator0(expression: Expression0) extends Expression0
case class RepeaterBlockIterator1(expression: Expression1) extends Expression1

case class Pack0(elements: List[Expression0]) extends Expression0
case class PackE1(elements: List[Expression1]) extends Expression1

case class Sequence0(elements: List[Expression0]) extends Expression0
case class SequenceE1(elements: List[Expression1]) extends Expression1

// This thing will be repeated, separated by commas, and all be joined in a pack
case class RepeaterPack0(expression: Expression0) extends Expression0
case class RepeaterPack1(expression: Expression1) extends Expression1

// Results in a pack, represents the differences between the elements
case class RepeaterPackIterator0(expression: Expression0) extends Expression0
case class RepeaterPackIterator1(expression: Expression1) extends Expression1

case class IntLiteral1(value: Int) extends Expression0 with Expression1

case class BoolLiteral1(value: Boolean) extends Expression0 with Expression1

case class StrLiteral1(value: String) extends Expression0 with Expression1

case class FloatLiteral1(value: Float) extends Expression0 with Expression1

case class Dot0(left: Expression0, member: String) extends Expression0
case class Dot1(left: Expression1, member: String) extends Expression1

case class DotCall0(left: Expression0, indexExpr: Pack0) extends Expression0
case class DotCall1(left: Expression1, indexExpr: Expression1) extends Expression1

case class TemplateSpecifiedLookup0(name: String, templateArgs: List[Type1]) extends Expression0
case class TemplateSpecifiedLookup1(name: String, templateArgs: List[Type1]) extends Expression1

case class Lookup0(name: String) extends Expression0
case class Lookup1(name: String) extends Expression1

case class Scramble0(elements: List[Expression0]) extends Expression0 {
  assert(!elements.isEmpty, "Can't have an empty scramble")
}
case class Scramble1(elements: List[Expression1]) extends Expression1 {
  assert(!elements.isEmpty, "Can't have an empty scramble")
}
