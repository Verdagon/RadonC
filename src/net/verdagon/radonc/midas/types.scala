package net.verdagon.radonc.midas

import net.verdagon.radonc.hammer.FunctionT3
import net.verdagon.radonc.scout.{Immutable, Mutability, Mutable}
import net.verdagon.radonc.templar._

sealed trait Register4
case class VariableAddressRegister4(reference: Reference4) extends Register4
case class ReferenceRegister4(reference: Reference4) extends Register4

trait Concrete4
case class FunctionT4(
    origin: FunctionT3,
    paramTypes: List[Reference4],
    returnType: Reference4) extends Concrete4
case class Int4() extends Concrete4
case class Bool4() extends Concrete4
case class Float4() extends Concrete4
case class Struct4(structDef4: StructDefinition4) extends Concrete4

trait Reference4 {
  def size: Int
  def align: Int
}
case class FunctionReference4(functionType: FunctionT4) extends Reference4 {
  override def size: Int = 8
  override def align: Int = 8
}
case class IntReference4() extends Reference4 {
  override def size: Int = 8
  override def align: Int = 8
}
case class BoolReference4() extends Reference4 {
  override def size: Int = 1
  override def align: Int = 1
}
case class FloatReference4() extends Reference4 {
  override def size: Int = 4
  override def align: Int = 4
}
case class InterfaceReference4(
    id: InterfaceId4,
    ownership: Ownership,
    mutability: Mutability) extends Reference4 {
  override def size: Int = 16
  override def align: Int = 8
}

sealed trait StructReference4 extends Reference4
case class InlineStructReference4(
    id: StructId4,
    ownership: Ownership,
    mutability: Mutability,
    size: Int,
    align: Int) extends StructReference4 {
}
case class FarStructReference4(
    id: StructId4,
    ownership: Ownership,
    mutability: Mutability) extends StructReference4 {
  override def size: Int = 8
  override def align: Int = 8

  if (ownership == Own && mutability == Immutable) { assert(false); } // cant own an immutable
}
