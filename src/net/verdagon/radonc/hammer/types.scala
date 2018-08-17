package net.verdagon.radonc.hammer

import net.verdagon.radonc.templar._

case class Reference3(ownership: Ownership, innerType: Referend3) {
  innerType match {
    case Int3() | Bool3() | Str3() | Void3() | Float3() => {
      assert(ownership == Share)
    }
    case _ =>
  }
}

trait Referend3

case class Int3() extends Referend3
case class Bool3() extends Referend3
case class Str3() extends Referend3
case class Void3() extends Referend3
case class Float3() extends Referend3

case class InterfaceRef3(interfaceId: Int, globalName: String) extends Referend3

case class StructRef3(structId: Int, globalName: String) extends Referend3

case class FunctionT3(
    paramTypes: List[Reference3],
    returnType: Reference3
) extends Referend3

case class ArrayT3(memberType: Reference3) extends Referend3
