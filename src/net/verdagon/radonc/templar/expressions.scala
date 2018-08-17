package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.Mutable

case class Let2(
    name: String,
    mutable: Boolean,
    expr: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(PackTemplar.emptyPackReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

case class Destroy2(name: String) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(PackTemplar.emptyPackReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

// WARNING: *ALL* of the arguments to the then and else are evaluated *BEFORE*
// we get to the branch instruction! They should really just be silly instructions
// like borrows, lends, assembling structs for closures, etc.
// It's like this to force us to move blocks out into their own functions.
case class If2(
    condition: ReferenceExpression2,
    thenCall: FunctionPointerCall2,
    elseCall: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister = thenCall.resultRegister

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ condition.all(func) ++ thenCall.all(func) ++ elseCall.all(func)
  }
}

case class Panic2() extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Nothing2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class Mutate2(
    destinationExpr: AddressExpression2,
    sourceExpr: ReferenceExpression2) extends ReferenceExpression2 {
  assert(destinationExpr.resultRegister.reference == sourceExpr.resultRegister.reference)
  override def resultRegister = ReferenceRegister2(Reference2(Share, PackTemplar.emptyPackType))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ destinationExpr.all(func) ++ sourceExpr.all(func)
  }
}


//case class CurriedFunc3(closureExpr: Expression3, funcName: String) extends Expression3

// when we make a closure, we make a struct full of pointers to all our variables
// and the first element is our parent closure
// this can live on the stack, since blocks are limited to this expression
// later we can optimize it to only have the things we use

case class Block2(elements: List[Expression2]) extends ReferenceExpression2 with Queriable2 {
  assert(elements.last.isInstanceOf[ReferenceExpression2])
  val lastReferenceExpr = elements.last.asInstanceOf[ReferenceExpression2]
  override def resultRegister = lastReferenceExpr.resultRegister

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func))
  }
}

case class PackE2(
    elements: List[ReferenceExpression2],
    resultReference: Reference2,
    packType: PackT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ packType.all(func)
  }
}

case class TupleE2(
    elements: List[ReferenceExpression2],
    resultReference: Reference2,
    tupleType: TupleT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ tupleType.all(func)
  }
}

case class ArraySequenceE2(
    elements: List[ReferenceExpression2],
    resultReference: Reference2,
    arrayType: ArraySequenceT2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ arrayType.all(func)
  }
}

case class IntLiteral2(value: Int) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Int2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class BoolLiteral2(value: Boolean) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Bool2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class NoneLiteral2() extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Nothing2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class StrLiteral2(value: String) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Str2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class FloatLiteral2(value: Float) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Float2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class LocalLookup2(name: String, reference: Reference2) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(reference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}

//case class PackLookup2(packExpr: Expression2, index: Int) extends Expression2 {
//  override def resultType: BaseType2 = {
//    // A pack can never be in a changeable variable, and so can't be an addressible, so will always
//    // be a pointer.
//    // (it can be in a final variable, when its spawned by pattern matching)
//    TypeUtils.softDecay(packExpr.resultType).innerType match {
//      case PackT2(memberTypes, underlyingStructRef) => memberTypes(index)
//    }
//  }
//
//  def all[T](func: PartialFunction[Ast2, T]): List[T] = {
//    List(this).collect(func) ++ packExpr.all(func)
//  }
//}

case class ArrayLookup2(
    arrayExpr: ReferenceExpression2,
    arrayType: ArrayT2,
    indexExpr: ReferenceExpression2) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(arrayType.memberType)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayExpr.all(func) ++ indexExpr.all(func) ++ arrayType.all(func)
  }
}

case class ArrayLength2(arrayExpr: ReferenceExpression2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(Reference2(Share, Int2()))
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayExpr.all(func)
  }
}

case class ReferenceMemberLookup2(
    name: String,
    structExpr: ReferenceExpression2,
    reference: Reference2) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(reference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structExpr.all(func) ++ resultRegister.all(func)
  }
}
case class AddressMemberLookup2(
    name: String,
    structExpr: ReferenceExpression2,
    reference: Reference2) extends AddressExpression2 {
  override def resultRegister = AddressRegister2(reference)
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structExpr.all(func) ++ resultRegister.all(func)
  }
}

case class FunctionLookup2(prototype: Prototype2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 =
    ReferenceRegister2(Reference2(Raw, prototype.functionType))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ prototype.all(func)
  }
}
case class InterfaceFunctionCall2(
    familyRootBanner: FunctionBanner2,
    functionType: Reference2,
    resultReference: Reference2,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 =
    ReferenceRegister2(resultReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ familyRootBanner.all(func) ++ functionType.all(func) ++ resultRegister.all(func) ++ args.flatMap(_.all(func))
  }
}

case class ExternFunctionCall2(
    prototype2: Prototype2,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  assert(prototype2.templateArgs.isEmpty)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ args.flatMap(_.all(func))
  }

  override def resultRegister = ReferenceRegister2(prototype2.functionType.returnType)
}

case class FunctionPointerCall2(
    callable: ReferenceExpression2,
    args: List[ReferenceExpression2]) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(functionType.returnType)
  }
  def functionType: FunctionT2 = {
    callable.resultRegister.reference.referend match {
      case ft2 @ FunctionT2(_, returnType) => ft2
    }
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ callable.all(func) ++ args.flatMap(_.all(func))
  }
}
case class And2(
    left: ReferenceExpression2,
    right: ReferenceExpression2) extends ReferenceExpression2 {

  override def resultRegister = ReferenceRegister2(Reference2(Share, Bool2()))

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ left.all(func) ++ right.all(func)
  }
}

case class Tuple2(
    elements: List[ReferenceExpression2],
    tupleReference: Reference2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(tupleReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ elements.flatMap(_.all(func)) ++ resultRegister.all(func)
  }
}

// A templar reinterpret is interpreting a type as a different one which is hammer-equivalent.
// For example, a pack and a struct are the same thing to hammer.
// Also, a closure and a struct are the same thing to hammer.
// But, Templar attaches different meanings to these things. The templar is free to reinterpret
// between hammer-equivalent things as it wants.
case class TemplarReinterpret2(
    expr: ReferenceExpression2,
    resultReference: Reference2) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func) ++ resultRegister.all(func)
  }
}

case class Construct2(
    structRef: StructRef2,
    resultReference: Reference2,
    args: List[Expression2]) extends ReferenceExpression2 {
  override def resultRegister = ReferenceRegister2(resultReference)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structRef.all(func) ++ args.flatMap(_.all(func))
  }
}

// Note: the functionpointercall's last argument is just a hardcoded integer,
// it's up to later stages to replace that with an actual index
case class ConstructArray2(
    arrayType: ArrayT2,
    sizeExpr: ReferenceExpression2,
    call: FunctionPointerCall2) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Reference2(
        if (arrayType.mutability == Mutable) Own else Share,
        arrayType))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ arrayType.all(func) ++ sizeExpr.all(func) ++ call.all(func)
  }
}

case class InterfaceToInterfaceUpcast2(
    innerExpr: ReferenceExpression2,
    targetInterfaceRef: InterfaceRef2) extends ReferenceExpression2 {
  def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Reference2(
        innerExpr.resultRegister.reference.ownership,
        targetInterfaceRef))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ innerExpr.all(func) ++ targetInterfaceRef.all(func)
  }
}

case class StructToInterfaceUpcast2(innerExpr: ReferenceExpression2, targetInterfaceRef: InterfaceRef2) extends ReferenceExpression2 {
  def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(
      Reference2(
        innerExpr.resultRegister.reference.ownership,
        targetInterfaceRef))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ innerExpr.all(func) ++ targetInterfaceRef.all(func)
  }
}

// A soft load is one that turns an int** into an int*. a hard load turns an int* into an int.
// Turns an Addressible(Pointer) into an OwningPointer. Makes the source owning pointer into null

// If the source was an own and target is borrow, that's a lend

case class SoftLoad2(expr: AddressExpression2, targetOwnership: Ownership) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    ReferenceRegister2(Reference2(targetOwnership, expr.resultRegister.reference.referend))
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

// If source was an own and target is borrow, that's a lend
// (thats the main purpose of this)
case class Alias2(expr: ReferenceExpression2, targetOwnership: Ownership) extends ReferenceExpression2 {
  override def resultRegister: ReferenceRegister2 = {
    expr.resultRegister.reference match {
      case Reference2(_, innerType) => ReferenceRegister2(Reference2(targetOwnership, innerType))
    }
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ expr.all(func)
  }
}

object ExpressionUtils {
  def flattenPack(expr: ReferenceExpression2): List[ReferenceExpression2] = {
    expr match {
      case PackE2(elements, resultType, packType) => {
        elements.map(flattenPack).foldLeft(List[ReferenceExpression2]())(_ ++ _)
      }
      case _ => List(expr)
    }
  }
}
