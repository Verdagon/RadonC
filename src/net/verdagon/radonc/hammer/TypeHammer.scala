package net.verdagon.radonc.hammer

import net.verdagon.radonc.templar._

object TypeHammer {
  def translateMembers(hinputs: Hinputs, hamuts0: Hamuts, members: List[StructMember2]):
  (Hamuts, List[StructMember3]) = {
    members match {
      case Nil => (hamuts0, Nil)
      case headMember2 :: tailMembers2 => {
        val (hamuts1, headMember3) = translateMember(hinputs, hamuts0, headMember2)
        val (hamuts2, tailMembers3) = translateMembers(hinputs, hamuts1, tailMembers2)
        (hamuts2, headMember3 :: tailMembers3)
      }
    }
  }

  def translateMember(hinputs: Hinputs, hamuts: Hamuts, member2: StructMember2):
  (Hamuts, StructMember3) = {
    val (hamuts2, member3) =
      member2.tyype match {
        case ReferenceMemberType2(reference2) => {
          val (hamuts1, reference3) = TypeHammer.translateReference(hinputs, hamuts, reference2)
          (hamuts1, ReferenceMemberType3(reference3))
        }
        case AddressMemberType2(reference2) => {
          val (hamuts1, reference3) = TypeHammer.translateReference(hinputs, hamuts, reference2)
          (hamuts1, AddressMemberType3(reference3))
        }
      }
    (hamuts2, StructMember3(member2.name, member3))
  }

//
//  def translateType(hinputs: Hinputs, hamuts: Hamuts, tyype: BaseType2):
//  (Hamuts, BaseType3) = {
//    tyype match {
//      case Addressible2(innerType) => {
//        val (hamuts1, pointer3) = translatePointer(hinputs, hamuts, innerType)
//        (hamuts1, Addressible3(pointer3))
//      }
//      case Reference2(ownership, innerType) => {
//        val (hamuts1, pointer3) = translate(hinputs, hamuts, innerType)
//        (hamuts1, Pointer3(ownership, pointer3))
//      }
//    }
//  }

  //  def translatePointer(tyype: Reference2): Pointer3 = {
  //  }

  def translateFunction(
      hinputs: Hinputs, hamuts0: Hamuts, tyype: FunctionT2):
  (Hamuts, FunctionT3) = {
    val FunctionT2(paramTypes, returnType) = tyype;
    val (hamuts1, returnType3) = translateReference(hinputs, hamuts0, tyype.returnType)
    val (hamuts2, paramTypes3) = translateReferences(hinputs, hamuts1, tyype.paramTypes)
    (hamuts2, FunctionT3(paramTypes3, returnType3))
  }

  private def translate(hinputs: Hinputs, hamuts0: Hamuts, tyype: Referend2):
  (Hamuts, Referend3) = {
    tyype match {
      case Int2() => (hamuts0, Int3())
      case Bool2() => (hamuts0, Bool3())
      case Float2() => (hamuts0, Float3())
      case Str2() => (hamuts0, Str3())
      case Void2() => (hamuts0, Void3())
      case t : FunctionT2 => translateFunction(hinputs, hamuts0, t)
      case s@ StructRef2(_, _) => StructHammer.translateStructRef(hinputs, hamuts0, s)

      case i : InterfaceRef2 => StructHammer.translateInterfaceRef(hinputs, hamuts0, i)

      // A Closure2 is really just a struct ref under the hood. The dinstinction is only meaningful
      // to the Templar.
      case c @ OrdinaryClosure2(handleStructRef, prototype) => translate(hinputs, hamuts0, handleStructRef)
      case c @ TemplatedClosure2(handleStructRef, terry) => translate(hinputs, hamuts0, handleStructRef)
      case GlobalFunctionGroup2(understructRef2, name, alreadySpecifiedTemplateArgs) => {
        StructHammer.translateStructRef(hinputs, hamuts0, understructRef2)
      }

      // A PackT2 is really just a struct ref under the hood. The dinstinction is only meaningful
      // to the Templar.
      case p @ PackT2(_, underlyingStruct) => StructHammer.translateStructRef(hinputs, hamuts0, underlyingStruct)
      case p @ TupleT2(_, underlyingStruct) => StructHammer.translateStructRef(hinputs, hamuts0, underlyingStruct)
      case a @ ArraySequenceT2(_, array) => translateArray(hinputs, hamuts0, array)
      case a @ ArrayT2(_, _) => translateArray(hinputs, hamuts0, a)
    }
  }

  def translateReference(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      reference2: Reference2):
  (Hamuts, Reference3) = {
    val Reference2(ownership, innerType) = reference2;
    val (hamuts1, inner3) = translate(hinputs, hamuts0, innerType);
    (hamuts1, Reference3(ownership, inner3))
  }

  def translateReferences(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      references2: List[Reference2]):
  (Hamuts, List[Reference3]) = {
    references2 match {
      case Nil => (hamuts0, Nil)
      case headReference2 :: tailPointers2 => {
        val (hamuts1, headPointer3) = translateReference(hinputs, hamuts0, headReference2)
        val (hamuts2, tailPointers3) = translateReferences(hinputs, hamuts1, tailPointers2)
        (hamuts2, headPointer3 :: tailPointers3)
      }
    }
  }

//  def checkReference(baseType3: BaseType3): Reference3 = {
//    baseType3 match {
//      case Addressible3(_) => throw new RuntimeException("Expected a pointer, was an addressible!")
//      case p @ Reference3(_, _) => p
//    }
//  }

  def checkConversion(expected: Reference3, actual: Reference3): Unit = {
    if (actual != expected) {
      throw new RuntimeException("Expected a " + expected + " but was a " + actual);
    }
  }

  def translateArray(hinputs: Hinputs, hamuts0: Hamuts, type2: ArrayT2) = {
    val (hamuts1, memberReference3) = TypeHammer.translateReference(hinputs, hamuts0, type2.memberType)
    (hamuts1, ArrayT3(memberReference3))
  }
}
