package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.Mutable

object NameTemplar {
  // Identifier names need to come from the Templar output because some things are erased
  // by Hammer types, such as template args. Hammer will sometimes output many functions
  // with the same signature because of this.

  def getReferenceIdentifierName(reference: Reference2): String = {
    val Reference2(ownership, referend) = reference;
    val ownershipString =
      ownership match {
        case Share => "sref"
        case Borrow => "bref"
        case Own => "oref"
        case Raw => "rref"
      }
    ownershipString + ":" + getReferendIdentifierName(referend)
  }

  def getReferendIdentifierName(tyype: Referend2): String = {
    tyype match {
      case Int2() => "i64"
      case Float2() => "double"
      case Bool2() => "i1"
      case Str2() => "__Str"
      case Void2() => "__Void"
      case ArrayT2(memberType2, mutability) => (if (mutability == Mutable) "mutarray" else "immarray") + ":" + getReferenceIdentifierName(memberType2)
      case FunctionT2(paramTypes, returnType) => {
        "func:" + getReferenceIdentifierName(returnType) + ":" + paramTypes.size + paramTypes.map(getReferenceIdentifierName).map(":" + _).mkString("")
      }
      case PackT2(innerTypes, underlyingStruct) => {
        getReferendIdentifierName(underlyingStruct)
        //        "pack:" + memberTypes.size + memberTypes.map(getIdentifierName).map(":" + _).mkString("")
      }
      case StructRef2(name, templateArgTypes) => {
        "struct:" + name + ":" + templateArgTypes.size + templateArgTypes.map(_.templata).map(getIdentifierName).map(":" + _).mkString("")
      }
      case InterfaceRef2(name, templateArgTypes) => {
        "interface:" + name + ":" + templateArgTypes.size + templateArgTypes.map(_.templata).map(getIdentifierName).map(":" + _).mkString("")
      }
      case OrdinaryClosure2(underlyingStruct, prototype2) => {
        getReferendIdentifierName(underlyingStruct)
      }
      case TemplatedClosure2(underlyingStruct, terry) => {
        getReferendIdentifierName(underlyingStruct)
      }
      case GlobalFunctionGroup2(understructRef2, name, alreadySpecifiedTemplateArgs) => {
        getReferendIdentifierName(understructRef2)
      }
    }
  }

  private def getIdentifierName(tyype: ITemplata): String = {
    tyype match {
      case ReferendTemplata(referend) => "t:" + getReferendIdentifierName(referend)
      case ReferenceTemplata(reference) => "t:" + getReferenceIdentifierName(reference)
      case StructTemplateTemplata(struct1) => "s1id:" + struct1.struct1Id
      case InterfaceTemplateTemplata(interface1) => "i1id:" + interface1.interface1Id
    }
  }

  def getIdentifierName(prototype: Prototype2): String = {
    val Prototype2(humanName, explicitTemplateArgs, FunctionT2(paramsTypes2, returnType2)) = prototype;
    val paramsStr = paramsTypes2.map(p => ":" + getReferenceIdentifierName(p)).mkString("")
    val explicitTemplateArgsStr = explicitTemplateArgs.map(p => ":" + getIdentifierName(p.templata)).mkString(",")
    humanName + (":" + explicitTemplateArgs.size) + explicitTemplateArgsStr + (":" + paramsTypes2.size) + paramsStr
  }
}
