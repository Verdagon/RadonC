package net.verdagon.radonc.templar

import net.verdagon.radonc._

import scala.collection.immutable.List
//import net.verdagon.radonc.carpenter.CovarianceCarpenter
import net.verdagon.radonc.scout._

object TypeTemplar {

  def evaluateAndReferencifyMaybeType(env: Environment, temputs0: Temputs, types1: Option[Type1], ownershipIfMutable: Ownership):
  (Temputs, Option[Reference2]) = {
    types1 match {
      case None => (temputs0, None)
      case Some(head1) => {
        val (temputs1, head2) = evaluateAndReferencifyType(env, temputs0, head1, ownershipIfMutable);
        (temputs1, Some(head2))
      }
    }
  }

  def evaluateAndReferencifyTypes(env: Environment, temputs0: Temputs, types1: List[Type1], ownershipIfMutable: Ownership):
  (Temputs, List[Reference2]) = {
    types1 match {
      case Nil => (temputs0, Nil)
      case head1 :: tail1 => {
        val (temputs1, head2) = evaluateAndReferencifyType(env, temputs0, head1, ownershipIfMutable);
        val (temputs2, tail2) = evaluateAndReferencifyTypes(env, temputs1, tail1, ownershipIfMutable);
        (temputs2, head2 :: tail2)
      }
    }
  }

  def evaluateTypes(env: Environment, temputs0: Temputs, types1: List[Type1]):
      (Temputs, List[ITemplata]) = {
    types1 match {
      case Nil => (temputs0, Nil)
      case head1 :: tail1 => {
        val (temputs1, head2) = evaluateType(env, temputs0, head1);
        val (temputs2, tail2) = evaluateTypes(env, temputs1, tail1);
        (temputs2, head2 :: tail2)
      }
    }
  }

  def evaluateMaybeType(env: Environment, temputs0: Temputs, maybeType1: Option[Type1]):
  (Temputs, Option[ITemplata]) = {
    maybeType1 match {
      case None => (temputs0, None)
      case Some(type1) => {
        val (temputs1, type2) = TypeTemplar.evaluateType(env, temputs0, type1)
        (temputs1, Some(type2))
      }
    };
  }

  def evaluateAndReferencifyType(env: Environment, temputs0: Temputs, type1: Type1, ownershipIfMutable: Ownership):
  (Temputs, Reference2) = {
    val (temputs1, typeTemplata) = evaluateType(env, temputs0, type1)
    val reference =
      typeTemplata match {
        case ReferenceTemplata(r) => r
        case ReferendTemplata(referend) => {
          pointifyReferend(temputs1, referend, ownershipIfMutable)
        }
      }
    (temputs1, reference)
  }

  def referencifyType(
      env: Environment,
      temputs0: Temputs,
      typeTemplata: ITemplata,
      ownershipIfMutable: Ownership):
  Reference2 = {
    typeTemplata match {
      case ReferenceTemplata(r) => r
      case ReferendTemplata(referend) => pointifyReferend(temputs0, referend, ownershipIfMutable)
    }
  }

  def evaluateType(env: Environment, temputs0: Temputs, type1: Type1):
      (Temputs, ITemplata) = {
    assert(type1.isInstanceOf[Type1])
    type1 match {
//      case TemplateParamName1(templateParamName) => {
//        val templateParamTypeTemplata = env.lookupType(templateParamName);
//        val pointerType = templateParamTypeTemplata match {
//          case TemplataType(templateParamType) => {
//            templateParamType match {
//              case p @ Reference2 => p
//              case concrete => Reference2(Owning, concrete.asInstanceOf[ConcreteValue2])
//            }
//          }
//          case TemplataStructTemplate(struct) => throw new RuntimeException("template arg cant be a struct template itself") // but someday, maybe?
//        }
//        (temputs0, pointerType)
//      }
      case TypeName1(name) => {
        (temputs0, env.lookupType(name))
      }
      case Nullable1(innerType1) => {
//        val (temputs1, innerValueType2) = evaluateType(env, temputs0, innerType1)
//        val innerPointerType2 = TypeTemplar.pointify(innerValueType2)
//        env.lookupType("Option") match {
//          case TemplataStructTemplate(_) => {
//            StructTemplar.getStructRef(env.globalEnv, temputs1, "Option", List(TemplataType(innerPointerType2)))
//          }
//        }
        throw new RuntimeException("support unions kkthx")
      }
      case TemplateCall1(templatedTypeName, templateArgs1) => {
        callTemplate(env, temputs0, templatedTypeName, templateArgs1)
      }
      case x => {
        println(x)
        throw new RuntimeException("not yet " + x)
      }
    }
  }

  def callTemplate(env: Environment, temputs0: Temputs, templatedTypeName: String, templateArgs1: List[Type1]):
  (Temputs, ITemplata) = {
    val (temputs1, templateArgTemplatas2) = evaluateTypes(env, temputs0, templateArgs1)

    env.lookupType(templatedTypeName) match {
      case StructTemplateTemplata(_) => {
        val (temputs2, structRef2) =
          StructTemplar.getStructRef(
            env.globalEnv, temputs1, templatedTypeName, templateArgTemplatas2)
        (temputs2, ReferendTemplata(structRef2))
      }
      case ArrayTemplateTemplata(mutable) => {
        val coercedTemplateArgs =
          TypeTemplar.coerceTemplateArgs(
            temputs0,
            List(TemplateParameter1("T", ReferenceTemplataType1)),
            templateArgTemplatas2)
        val tyype =
          coercedTemplateArgs match {
            case List(CoercedTemplateArg2(ReferenceTemplata(ref))) => ref
          }
        val mutability = Templar.getMutability(temputs0, tyype.referend)
        val arrayType = ArrayT2(tyype, mutability)
        (temputs1, ReferendTemplata(arrayType))
      }
      case InterfaceTemplateTemplata(_) => {
        val (temputs2, interfaceRef2) =
          StructTemplar.getInterfaceRef(
            env.globalEnv, temputs1, templatedTypeName, templateArgTemplatas2)
        (temputs2, ReferendTemplata(interfaceRef2))
      }
    }
  }

  def pointifyReferends(temputs: Temputs, valueTypes: List[Referend2], ownershipIfMutable: Ownership): List[Reference2] = {
    valueTypes.map(valueType => pointifyReferend(temputs, valueType, ownershipIfMutable))
  }

  def pointifyReferend(temputs: Temputs, referend: Referend2, ownershipIfMutable: Ownership): Reference2 = {
    referend match {
      case a @ ArrayT2(_, mutability) => {
        val ownership = if (mutability == Mutable) ownershipIfMutable else Share
        Reference2(ownership, a)
      }
      case s @ StructRef2(_, _) => {
        val ownership = if (temputs.lookupMutability(s) == Mutable) ownershipIfMutable else Share
        Reference2(ownership, s)
      }
      case i @ InterfaceRef2(_, _) => {
        val ownership = if (temputs.lookupMutability(i) == Mutable) ownershipIfMutable else Share
        Reference2(ownership, i)
      }
      case Void2() => {
        Reference2(Share, Void2())
      }
      case Int2() => {
        Reference2(Share, Int2())
      }
      case Str2() => {
        Reference2(Share, Str2())
      }
    }
  }

//
//  private def areTypesConvertible(
//      program2: Program2,
//      sourcePointerTypes: List[Reference2],
//      targetPointerTypes: List[Reference2]): Boolean = {
//    (sourcePointerTypes.headOption, targetPointerTypes.headOption) match {
//      case (None, None) => true
//      case (Some(headSourcePointerType), Some(headTargetPointerType)) => {
//        isTypeConvertible(program2, headSourcePointerType, headTargetPointerType) &&
//        areTypesConvertible(program2, sourcePointerTypes.tail, targetPointerTypes.tail)
//      }
//    }
//  }

  def isTypeConvertible(program2: Program2, sourcePointerType: Reference2, targetPointerType: Reference2): Boolean = {
    getTypeDistance(program2, sourcePointerType, targetPointerType).nonEmpty
  }

  // Order of these members matters for comparison
  case class TypeDistance(upcastDistance: Int, ownershipDistance: Int) {
    def lessThanOrEqualTo(that: TypeDistance): Boolean = {
      if (this.upcastDistance < that.upcastDistance) return true;
      if (this.upcastDistance > that.upcastDistance) return false;
      if (this.ownershipDistance < that.ownershipDistance) return true;
      if (this.ownershipDistance > that.ownershipDistance) return false;
      true
    }
  }

  def getTypeDistance(
      program2: Program2,
      sourcePointerType: Reference2,
      targetPointerType: Reference2): Option[TypeDistance] = {
    val Reference2(targetOwnership, targetType) = targetPointerType;
    val Reference2(sourceOwnership, sourceType) = sourcePointerType;

    val upcastDistance =
      if (sourceType == targetType) {
        0
      } else {
        (sourceType, targetType) match {
          case (Nothing2(), _) => 1
          case (_, Any2()) => 1
          case (Void2(), _) => return None
          case (Int2(), _) => return None
          case (Bool2(), _) => return None
          case (Str2(), _) => return None
          case (_, Void2()) => return None
          case (_, Int2()) => return None
          case (_, Bool2()) => return None
          case (_, Str2()) => return None
          case (a : CitizenRef2, b : CitizenRef2) => {
            program2.lookupCitizen(a).measureDistance(program2, b) match {
              case None => return None
              case Some(distance) => distance
            }
          }
        }
      }

    val ownershipDistance =
      (sourceOwnership, targetOwnership) match {
        case (Own, Own) => 0
        case (Borrow, Own) => return None
        case (Own, Borrow) => 1
        case (Borrow, Borrow) => 0
        case (Raw, Raw) => 0
        case (Share, Share) => 0
      }

    Some(TypeDistance(upcastDistance, ownershipDistance))
  }

  def convertExprs(
      env: Environment,
      temputs0: Temputs,
      sourceExprs: List[ReferenceExpression2],
      targetPointerTypes: List[Reference2]):
  List[ReferenceExpression2] = {
    if (sourceExprs.size != targetPointerTypes.size) {
      throw new RuntimeException(sourceExprs + "and\n" + targetPointerTypes)
    }
    (sourceExprs zip targetPointerTypes).map({
      case (sourceExpr, targetPointerType) => {
        convert(env, temputs0, sourceExpr, targetPointerType)
      }
    })
  }

  def convert(
      env: Environment,
      temputs: Temputs,
      sourceExpr: ReferenceExpression2,
      targetPointerType: Reference2):
  ReferenceExpression2 = {
    val sourcePointerType = sourceExpr.resultRegister.reference

    val Reference2(targetOwnership, targetType) = targetPointerType;
    val Reference2(sourceOwnership, sourceType) = sourcePointerType;

    val sourceExprDecayedOwnershipped =
      (sourceOwnership, targetOwnership) match {
        case (Own, Own) => sourceExpr
        case (Borrow, Own) => throw new RuntimeException("Supplied a borrow but target wants to own the argument")
        case (Own, Borrow) => throw new RuntimeException("Supplied an owning but target wants to only borrow")
        case (Borrow, Borrow) => sourceExpr
        case (Raw, Raw) => sourceExpr
        case (Share, Share) => sourceExpr
        case (Own, Share) => {
          throw new RuntimeException(); // curious
        }
        case (Borrow, Share) => {
          throw new RuntimeException(); // curious
        }
      }

    val sourceExprDecayedOwnershippedConverted =
      if (sourceType == targetType) {
        sourceExprDecayedOwnershipped
      } else {
        (sourceType, targetType) match {
          case (s @ StructRef2(_, _), i : InterfaceRef2) => {
            StructTemplar.convert(env.globalEnv, temputs, sourceExprDecayedOwnershipped, s, i)
          }
          case (TemplatedClosure2(structRef2, terry), s @ StructRef2(_, _))
          if structRef2 == s => {
            TemplarReinterpret2(sourceExprDecayedOwnershipped, Reference2(targetOwnership, targetType))
          }
        }
      };

    sourceExprDecayedOwnershippedConverted
  }

  private def getCovariantParamIndices(
      program2: Program2, functionType: FunctionT2): Set[Int] = {
    val supertypes: Set[Referend2] =
      program2.getAllInterfaces.map(_.getRef).toSet ++ program2.getAllStructs.map(_.getRef).toSet
    val isCovariantBools =
      functionType.paramTypes.map(
        pointerType => supertypes.contains(pointerType.referend))
    isCovariantBools.zipWithIndex.filter(_._1 == true).map(_._2).toSet
  }

  // Assumes we already checked the names matched
  private def checkIfOverridesAssumingNamesMatch(
      program2: Program2,
      overridden: FunctionT2,
      overrride: FunctionT2):
  Boolean = {
    val virtualParamIndices = getCovariantParamIndices(program2, overridden)

    virtualParamIndices.forall(virtualParamIndex => {
      checkIfOverridesAtIndexAssumingNamesMatch(
        program2, overridden, overrride, virtualParamIndex)
    })
  }

  // Assumes we already checked the names matched
  private def checkIfOverridesAtIndexAssumingNamesMatch(
      program2: Program2,
      overridden: FunctionT2,
      overrride: FunctionT2,
      virtualParamIndex: Int):
  Boolean = {
    if (overrride.paramTypes.size != overridden.paramTypes.size) {
      return false;
    }
    val testParamTypes = overrride.paramTypes;
    val interfaceParamTypes = overridden.paramTypes
    val paramsAndIndices = testParamTypes.zip(interfaceParamTypes).zipWithIndex
    paramsAndIndices.forall({
      case ((testParamType, interfaceParamType), index) => {
        if (index == virtualParamIndex) {
          TypeTemplar.isTypeConvertible(program2, testParamType, interfaceParamType)
        } else {
          testParamType == interfaceParamType
        }
      }
    })
  }

  def coerceTemplateArg(
      temputs0: Temputs,
      templateParam: TemplateParameter1,
      templateArgTemplata: ITemplata):
  CoercedTemplateArg2 = {
    val TemplateParameter1(name, templateParamTemplataType) = templateParam;
    val coercedTemplateArgTemplata2 =
      (templateParamTemplataType, templateArgTemplata) match {
        case (ReferendTemplataType1, tr @ ReferendTemplata(_)) => tr
        case (ReferenceTemplataType1, tr @ ReferenceTemplata(_)) => tr
        case (TypeTemplateTemplataType1, st @ StructTemplateTemplata(_)) => st
        case (TypeTemplateTemplataType1, it @ InterfaceTemplateTemplata(_)) => it
        case (ReferenceTemplataType1, ReferendTemplata(referend)) => {
          ReferenceTemplata(TypeTemplar.pointifyReferend(temputs0, referend, Own))
        }
      }
    CoercedTemplateArg2(coercedTemplateArgTemplata2)
  }

  def coerceTemplateArgs(
      temputs0: Temputs,
      templateParams: List[TemplateParameter1],
      templateArgTemplatas: List[ITemplata]):
  List[CoercedTemplateArg2] = {
//    took this out because evaluateTemplatedLightBannerAndTerryFromCall calls coerceTemplateArgs on
//    only the explicitly specified template args, not all of them.
//    if (templateArgTemplatas.size != templateParams.size) {
//      throw new RuntimeException("Bad number of template arguments, expected " + templateParams.size + " but received " + templateArgTemplatas.size)
//    }
    templateParams.zip(templateArgTemplatas).map({
      case (tp @ TemplateParameter1(_, _), templateArgTemplata) => {
        coerceTemplateArg(temputs0, tp, templateArgTemplata)
      }
    })
  }
}