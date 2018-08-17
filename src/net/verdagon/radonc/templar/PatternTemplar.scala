package net.verdagon.radonc.templar;

import net.verdagon.radonc.scout._
import net.verdagon.radonc._
import net.verdagon.radonc.scout.{Pattern1, Parameter1}

// either want:
// 1. (nonchecking) thing that just trusts its good and extracts it into locals. (for lets)
// 2. (checking) thing that checks it matches, returns None if not, otherwise returns
//    a struct containing it all (for signatures)

object PatternTemplar {

  def nonCheckingTranslateList(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternIds: List[Int],
      patterns1: List[Pattern1],
      patternInputExprs2: List[SoftLoad2]):
  (Temputs, List[Let2]) = {
    patternIds.zip(patterns1).zip(patternInputExprs2) match {
      case Nil => (temputs0, Nil)
      case ((patternId, CaptureP1(_, _, Some(pattern1))), patternInputExpr2) :: tail => {
        val (temputs1, headLets) =
          PatternTemplar.nonCheckingTranslate(
            env, temputs0, patternId, pattern1, patternInputExpr2);
        val (temputs2, tailLets) =
          nonCheckingTranslateList(env, temputs1, patternIds.tail, patterns1.tail, patternInputExprs2.tail)
        (temputs2, headLets ++ tailLets)
      }
      case _ => throw new RuntimeException("wat")
    }
  }

  def nonCheckingTranslate(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternId: Int,
      pattern: Pattern1,
      inputLookupExpr: SoftLoad2):
  (Temputs, List[Let2]) = {
    val (temputs1, _, _, lets) =
        innerNonCheckingTranslate(
          env, temputs0, patternId, 1, false, pattern, inputLookupExpr)
    (temputs1, lets)
  }

  // the #1 case above
  // returns the temputs, the new seq num, the resulting pointer type, and a bunch of lets.
  private def innerNonCheckingTranslate(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternId: Int,
      seqNum0: Int,
      borrow: Boolean, // Whether we're inside a &
      pattern: Pattern1,
      inputLookupExpr: SoftLoad2):
      (Temputs, Int, Reference2, List[Let2]) = {
//    val decayedInputLookupExpr = Templar.maybeLendSoftDereference(undecayedInputLookupExpr);

    pattern match {
      case TypeOfP1(TemplateCall1(templateName, templateArgTypes1)) => {
        val (temputs2, expectedCitizenRef2) =
          env.lookupType(templateName) match {
            case ReferendTemplata(tyype) => throw new RuntimeException("wat")
            case ReferenceTemplata(tyype) => throw new RuntimeException("wat")
            case StructTemplateTemplata(struct1) => {
              val (temputs1, templateArgValueTypes2) =
                TypeTemplar.evaluateTypes(env, temputs0, templateArgTypes1)
              StructTemplar.getStructRef(env.globalEnv, temputs1, struct1.name, templateArgValueTypes2)
            }
            case InterfaceTemplateTemplata(interface1) => {
              val (temputs1, templateArgValueTypes2) =
                TypeTemplar.evaluateTypes(env, temputs0, templateArgTypes1)
              StructTemplar.getInterfaceRef(
                env.globalEnv, temputs1, interface1.name, templateArgValueTypes2)
            }
          }

        // Our resulting variable will have this ownership
        val expectedCitizenDef2 = temputs2.lookupCitizen(expectedCitizenRef2)

        val expectedOwnership =
          if (expectedCitizenDef2.mutability == Immutable) {
            Share
          } else {
            if (borrow) Borrow else Own
          }

        val expectedPointerType = Reference2(expectedOwnership, expectedCitizenRef2)

        // Don't need output, since we're just doing a compile time check here
        TypeTemplar.convert(env, temputs2, inputLookupExpr, expectedPointerType)

        (temputs2, seqNum0, expectedPointerType, List())
      }
      case TypeOfP1(type1 @ TypeName1(name)) => {
        val (temputs1, targetReference) =
          TypeTemplar.evaluateAndReferencifyType(
            env, temputs0, type1, if (borrow) Borrow else Own)

        if (borrow && targetReference.ownership != Borrow) {
          throw new RuntimeException("cant borrow, the incoming thing wasnt a borrow, was a " + targetReference.ownership)
        }

        // Don't need output, since we're just doing a compile time check here
        TypeTemplar.convert(env, temputs1, inputLookupExpr, targetReference)

        (temputs1, seqNum0, targetReference, List())
      }
//      case TemplateTypeOf1(name) => {
//        val tyype =
//            env.lookup(name) match {
//              case TemplataType(tyype) => tyype
//              case _ => throw new RuntimeException("wat")
//            };
//        (temputs0, List())
//      }
      case CaptureP1(name, mutable, None) => {
        val inputPointerType = inputLookupExpr.resultRegister.reference
        val let = Let2(name, mutable, inputLookupExpr);
        (temputs0, seqNum0, inputPointerType, List(let))
      }
      case CaptureP1(name, mutable, Some(inner1)) => {
        val (temputs1, seqNum1, expectedPointerType, innerLets) =
            innerNonCheckingTranslate(
              env, temputs0, patternId, seqNum0, false, inner1, inputLookupExpr)
        println("making let a")
        val convertedInputLookupExpr =
            TypeTemplar.convert(env, temputs1, inputLookupExpr, expectedPointerType);
        val newLet = Let2(name, mutable, convertedInputLookupExpr)
        (temputs1, seqNum1, expectedPointerType, newLet :: innerLets)
      }
      case p @ PackP1(innerPatterns) => {
        nonCheckingTranslatePack(
          env, temputs0, patternId, seqNum0, false, p, inputLookupExpr)
      }
      case _ => {
        throw new RuntimeException("not yet " + pattern.toString)
      }
    }
  }

  private def nonCheckingTranslatePack(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternId: Int,
      seqNum0: Int,
      borrow: Boolean, // whether we're inside a & or not
      pattern: PackP1,
      inputLookupPackExpr: ReferenceExpression2):
      (Temputs, Int, Reference2, List[Let2]) = {
    val PackP1(innerPatterns) = pattern;

    inputLookupPackExpr.resultRegister match {
      // if ownership is a borrow that would be weird. i dont think we can borrow packs.
      // so, ownership can be own or share.
      case ReferenceRegister2(Reference2(Share, packType @ PackT2(memberTypes, underlyingStruct))) => {
        val underlyingStructDef = temputs0.lookupStruct(underlyingStruct)
        assert(underlyingStructDef.mutability == Immutable)

        // Still, even though the pack struct is immutable, it could contain borrows.
        // Just not any owns.

        val (temputs4, seqNum4, innerTypes2, lets) =
          innerPatterns.zip(memberTypes).zipWithIndex
            .foldLeft(temputs0, seqNum0, List[Reference2](), List[Let2]())({
              case ((temputs1, seqNum1, previousTypes, previousLets), ((innerPattern, innerType), index)) => {
                val letName = "__pattern_" + patternId + "_" + seqNum1;
                val seqNum2 = seqNum1 + 1;
                innerType.ownership match {
                  case Share => {
                    val member = underlyingStructDef.members(index);
                    val lookupExpr = SoftLoad2(ReferenceMemberLookup2(member.name, inputLookupPackExpr, member.tyype.expectReferenceMember().reference), Share);
                    val let = Let2(letName, false, lookupExpr)
                    val lookup = SoftLoad2(LocalLookup2(letName, innerType), Share);
                    val (temputs3, seqNum3, expectedType, innerLets) =
                      innerNonCheckingTranslate(env, temputs1, patternId, seqNum2, false, innerPattern, lookup)
                    (temputs3, seqNum3, previousTypes :+ expectedType, previousLets ++ (let :: innerLets))
                  }
                  case _ => throw new RuntimeException("wat")
                }
              }
            })
        (temputs4, seqNum4, Reference2(Share, packType), lets)
      }
      case ReferenceRegister2(Reference2(Own, packType @ PackT2(memberTypes, underlyingStruct))) => {
        assert(memberTypes.size == innerPatterns.size)

        val inputLookupStructExpr =
          TemplarReinterpret2(
            inputLookupPackExpr,
            Reference2(Borrow, underlyingStruct))

        val structDef = temputs0.lookupStruct(underlyingStruct);

        val (temputs4, seqNum4, innerTypes2, lets) =
          innerPatterns.zip(memberTypes).zipWithIndex
            .foldLeft(temputs0, seqNum0, List[Reference2](), List[Let2]())({
              case ((temputs1, seqNum1, previousTypes, previousLets), ((innerPattern, innerType), index)) => {
                val letName = "__pattern_" + patternId + "_" + seqNum1;
                val seqNum2 = seqNum1 + 1;
                throw new RuntimeException("hardcoding a borrow! this is bad!")
                val member = structDef.members(index);
                val undecayedLookupExpr =
                  ReferenceMemberLookup2(
                    member.name, inputLookupStructExpr, member.tyype.reference);
                val decayedLookupdExpr = SoftLoad2(undecayedLookupExpr, Borrow);
                val let = Let2(letName, false, decayedLookupdExpr)
                val lookup = SoftLoad2(LocalLookup2(letName, innerType), Borrow);
                val (temputs3, seqNum3, expectedType, innerLets) =
                  innerNonCheckingTranslate(
                    env, temputs1, patternId, seqNum2, false, innerPattern, lookup)
                (temputs3, seqNum3, previousTypes :+ expectedType, previousLets ++ (let :: innerLets))
              }
            });
        (temputs4, seqNum4, Reference2(Own, packType), lets)
      }
    }
  }

  // Assumes the templated stuff has already been put into this environment
  def getParameterType2s(env: LocalEnvironment, temputs0: Temputs, params1: List[Parameter1]):
  (Temputs, List[Reference2]) = {
    val types1 = getParameterType1s(params1)
    val (temputs2, types2) =
      TypeTemplar.evaluateTypes(env, temputs0, types1)
    (temputs2, TemplataTemplar.coerceTemplatasToReferences(temputs2, types2, Own))
  }

  def getParameterType1s(params1: List[Parameter1]): List[Type1] = {
    getPatternType1s(params1.map(_.capturePattern))
  }

  def getPatternType1s(patterns1: List[Pattern1]):
  List[Type1] = {
    patterns1.map(pattern1 => getPatternType1(pattern1))
  }

  // Once we know that a function isnt templated, we use this to figure out
  // the types of its parameters
  private def getPatternType1(pattern: Pattern1):
      Type1 = {
    pattern match {
      case TypeOfP1(type1) => type1
//      case TypeOfP1(TypeName1(name)) => {
//        val tyype = env.lookupType(name);
//        val pointerType =
//          tyype match {
//            case TemplataReference(tyype) => {
//              tyype match {
//                case p @ Reference2(Own, _) => p
//                case p @ Reference2(Borrow, _) => p
//                case p @ Reference2(Raw, _) => p
//                case p @ Reference2(Share, _) => p
//              }
//            }
//            case TemplataReferend(concrete) => {
//              concrete match {
//                case sr2 @ StructRef2(_, _) => {
//                  val structDef2 = temputs0.lookupCitizen(sr2)
//                  val ownership = if (structDef2.mutability == Mutable) Own else Share
//                  Reference2(ownership, concrete)
//                }
//                case sr2 @ InterfaceRef2(_, _) => {
//                  val structDef2 = temputs0.lookupCitizen(sr2)
//                  val ownership = if (structDef2.mutability == Mutable) Own else Share
//                  Reference2(ownership, concrete)
//                }
//              }
//            }
//            case TemplataStructTemplate(struct) => throw new RuntimeException("cant match against a template type") // but someday, perhaps? would it be useful?
//          }
//        (temputs0, pointerType)
//      }
      case CaptureP1(name, mutable, Some(inner1)) => {
        getPatternType1(inner1)
      }
      case PackP1(elements0) => {
        PackT1(getPatternType1s(elements0))
      }
      case CaptureP1(name, mutable, None) => {
        // we should already know that this isn't templated
        throw new RuntimeException("wat")
      }
      case _ => throw new RuntimeException("wat " + pattern)
    }
  }

  private def innerNonCheckingTranslateList(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternId: Int,
      seqNum0: Int,
      params: List[Pattern1],
      inputLookupExprs: List[SoftLoad2]):
  (Temputs, Int, List[Let2]) = {
    innerNonCheckingTranslateList(
        env, temputs0, patternId, seqNum0, params.zip(inputLookupExprs))
  }

  // returns the new env, the top level  the bool expression, and a list of lets
  private def innerNonCheckingTranslateList(
      env: LocalEnvironment,
      temputs0: Temputs,
      patternId: Int,
      seqNum0: Int,
      paramsAndInputLookupExprs: List[(Pattern1, SoftLoad2)]):
  (Temputs, Int, List[Let2]) = {
     paramsAndInputLookupExprs match {
      case Nil => (temputs0, seqNum0, List())
      case (firstParam, firstInputLookupExpr) :: rest => {
        val (temputs1, seqNum1, paramType2, firstLets) =
            innerNonCheckingTranslate(
              env, temputs0, patternId, seqNum0, false, firstParam, firstInputLookupExpr);
        val (temputs2, seqNum2, restLets) =
            innerNonCheckingTranslateList(env, temputs1, patternId, seqNum1, rest);
        (temputs2, seqNum2, firstLets ++ restLets)
      }
    } 
  }
  
  
  private def getPatternCaptureNames(patterns: List[Pattern1]):
      Set[String] = {
    patterns.map(getPatternCaptureNames).reduce(_ ++ _)
  }
  
  // returns seq num, captured names, and new pattern
  def getPatternCaptureNames(pattern: Pattern1): Set[String] = {
    pattern match {
      case DiscardP1() => Set()
      case CaptureP1(name, mutable, None) => Set(name)
      case CaptureP1(name, mutable, Some(inner0)) => {
        getPatternCaptureNames(inner0) + name
      }
      case TypeOfP1(name) => Set()
      case TupleP1(elements0) => getPatternCaptureNames(elements0)
      case PackP1(elements0) => getPatternCaptureNames(elements0)
      case DestructureP1(name, members) => getPatternCaptureNames(members)
      case PackRestP1(inner0) => getPatternCaptureNames(inner0)
//      case TemplateSpecifiedTypeOfP1(name, templateArgs) => Set()
//      case TemplatePackRest1(templateParamName) => (List(), TemplatePackRest1(templateParamName))
//      case TemplateTypeOf1(templateParamName) => (List(), TemplateTypeOf1(templateParamName))
    }
  }
  
}
