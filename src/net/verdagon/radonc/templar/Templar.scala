package net.verdagon.radonc.templar;

import net.verdagon.radonc._
import net.verdagon.radonc.scout._

import scala.collection.immutable.{List, ListMap}

object Templar {
  def evaluate(
      program: Program1,
      envTypeMembers: Map[String, ITemplata],
      envExternFunctionMembers: Map[String, List[FunctionHeader2]],
      envValues: Map[String, IValueTemplata]):
  CompleteProgram2 = {
    val Program1(typeDefs1, impls1, functions1) =
      program;

    val env0 = GlobalEnvironment(Map(), Map(), envTypeMembers, envValues, Map())

    val env2 = impls1.foldLeft(env0)({
      case (env1, impl1) => env1.addImpl(impl1)
    })
    val temputs0 =
      Temputs(Set(), Map(), Map(), List(), Map(), Set(), ListMap(), Set(), ListMap(), Map())

    val temputs1 = StructTemplar.addBuiltInStructs(env2, temputs0)

    val (env8, temputs8) = FunctionTemplar.scanExternFunctions(env2, temputs1, envExternFunctionMembers)

    val (env9, temputs9) = StructTemplar.scanTypeDefinitions(env8, temputs8, typeDefs1);
    val (env10, temputs10) = StructTemplar.evaluateTypeDefinitions(env9, temputs9, typeDefs1);

    val interfaces1 = typeDefs1.collect({ case i : Interface1 => i });
    val (env11, temputs11) =
      FunctionTemplar.scanOrdinaryInterfacesMembers(env10, temputs10, interfaces1);

    val FunctionCategories(abstractFunctions1, implementedFunctions1) = categorizeFunctions(functions1);

    val (env12, temputs12) = FunctionTemplar.scanAbstractFunctions(env11, temputs11, abstractFunctions1);

    val (env13, temputs13) = FunctionTemplar.scanImplementedFunctions(env12, temputs12, implementedFunctions1);

    val temputs14 = evaluate(env13.spawnLocalEnv(), temputs13, implementedFunctions1);

    val result = CompleteProgram2(
      temputs14.getAllInterfaces().toList,
      temputs14.getAllStructs().toList,
      temputs14.getAllFunctions(),
      temputs14.functionFamiliesByRootBanner)

    checkFamiliesCompleteness(result)

    result
  }

  case class FunctionCategories(
      abstractFunctions1: List[NTVFunction1],
      implementedFunctions1: List[Function1])

  private def categorizeFunctions(functions1: List[Function1]):
  FunctionCategories = {
    functions1.foldLeft(FunctionCategories(List(), List()))({
      case (FunctionCategories(abstractFunctions1, implementedFunctions1), function1) => {
        if (function1.isAbstract) {
          FunctionCategories(
            NTVFunction1(function1, function1.name, function1.templateParams, Simplify.simplifyParams(function1.params)) :: abstractFunctions1,
            implementedFunctions1)
        } else {
          FunctionCategories(
            abstractFunctions1,
            function1 :: implementedFunctions1)
        }
      }
    })
  }

  private def checkFamiliesCompleteness(program: CompleteProgram2) = {
    program.functionFamiliesByRootBanner.foreach({
      case (rootBanner, family) => {
        checkFamilyCompleteness(program, rootBanner, family.memberSignaturesByVirtualRoots.values.toSet)
      }
    })
  }

  private def checkFamilyCompleteness(program: CompleteProgram2, banner: FunctionBanner2, signatures: Set[Signature2]) = {
    val overrideParamCombinations =
      VirtualTemplar.getAllOverrideParamCombinations(program, banner).values
    overrideParamCombinations.foreach(neededParams => {
      val exists =
        signatures.map(_.paramTypes).exists(signatureParams => {
          neededParams.zip(signatureParams).forall({
            case (neededParam, signatureParam) => TypeTemplar.isTypeConvertible(program, neededParam, signatureParam)
          })
        })
      if (!exists) {
        throw new RuntimeException("Can't find a family member for calling: " + banner.humanName + "(" + neededParams.map(":" + _) + ")")
      }
    })

  }

  private def evaluate(env: LocalEnvironment, temputs0: Temputs, functions: List[Function1]): Temputs = {
    functions match {
      case Nil => temputs0
      case firstFunction1 :: restFunctions1 => {
        val temputs1 =
          if (firstFunction1.templateParams.isEmpty) {
            FunctionTemplar.evaluateOrdinaryLightFunctionFromNonCallForTemputs(
              env, temputs0, firstFunction1)
          } else {
            temputs0
          }
        evaluate(env, temputs1, restFunctions1)
      }
    }
  }

//  def maybeSoftDereference(expr2: Expression2): Expression2 = {
//    expr2.resultRegister match {
//      case Addressible2(pointerType) => {
//        pointerType match {
//          case Reference2(ownership, innerType) => SoftLoad2(expr2, ownership)
//        }
//      }
//      case Reference2(Own, _) => expr2
//      case Reference2(Borrow, _) => expr2
//      case Reference2(Raw, _) => expr2
//      case Reference2(Share, _) => expr2
//      case _ => throw new RuntimeException("wat " + expr2.resultRegister) // curiosity should this ever happen
//    }
//  }
//
//  def maybeMoveSoftDereference(expr2: Expression2): Expression2 = {
//    expr2.resultRegister match {
//      case Addressible2(pointerType) => {
//        pointerType match {
//          // ownership doesnt matter because owning pointers arent counted, so moving a none is easy
//          case Reference2(Own, innerType) => SoftLoad2(expr2, Own)
//          case Reference2(Borrow, innerType) => throw new RuntimeException("wat") // curiosity should this ever happen
//        }
//      }
//      // ownership doesnt matter because owning pointers arent counted, so moving a none is easy
//      // besides, this is already a Reference2 so we want a no-op
//      case Reference2(Own, _) => expr2
//      case Reference2(Share, _) => throw new RuntimeException("Tried to move a shared reference")
//      case _ => throw new RuntimeException("wat " + expr2.resultRegister) // curiosity should this ever happen
//    }
//  }
//
//  def maybeLend(expr2: Expression2): Expression2 = {
//    expr2.resultRegister match {
//      case Reference2(Own, innerType) => Alias2(expr2, Borrow)
//      case Addressible2(innerPointer) => SoftLoad2(expr2, Borrow)
//    }
//  }

  def getMutabilities(temputs0: Temputs, concreteValues2: List[Referend2]):
  List[Mutability] = {
    concreteValues2.map(concreteValue2 => getMutability(temputs0, concreteValue2))
  }

  def getMutability(temputs0: Temputs, concreteValue2: Referend2):
  Mutability = {
    concreteValue2 match {
      case Int2() => Immutable
      case Bool2() => Immutable
      case Str2() => Immutable
      case FunctionT2(_, _) => Immutable
      case ArrayT2(_, mutability) => mutability
      case ArraySequenceT2(_, ArrayT2(_, mutability)) => mutability
      case sr @ StructRef2(_, _) => temputs0.lookupMutability(sr)
      case ir @ InterfaceRef2(_, _) => temputs0.lookupMutability(ir)
      case TemplatedClosure2(structRef, terry) => temputs0.lookupMutability(structRef)
      case GlobalFunctionGroup2(understructRef, name, alreadySpecifiedTemplateArgs) => {
        // Just like FunctionT2
        Immutable
      }
    }
  }

  def newGlobalFunctionGroupExpression(
      name: String,
      alreadySpecifiedTemplateArgs: List[ITemplata]) = {
    TemplarReinterpret2(
      PackTemplar.newPackExpression,
      Reference2(
        Share,
        GlobalFunctionGroup2(
          StructTemplar.emptyPackStructRef,
          name,
          alreadySpecifiedTemplateArgs)))
  }
}
