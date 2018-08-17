package net.verdagon.radonc.templar

import net.verdagon.radonc.scout
import net.verdagon.radonc.scout._

import scala.collection.immutable.List

object InnerFunctionTemplar {


  // Preconditions:
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  // - all params were added to the env.
  def innerEvaluateFunctionForPrototype(
      env0: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1,
      templateArgs: List[CoercedTemplateArg2],
      params2: List[Parameter2]):
  (Temputs, FunctionHeader2) = {
    // Check preconditions
    function1.templateParams.map(_.name).foreach(templateParamName => {
      assert(env0.typeMembers.contains(templateParamName));
    })
    function1.closuredNames.foreach(closuredName => {
      assert(env0.variables.contains(closuredName));
    })
    Simplify.simplifyParams(function1.params).foreach(param1 => {
      assert(env0.variables.contains(param1.name));
    });

    function1.body match {
      case None => {
        val (temputs1, returnType2) =
          function1.ret match {
            case None => throw new RuntimeException("wat");
            case Some(returnType1) => {
              TypeTemplar.evaluateAndReferencifyType(env0, temputs0, returnType1, Own)
            }
          };

        if (function1.isExtern) {
          val header = FunctionHeader2(function1.name, false, true, templateArgs, params2, returnType2, Some(function1))
          val temputs2 =
            temputs1
                .declareFunctionSignature(header.toSignature)
                .declareFunctionReturnType(header.toSignature, header.returnType)
                .addFunction(makeExternFunction(header))
          (temputs2, header)
        } else if (function1.isAbstract) {
          val header = FunctionHeader2(function1.name, true, false, templateArgs, params2, returnType2, Some(function1))
          val temputs2 =
            temputs1
                .declareFunctionSignature(header.toSignature)
                .declareFunctionReturnType(header.toSignature, header.returnType)
                .addFunction(makeInterfaceFunction(header))
          (temputs2, header)
        } else {
          throw new RuntimeException("wat")
        }
      }
      case Some(body) => {
        val (temputs2, header, body2) =
          innerDeclareAndEvaluateFunctionBody(
            env0, temputs0, BFunction1(function1, function1.name, body), templateArgs, params2)

        // Funny story... let's say we're current instantiating a constructor,
        // for example MySome:T().
        // The constructor returns a MySome:T, which means when we do the above
        // evaluating of the function body, we stamp the MySome:T struct.
        // That ends up stamping the entire struct, including the constructor.
        // That's what we were originally here for, and evaluating the body above
        // just did it for us O_o
        // So, here we check to see if we accidentally already did it.

        temputs2.lookupFunction(header.toSignature) match {
          case None => {
            val function2 = Function2(header, body2);
            val temputs3 = temputs2.addFunction(function2)
            (temputs3, function2.header)
          }
          case Some(function2) => {
            (temputs2, function2.header)
          }
        }
      }
    }
  }

  private def innerDeclareAndEvaluateFunctionBody(
      env: LocalEnvironment,
      temputs0: Temputs,
      bfunction1: BFunction1,
      templateArgs: List[CoercedTemplateArg2],
      params2: List[Parameter2]):
  (Temputs, FunctionHeader2, ReferenceExpression2) = {
    val BFunction1(function1, name, _) = bfunction1;
    function1.ret match {
      case None => {
        val banner = FunctionBanner2(Some(function1), name, templateArgs, params2)
        val temputs1 = temputs0.declareFunctionSignature(banner.toSignature)
        val (temputs2, body2, returnType2) =
          innerEvaluateFunctionBody(env, temputs1, bfunction1, params2);
        val temputs3 = temputs2.declareFunctionReturnType(banner.toSignature, returnType2)
        val header = FunctionHeader2(name, false, false, templateArgs, params2, returnType2, Some(function1));

        val temputs4 = VirtualTemplar.ensureFamiliesExistsAndStampRelatives(env, temputs3, header)
        (temputs4, header, body2)
      }
      case Some(returnType1) => {
        val (temputs3, returnType2) =
          TypeTemplar.evaluateAndReferencifyType(env, temputs0, returnType1, Own);
        val header = FunctionHeader2(name, false, false, templateArgs, params2, returnType2, Some(function1));
        val temputs4 = temputs3.declareFunctionSignature(header.toSignature)
        val temputs5 = VirtualTemplar.ensureFamiliesExistsAndStampRelatives(env, temputs4, header)
        val (temputs6, body2, _) =
          innerEvaluateFunctionBody(env, temputs5, bfunction1, params2);
        val temputs7 = temputs6.declareFunctionReturnType(header.toSignature, returnType2)
        (temputs7, header, body2)
      }
    }
  }

  private def innerEvaluateFunctionBody(
      env0: LocalEnvironment,
      temputs0: Temputs,
      bfunction1: BFunction1,
      params2: List[Parameter2]):
  (Temputs, ReferenceExpression2, Reference2) = {
    val BFunction1(function1, name, bodyWithoutLets1) = bfunction1;

    val (temputs1, maybeReturnType2) =
      TypeTemplar.evaluateAndReferencifyMaybeType(env0, temputs0, function1.ret, Own)

    val (temputs2, letExprs2) =
      evaluateLets(env0, temputs1, function1.params, params2);
    val env1 =
      env0.addVariables(letExprs2.map(let2 => let2.name -> LocalVariable2(let2.resultRegister.reference)).toMap)

    val (temputs3, bodyWithoutLets2, unusedBodyExportedTemplatas) =
      ExpressionTemplar.evaluateBlock(env1, temputs2, bodyWithoutLets1);

    val bodyWithLets2 =
      if (letExprs2.isEmpty)
        bodyWithoutLets2
      else
        Block2(letExprs2 ++ bodyWithoutLets2.elements)
    val resultPointer = bodyWithLets2.resultRegister.reference

    maybeReturnType2 match {
      case None => {}
      case Some(expectedReturnType2) => {
        if (!TypeTemplar.isTypeConvertible(temputs3, bodyWithLets2.resultRegister.reference, expectedReturnType2)) {
          throw new RuntimeException("Return type " + expectedReturnType2 + " doesn't match body's result " + resultPointer)
        }
      }
    };
    (temputs3, bodyWithLets2, resultPointer)
  }

//  private def decayBody(undecayedBody2: Block2): (Block2, Reference2) = {
//    undecayedBody2.resultType match {
//      case Addressible2(Reference2(Owning, concreteType)) => {
//        // this would happen if the last statement was just "x"
//
//        val Block2(exprs2) = undecayedBody2;
//        val everythingBeforeTail = exprs2.slice(0, exprs2.size - 1)
//        val oldTail = exprs2.last
//        val newTail = MoveSoftLoad2(oldTail);
//        val newExprs2 = everythingBeforeTail :+ newTail
//        (Block2(newExprs2), newTail.resultType)
//      }
//      case o @ Reference2(Owning, _) => (undecayedBody2, o)
//      case o @ Reference2(Borrow, _) => (undecayedBody2, o)
//      case o @ Reference2(Raw, _) => (undecayedBody2, o)
//      case _ => throw new RuntimeException("wat " + undecayedBody2.resultType)
//    }
//  }

  private def evaluateLets(env: LocalEnvironment, temputs0: Temputs, params1: List[Parameter1], params2: List[Parameter2]):
  (Temputs, List[Let2]) = {
    val paramInputExprs2 =
      params2.map(p => {
        val lookup = LocalLookup2(p.name, p.tyype)
        SoftLoad2(lookup, lookup.reference.ownership)
      })
    val (temputs1, letExprs2) =
      PatternTemplar.nonCheckingTranslateList(env, temputs0, params1.map(_.patternId), params1.map(_.capturePattern), paramInputExprs2);

    // todo: at this point, to allow for recursive calls, add a callable type to the environment
    // for everything inside the body to use

    (temputs1, letExprs2)
  }

  def evaluateClosureStruct(
      outerEnv: LocalEnvironment, temputs0: Temputs, function1: scout.Function1):
  (Temputs, StructRef2) = {
    val closuredNames = function1.closuredNames;

    // Note, this is where the unordered closuredNames set becomes ordered.
    val closuredVarNamesAndTypes: List[(String, Reference2)] =
      closuredNames
          .map(closuredName => (closuredName, outerEnv.variables(closuredName).reference))
          .toList;

    val (temputs1, structRef, _) =
      StructTemplar.makeClosureUnderstruct(outerEnv, temputs0, function1.name, closuredVarNamesAndTypes)
    (temputs1, structRef)
  }

  def makeExternFunction(header: FunctionHeader2): Function2 = {
    Function2(
      header,
      ExternFunctionCall2(
        header.toPrototype,
        header.params.map({
          case Parameter2(name, _, p @ Reference2(ownership, _)) => {
            SoftLoad2(LocalLookup2(name, p), ownership)
          }
        })))
  }

  def makeInterfaceFunction(header: FunctionHeader2): Function2 = {
    Function2(
      header,
      InterfaceFunctionCall2(
        header.toBanner,
        Reference2(Raw, header.toPrototype.functionType),
        header.returnType,
        header.params.map({
          case Parameter2(name, _, p @ Reference2(ownership, _)) => {
            SoftLoad2(LocalLookup2(name, p), ownership)
          }
        })))
  }
}
