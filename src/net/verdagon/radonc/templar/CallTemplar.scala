package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.Mutable

import scala.collection.immutable.List

object CallTemplar {

  private def makeClosureArgument(
      temputs0: Temputs,
      closureStructRef: StructRef2,
      givenCallableUnborrowedExpr2: ReferenceExpression2,
      givenArgsExprs2: List[ReferenceExpression2]) = {

    // Whether we're given a borrow or an own, the call itself will be given a borrow.
    val givenCallableBorrowExpr2 =
      givenCallableUnborrowedExpr2.resultRegister.reference match {
        case Reference2(Own, _) => Alias2(givenCallableUnborrowedExpr2, Borrow)
        case Reference2(Borrow, _) => givenCallableUnborrowedExpr2
        case Reference2(Share, _) => givenCallableUnborrowedExpr2
        case Reference2(Raw, _) => givenCallableUnborrowedExpr2
      }

    val ownership = if (temputs0.lookupMutability(closureStructRef) == Mutable) Borrow else Share
    TemplarReinterpret2(givenCallableBorrowExpr2, Reference2(ownership, closureStructRef))
  }

  private def evaluateCall(
      env: LocalEnvironment,
      temputs0: Temputs,
      callableExpr: ReferenceExpression2,
      givenArgsExprs2: List[ReferenceExpression2]):
      (Temputs, FunctionPointerCall2) = {
    callableExpr.resultRegister.reference.referend match {
      case OrdinaryClosure2(structRef, prototype) => {
        evaluateClosureCall(
          env, temputs0, structRef, prototype, callableExpr, givenArgsExprs2)
      }
      case tc2 @ TemplatedClosure2(understructRef2, terry) => {
        val argsTypes2 = givenArgsExprs2.map(_.resultRegister.reference)
        val (temputs2, header) =
          FunctionTemplar.evaluateTemplatedClosureFunctionFromCallForPrototype(
            env, temputs0, tc2, argsTypes2)
        evaluateClosureCall(
          env,
          temputs2,
          understructRef2,
          header.toPrototype,
          callableExpr,
          givenArgsExprs2)
      }
      case GlobalFunctionGroup2(understruct, functionName, alreadySpecifiedTemplateArgs) => {
        val unconvertedArgsPointerTypes2 =
          givenArgsExprs2.map(_.resultRegister.expectReference().reference)
        val (temputs2, prototype) =
          OverloadTemplar.scoutFunctionForPrototype(
            env,
            temputs0,
            functionName,
            alreadySpecifiedTemplateArgs,
            unconvertedArgsPointerTypes2,
            exact = false);
        val argsExprs2 =
          TypeTemplar.convertExprs(
            env, temputs2, givenArgsExprs2, prototype.functionType.paramTypes)
        evaluateNonClosureCallInnerForPossibleVirtuals(
          env, temputs2, prototype.toPrototype, argsExprs2)
      }
      case ft @ FunctionT2(_, _) => {
        val argsExprs2 =
          TypeTemplar.convertExprs(
            env, temputs0, givenArgsExprs2, ft.paramTypes)
        evaluateNonClosureCallInnerForNonVirtuals(
          temputs0, callableExpr, argsExprs2)
      }
    }
  }


  // given args means, the args that the user gave, like in
  // let a = 6;
  // let f = {[a](x) print(6, x) };
  // f(4);
  // in the f(4), the given args is just 4.
  //
  // however, since f is actually a struct, it's secretly this:
  // let a = 6;
  // let f = {[a](x) print(6, x) };
  // f.__function(f.__closure, 4);
  // in that f.__function(f.__closure, 4), the given args is just 4, but the actual args is f.__closure and 4.
  // also, the given callable is f, but the actual callable is f.__function.

  private def evaluateClosureCall(
      env: LocalEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      prototype2: Prototype2,
      givenCallableUnborrowedExpr2: ReferenceExpression2,
      givenArgsExprs2: List[ReferenceExpression2]):
      (Temputs, FunctionPointerCall2) = {

    // Whether we're given a borrow or an own, the call itself will be given a borrow.
    val givenCallableBorrowExpr2 =
      givenCallableUnborrowedExpr2.resultRegister.reference match {
        case Reference2(Own, _) => Alias2(givenCallableUnborrowedExpr2, Borrow)
        case Reference2(Borrow, _) => givenCallableUnborrowedExpr2
        case Reference2(Share, _) => givenCallableUnborrowedExpr2
        case Reference2(Raw, _) => givenCallableUnborrowedExpr2
      }


    val ownership = if (temputs0.lookupMutability(closureStructRef) == Mutable) Borrow else Share
    val actualCallableExpr2 =
      TemplarReinterpret2(givenCallableBorrowExpr2, Reference2(ownership, closureStructRef))

    val actualArgsExprs2 = actualCallableExpr2 :: givenArgsExprs2

    val argTypes = actualArgsExprs2.map(_.resultRegister.reference)
    assert(argTypes == prototype2.functionType.paramTypes, "arg param type mismatch. params: " + prototype2.functionType.paramTypes + " args: " + argTypes)

    CallTemplar.checkTypes(temputs0, prototype2.functionType.paramTypes, argTypes, exact = true)

    val resultingExpr2 = FunctionPointerCall2(FunctionLookup2(prototype2), actualArgsExprs2);

    (temputs0, resultingExpr2)
  }


  def evaluateNonClosureCallInnerForNonVirtuals(
      temputs0: Temputs,
      callableExpr2: ReferenceExpression2,
      argsExprs2: List[ReferenceExpression2]):
  (Temputs, FunctionPointerCall2) = {
    val argsPointerTypes2 = argsExprs2.map(_.resultRegister.expectReference().reference)

    val callableType = callableExpr2.resultRegister.reference.referend.asInstanceOf[FunctionT2]

    checkTypes(temputs0, callableType.paramTypes, argsPointerTypes2, exact = true);

    (temputs0, FunctionPointerCall2(callableExpr2, argsExprs2))
  }

  def evaluateNonClosureCallInnerForPossibleVirtuals(
      env: LocalEnvironment,
      temputs0: Temputs,
      prototype: Prototype2,
      argsExprs2: List[ReferenceExpression2]):
  (Temputs, FunctionPointerCall2) = {
    val header = temputs0.lookupFunction(prototype.toSignature).get.header;

    CallTemplar.checkTypes(
      temputs0,
      header.toBanner.paramTypes,
      argsExprs2.map(a => a.resultRegister.reference),
      exact = true)

    if (header.params.exists(_.virtuality.nonEmpty)) {
      val temputs1 =
        VirtualTemplar.ensureFamiliesExistsAndStampRelatives(env, temputs0, header);
      assert(temputs1.lookupFunction(header.toSignature).nonEmpty);
      (temputs1, FunctionPointerCall2(FunctionLookup2(prototype), argsExprs2))
    } else {
      evaluateNonClosureCallInnerForNonVirtuals(temputs0, FunctionLookup2(prototype), argsExprs2)
    }
  }

  def checkTypes(temputs: Temputs, params: List[Reference2], args: List[Reference2], exact: Boolean): Unit = {
    assert(params.size == args.size)
    (params, args) match {
      case (Nil, Nil) => {}
      case (paramsHead :: paramsTail, argsHead :: argsTail) => {
        if (paramsHead == argsHead) {
          // Do nothing, we're done
        } else {
          if (!exact && TypeTemplar.isTypeConvertible(temputs, argsHead, paramsHead)) {
            // Do nothing, we're done
          } else {
//          do stuff here.
//          also there is one special case here, which is when we try to hand in
//          an owning when they just want a borrow, gotta account for that here
            throw new RuntimeException("do stuff " + argsHead + " and " + paramsHead)
          }
        }
        checkTypes(temputs, paramsTail, argsTail, exact)
      }
      case _ => throw new RuntimeException("wat")
    }
//    checkTypes(params.tail, args.tail)
//    assert(argTypes == callableType.paramTypes, "arg param type mismatch. params: " + callableType.paramTypes + " args: " + argTypes)
  }

  def evaluatePrefixCall(
      env: LocalEnvironment,
      temputs0: Temputs,
      callableExpr2: Expression2,
      argsExprs2: Expression2):
  (Temputs, FunctionPointerCall2) = {
    val flattenedArgsExprs =
      ExpressionUtils.flattenPack(
        ExpressionTemplar.coerceToReferenceExpression(argsExprs2));
    evaluateCall(
      env,
      temputs0,
      ExpressionTemplar.coerceToReferenceExpression(callableExpr2),
      flattenedArgsExprs)
  }

  def evaluateInfixCall(
      env: LocalEnvironment,
      temputs0: Temputs,
      callableExpr: ReferenceExpression2,
      leftArgExpr: ReferenceExpression2,
      rightArgExpr: ReferenceExpression2):
  (Temputs, ReferenceExpression2) = {
    val flattenedLeftArgExprs = ExpressionUtils.flattenPack(leftArgExpr);
    if (flattenedLeftArgExprs.size != 1) {
      throw new RuntimeException("Thing on left is zero or >1");
    }
    val flattenedRightArgExprs = ExpressionUtils.flattenPack(rightArgExpr)
    if (flattenedRightArgExprs.size != 1) {
      throw new RuntimeException("Thing on right is zero or >1");
    }
    val argExprs = flattenedLeftArgExprs ++ flattenedRightArgExprs;
    evaluateCall(env, temputs0, callableExpr, argExprs)
  }

//  private def stampForFunctionRef(env0: LocalEnvironment, temputs0: Temputs, function1: Function1, argTypes: List[Reference2], alreadySpecifiedTemplateArgs: List[Value2]):
//  (Temputs, FunctionHeader2) = {
//    val templateArgs = FunctionTemplar.evaluateFunctionTemplateArgs(function1, argTypes, alreadySpecifiedTemplateArgs)
//    val (temputs1, params2) = FunctionTemplar.evaluateFunctionParams(env0, temputs0, function1, templateArgs);
//
//    temputs1.findFunction(function1.name, params2.map(_.tyype)) match {
//      case Some(header) => (temputs1, header)
//      case None => {
//        FunctionTemplar.evaluateReadyFunctionForRef(
//          env0, temputs1, function1, templateArgs, params2, (temputs, header) => temputs);
//      }
//    }
//  }
}