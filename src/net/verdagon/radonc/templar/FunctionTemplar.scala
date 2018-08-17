package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc.scout._

import scala.collection.immutable.List


// closure concerns first.
// then templated stuff.
// then the underlying function

object FunctionTemplar {

  def scanOrdinaryInterfacesMembers(env0: GlobalEnvironment, temputs0: Temputs, interfaces1: List[Interface1]):
  (GlobalEnvironment, Temputs) = {
    interfaces1 match {
      case Nil => (env0, temputs0)
      case headInterface1 :: tailInterfaces1 => {
        val (env1, temputs1) = scanOrdinaryInterfaceMembers(env0, temputs0, List(), headInterface1.members)
        val (env2, temputs2) = scanOrdinaryInterfacesMembers(env1, temputs1, tailInterfaces1)
        (env2, temputs2)
      }
    }
  }

  private def scanOrdinaryInterfaceMembers(
      env0: GlobalEnvironment,
      temputs0: Temputs,
      interfaceExplicitTemplateArgs: List[ITemplata],
      members: List[NTVFunction1]):
  (GlobalEnvironment, Temputs) = {
    members match {
      case Nil => (env0, temputs0)
      case head1 :: tail1 => {
        val (env1, temputs1) = scanOrdinaryInterfaceMember(env0, temputs0, interfaceExplicitTemplateArgs, head1);
        val (env2, temputs2) = scanOrdinaryInterfaceMembers(env1, temputs1, interfaceExplicitTemplateArgs, tail1);
        (env2, temputs2)
      }
    }
  }

  private def scanOrdinaryInterfaceMember(
      env0: GlobalEnvironment,
      temputs0: Temputs,
      interfaceExplicitTemplateArgs: List[ITemplata],
      prototype1: NTVFunction1):
  (GlobalEnvironment, Temputs) = {
    val types1 = prototype1.params.map(_.tyype)

    val (temputs1, paramReferenceTypes2) =
      TypeTemplar.evaluateAndReferencifyTypes(env0, temputs0, types1, Own)

    val params2 =
      prototype1.params.zip(paramReferenceTypes2).map({
        case (param1, paramType2) => Parameter2(param1.name, param1.virtuality, paramType2)
      });

    // If there's no return type, assume Any.
    val (temputs3, returnReferenceType2) =
      prototype1.origin.ret match {
        case None => {
          (temputs1, Reference2(Share, Any2()))
        }
        case Some(returnType1) => {
          TypeTemplar.evaluateAndReferencifyType(env0, temputs1, returnType1, Own);
        }
      };

    if (prototype1.templateParams.isEmpty) {
      val header =
        FunctionHeader2(
          prototype1.name,
          isAbstract = true,
          isExtern = false,
          List(),
          params2,
          returnReferenceType2,
          Some(prototype1.origin))

      val env1 = env0.addFunctionBanner(header.humanName, header.toBanner)

      val function2 = InnerFunctionTemplar.makeInterfaceFunction(header);
      val temputs4 =
        temputs3
            .declareFunctionSignature(header.toSignature)
            .declareFunctionReturnType(header.toSignature, returnReferenceType2)
            .addFunction(function2)

      assert(temputs4.exactDeclaredSignatureExists(header.humanName, header.templateArgs, header.paramTypes))

      (env1, temputs4)
    } else {
      FunctionTemplar.scanAbstractFunction(env0, temputs3, prototype1)
    }
  }

  // innerEvaluate means that all of the args and template args have been figured out,
  // and added to the environment, and the lets are ready to go

  private def evaluateTemplatedClosureFunctionFromNonCallForPrototype(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      closure: TemplatedClosure2):
  (Temputs, FunctionHeader2) = {
    val TemplatedClosure2(closureStructRef, terry) = closure;

    val env0 = ourEnv.globalEnv.getTerryLocalEnv(terry);

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val env1 =
      env0.addVariables(
        closureStructDef.members.map(member => {
          (member.name -> ClosureVariable2(closureStructRef, member.tyype.expectAddressMember().reference))
        }).toMap);
    val env2 =
      env1.addType(closureStructRef.humanName, ReferendTemplata(closureStructRef))

    evaluateTemplatedFunctionFromNonCallForPrototype(env1, temputs0, terry)
  }

  def evaluateTemplatedClosureFunctionFromCallForPrototype(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      closure: TemplatedClosure2,
      argTypes2: List[Reference2]):
  (Temputs, FunctionHeader2) = {
    val TemplatedClosure2(closureStructRef, terry) = closure;

    val env0 = ourEnv.globalEnv.getTerryLocalEnv(terry);

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val env1 =
      env0.addVariables(
        closureStructDef.members.map(member => {
          (member.name -> ClosureVariable2(closureStructRef, member.tyype.expectAddressMember().reference))
        }).toMap);
    val env2 =
      env1.addType(closureStructRef.humanName, ReferendTemplata(closureStructRef))

    println("Hardcoding borrow, will need to change for free lambdas")
    val closureArgOwnership = if (temputs0.lookupMutability(closureStructRef) == Mutable) Borrow else Share
    val closureArgType = Reference2(closureArgOwnership, closureStructRef)

    // We also pass in the closure, so make sure that's included in the argTypes
    val argsIncludingClosure2 = closureArgType :: argTypes2

    evaluateTemplatedFunctionFromCallForPrototype(
      env2, temputs0, terry, argsIncludingClosure2)
  }

  def evaluateTemplatedLightFunctionFromCallForBanner(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry,
      argTypes2: List[Reference2]):
  (Temputs, FunctionBanner2) = {
    //    println("blark " + terry.function1.header.name + " " + terry.alreadySpecifiedTemplateArgs + " " + argTypes2)
    val TemplataFunctionTerry(_, function1, explicitTemplateArgs) = terry;

    val env = ourEnv.globalEnv.getTerryLocalEnv(terry);

    assert(function1.closuredNames.isEmpty)

    evaluateTemplatedFunctionFromCallForBanner(env, temputs0, terry, argTypes2)
  }

  def evaluateTemplatedLightFunctionFromCallForPrototype(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry,
      argTypes2: List[Reference2]):
  (Temputs, FunctionHeader2) = {
//    println("blark " + terry.function1.header.name + " " + terry.alreadySpecifiedTemplateArgs + " " + argTypes2)
    val TemplataFunctionTerry(_, function1, explicitTemplateArgs) = terry;

    val env = ourEnv.globalEnv.getTerryLocalEnv(terry);

    assert(function1.closuredNames.isEmpty)

    argTypes2.foreach({
      case Reference2(_, StructRef2("SB", _)) => assert(false);
      case _ => {}
    })

    evaluateTemplatedFunctionFromCallForPrototype(env, temputs0, terry, argTypes2)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForTemputs(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1):
  Temputs = {
    val (temputs1, prototype) =
      evaluateOrdinaryLightFunctionForPrototype(
        env, temputs0, function1)
    temputs1
  }

  def evaluateOrdinaryClosureFunctionFromNonCallForPrototype(
      env0: LocalEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      function1: Function1):
  (Temputs, FunctionHeader2) = {
      evaluateOrdinaryClosureFunctionForPrototype(
        env0, temputs0, closureStructRef, function1)
  }

  def evaluateOrdinaryLightFunctionFromNonCallForPrototype(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1):
  (Temputs, FunctionHeader2) = {
//    val (temputs1, banner) = evaluateOrdinaryLightBannerForTemputs(env, temputs0, function1)
//    if (temputs1.exactDeclaredSignatureExists(banner.humanName, banner.paramTypes)) {
//      val prototype = temputs1.functions.find(_.header.toBanner == banner).get.header.toPrototype
//      (temputs1, prototype)
//    } else {
//      val (temputs2, prototype2) =
        evaluateOrdinaryLightFunctionForPrototype(env, temputs0, function1)
//      (temputs2, prototype2)
//    }
  }

//  private def evaluateOrdinaryLightFunctionFromNonCallForBannerAndTemputs(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      name: String,
//      paramTypes: List[Reference2]):
//  (Temputs, FunctionBanner2) = {
//    val (temputs1, banner) = evaluateOrdinaryLightBanner(env, temputs0, name, paramTypes)
//    if (temputs1.exactDeclaredSignatureExists(banner.humanName, List(), banner.paramTypes)) {
//      (temputs1, banner)
//    } else {
//      val originFunction1 =
//        env.globalEnv.originFunctions1ByOrdinarySignature(Signature2(name, List(), paramTypes))
//          ...?
//    }
//  }

  private def evaluateTemplatedLightBannerFromNonCall(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry):
  (Temputs, FunctionBanner2) = {
    val TemplataFunctionTerry(maybeLocalEnv, function1, alreadySpecifiedTemplateArgs) = terry;

    val env0 = ourEnv.globalEnv.getTerryLocalEnv(terry);

    // Check preconditions
    assert(function1.closuredNames.isEmpty)
    assert(function1.templateParams.nonEmpty);

    if (function1.templateParams.size != alreadySpecifiedTemplateArgs.size) {
      throw new RuntimeException("Specified wrong number of args (" + alreadySpecifiedTemplateArgs.size + ") for " + function1.name + " (num template params " + function1.templateParams.size + ")")
    }

    val env2 =
      function1.templateParams.zip(alreadySpecifiedTemplateArgs).foldLeft(env0)({
        case (env1, (templateParam, templateArgType)) =>
          env1.addType(templateParam.name, templateArgType)
      })

    val templateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0,
        function1.templateParams,
        terry.alreadySpecifiedExplicitTemplateArgs);
    assert(templateArgs.size == function1.templateParams.size)

    evaluateLightBanner(env2.localEnv, temputs0, function1, templateArgs)
  }

  // This is called while we're trying to figure out what function1s to call when there
  // are a lot of overloads available.
  // This assumes it met any type bound restrictions (or, will; not implemented yet)
  def evaluateTemplatedLightBannerAndTerryFromCall(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry,
      argTypes2: List[Reference2]):
  (Temputs, Option[(FunctionBanner2, TemplataFunctionTerry)]) = {
    val TemplataFunctionTerry(_, function1, alreadySpecifiedTemplateArgs) = terry;

    val coercedAlreadySpecifiedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0, function1.templateParams, alreadySpecifiedTemplateArgs)

    val env0 = ourEnv.globalEnv.getTerryLocalEnv(terry);

    // Check preconditions
    assert(function1.closuredNames.isEmpty)
    assert(function1.templateParams.nonEmpty);

    val maybeTemplateArgTemplatas =
      evaluateFunctionTemplateArgs(
        env0, temputs0, function1, argTypes2, coercedAlreadySpecifiedTemplateArgs)

    maybeTemplateArgTemplatas match {
      case None => (temputs0, None)
      case Some(templateArgTemplatas) => {
        val env2 =
          function1.templateParams.zip(templateArgTemplatas).foldLeft(env0)({
            case (env1, (templateParam, templateArgType)) =>
              env1.addType(templateParam.name, templateArgType.templata)
          })

        val (temputs1, banner) = evaluateLightBanner(env2.localEnv, temputs0, function1, coercedAlreadySpecifiedTemplateArgs)
        (temputs1, Some((banner, TemplataFunctionTerry(Some(env2.localEnv), function1, alreadySpecifiedTemplateArgs))))
      }
    }
  }

  private def evaluateTemplatedLightBanner(env0: LocalEnvironment, temputs0: Temputs, terry: TemplataFunctionTerry):
  (Temputs, FunctionBanner2) = {
    val TemplataFunctionTerry(_, function1, explicitTemplateArgs) = terry;

    val env2 =
      function1.templateParams.zip(explicitTemplateArgs)
          .foldLeft(env0)({
            case (env1, (templateParam, templateArgType)) =>
              env1.addType(templateParam.name, templateArgType)
          })

    val templateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0,
        function1.templateParams,
        terry.alreadySpecifiedExplicitTemplateArgs);
    if (templateArgs.size != function1.templateParams.size) {
      throw new RuntimeException("wat")
    }

    evaluateLightBanner(env2, temputs0, function1, templateArgs)
  }

  private def evaluateOrdinaryLightBannerForTemputs(env: LocalEnvironment, temputs0: Temputs, function1: Function1):
  (Temputs, FunctionBanner2) = {
    assert(function1.templateParams.isEmpty)
    assert(function1.closuredNames.isEmpty)
    evaluateLightBanner(env, temputs0, function1, List())
  }

  private def evaluateOrdinaryLightBannerForScan(env: GlobalEnvironment, temputs0: Temputs, function1: Function1):
  (Temputs, FunctionBanner2) = {
    assert(function1.templateParams.isEmpty)
    assert(function1.closuredNames.isEmpty)
    evaluateLightBanner(env.spawnLocalEnv(), temputs0, function1, List())
  }

//  private def evaluateOrdinaryLightBanner(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      name: String,
//      paramTypes: List[Reference2]):
//  (Temputs, FunctionBanner2) = {
//    val function1 =
//      env.globalEnv.originFunctions1ByOrdinarySignature(
//        Signature2(name, List(), paramTypes))
//    assert(function1.header.templateParams.isEmpty)
//    assert(function1.closuredNames.isEmpty)
//    evaluateLightBanner(env, temputs0, function1, List())
//  }

  // Preconditions:
  // - either no template args, or they were already added to the env.
  // - not a closure.
  private def evaluateLightBanner(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1,
      templateArgs: List[CoercedTemplateArg2]):
  (Temputs, FunctionBanner2) = {
    // Check preconditions
    function1.templateParams.foreach(templateParam => {
      assert(env.typeMembers.contains(templateParam.name));
    })
    assert(function1.closuredNames.isEmpty)

    val (temputs1, params2) = evaluateFunctionParams(env, temputs0, function1.params);
    (temputs1, FunctionBanner2(Some(function1), function1.name, templateArgs, params2))
  }

  def evaluateTemplatedLightFunctionFromNonCallForPrototype(
      ourEnv: LocalEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry):
  (Temputs, FunctionHeader2) = {
    val TemplataFunctionTerry(_, function1, explicitTemplateArgs) = terry;

    val env = ourEnv.globalEnv.getTerryLocalEnv(terry);

    assert(function1.templateParams.size == explicitTemplateArgs.size);
    assert(function1.closuredNames.isEmpty)

    evaluateTemplatedFunctionFromNonCallForPrototype(env, temputs0, terry)
  }

  def evaluateOrdinaryLightFunctionForPrototype(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1):
  (Temputs, FunctionHeader2) = {
    // This should only be called with a non-templated function
    assert(function1.templateParams.isEmpty);

    val (temputs1, params2) = evaluateFunctionParams(env, temputs0, function1.params);

    evaluateOrdinaryFunctionForPrototype(env, temputs1, function1, params2)
  }

  def evaluateOrdinaryClosureFunctionForPrototype(
      env0: LocalEnvironment,
      temputs0: Temputs,
      closureStructRef: StructRef2,
      function1: Function1):
  (Temputs, FunctionHeader2) = {
    // This should only be called with a non-templated function
    assert(function1.templateParams.isEmpty);

    val closureStructDef = temputs0.lookupStruct(closureStructRef);

    val env1 =
      env0.addVariables(
        closureStructDef.members.map(member => {
          (member.name -> ClosureVariable2(closureStructRef, member.tyype.expectAddressMember().reference))
        }).toMap);
    val env2 =
      env1.addType(
        closureStructRef.humanName,
        ReferendTemplata(closureStructRef))

    val (temputs1, params2) = evaluateFunctionParams(env2, temputs0, function1.params);

    evaluateOrdinaryFunctionForPrototype(
      env2, temputs1, function1, params2)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  private def evaluateOrdinaryFunctionForPrototype(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1,
      params2: List[Parameter2]):
  (Temputs, FunctionHeader2) = {
    // Check preconditions
    function1.closuredNames.foreach(closuredName => {
      assert(env.variables.contains(closuredName));
    })
    assert(function1.templateParams.isEmpty);

    evaluateFunctionForPrototype(env, temputs0, function1, List(), params2)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  private def evaluateTemplatedFunctionFromNonCallForPrototype(
      env0: TerryEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry):
  (Temputs, FunctionHeader2) = {
    val TemplataFunctionTerry(_, function1, alreadySpecifiedTemplateArgs) = terry;

    // Check preconditions
    function1.closuredNames.foreach(closuredName => {
      assert(env0.localEnv.variables.contains(closuredName));
    })
    assert(function1.templateParams.nonEmpty);

    assert(function1.templateParams.size == alreadySpecifiedTemplateArgs.size);

    val coercedAlreadySpecifiedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0, function1.templateParams, alreadySpecifiedTemplateArgs)

    val env2 =
      function1.templateParams.zip(coercedAlreadySpecifiedTemplateArgs)
      .foldLeft(env0)({
        case (env1, (templateParam, coercedTemplateArgType)) =>
          env1.addType(templateParam.name, coercedTemplateArgType.templata)
      })

    val (temputs1, params2) = evaluateFunctionParams(env0.localEnv, temputs0, function1.params);

    evaluateFunctionForPrototype(
      env2.localEnv, temputs1, function1, coercedAlreadySpecifiedTemplateArgs, params2)
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env0 is the environment the templated function was made in
  private def evaluateTemplatedFunctionFromCallForBanner(
      env0: TerryEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry,
      argTypes2: List[Reference2]):
  (Temputs, FunctionBanner2) = {
    val TemplataFunctionTerry(_, function1, alreadySpecifiedTemplateArgs) = terry;

    // Check preconditions
    function1.closuredNames.foreach(closuredName => {
      assert(env0.localEnv.variables.contains(closuredName));
    })
    assert(function1.templateParams.nonEmpty);

    val coercedAlreadySpecifiedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0, function1.templateParams, alreadySpecifiedTemplateArgs)

    val maybeCoercedTemplateArgs =
      evaluateFunctionTemplateArgs(
        env0,
        temputs0, function1, argTypes2, coercedAlreadySpecifiedTemplateArgs)

    assert(maybeCoercedTemplateArgs.nonEmpty)
    val coercedTemplateArgs = maybeCoercedTemplateArgs.get;

    val env2 =
      function1.templateParams.zip(coercedTemplateArgs).foldLeft(env0)({
        case (env1, (templateParam, coercedTemplateArgType)) =>
          env1.addType(templateParam.name, coercedTemplateArgType.templata)
      })

    val (temputs1, params2) =
      evaluateFunctionParams(env2.localEnv, temputs0, function1.params);

    temputs1.lookupFunction(Signature2(function1.name, coercedTemplateArgs, params2.map(_.tyype))) match {
      case Some(Function2(header, _)) => {
        (temputs1, header.toBanner)
      }
      case None => {
        val signature2 = Signature2(function1.name, coercedTemplateArgs, params2.map(_.tyype))
        if (temputs1.declaredSignatures.contains(signature2)) {
          (temputs1, FunctionBanner2(Some(function1), function1.name, coercedTemplateArgs, params2))
        } else {
          val temputs2 = temputs1.declareFunctionSignature(signature2)
          val (temputs3, header) =
            evaluateFunctionForPrototype(env2.localEnv, temputs2, function1, coercedTemplateArgs, params2)
          (temputs3, header.toBanner)
        }
      }
    }
  }

  // Preconditions:
  // - either no closured vars, or they were already added to the env.
  // - env0 is the environment the templated function was made in
  private def evaluateTemplatedFunctionFromCallForPrototype(
      env0: TerryEnvironment,
      temputs0: Temputs,
      terry: TemplataFunctionTerry,
      argTypes2: List[Reference2]):
  (Temputs, FunctionHeader2) = {
    val TemplataFunctionTerry(_, function1, alreadySpecifiedTemplateArgs) = terry;

    // Check preconditions
    function1.closuredNames.foreach(closuredName => {
      assert(env0.localEnv.variables.contains(closuredName));
    })
    assert(function1.templateParams.nonEmpty);

    val coercedAlreadySpecifiedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0, function1.templateParams, alreadySpecifiedTemplateArgs)

    val maybeCoercedTemplateArgs =
      evaluateFunctionTemplateArgs(
        env0,
        temputs0, function1, argTypes2, coercedAlreadySpecifiedTemplateArgs)

    assert(maybeCoercedTemplateArgs.nonEmpty)
    val coercedTemplateArgs = maybeCoercedTemplateArgs.get;

    val env2 =
      function1.templateParams.zip(coercedTemplateArgs).foldLeft(env0)({
        case (env1, (templateParam, coercedTemplateArgType)) =>
          env1.addType(templateParam.name, coercedTemplateArgType.templata)
      })

    val (temputs1, params2) =
      evaluateFunctionParams(env2.localEnv, temputs0, function1.params);

    temputs1.lookupFunction(Signature2(function1.name, coercedTemplateArgs, params2.map(_.tyype))) match {
      case Some(Function2(header, _)) => {
        (temputs1, header)
      }
      case None => {
        evaluateFunctionForPrototype(env2.localEnv, temputs1, function1, coercedTemplateArgs, params2)
      }
    }
  }

  // Preconditions:
  // - either no template args, or they were already added to the env.
  // - either no closured vars, or they were already added to the env.
  // We're handing in the params2 in case the caller wanted to add any in (like __Closure)
  private def evaluateFunctionForPrototype(
      env0: LocalEnvironment,
      temputs0: Temputs,
      function1: Function1,
      templateArgs: List[CoercedTemplateArg2],
      params2: List[Parameter2]):
  (Temputs, FunctionHeader2) = {
    // Check preconditions
    function1.templateParams.foreach(templateParam => {
      assert(env0.typeMembers.contains(templateParam.name));
    })
    function1.closuredNames.foreach(closuredName => {
      assert(env0.variables.contains(closuredName));
    })

    // Eventual goal is to be able to call innerEvaluateOrdinaryLightFunctionForPrototype.
    // For that, we need to put any template args and params into the environment.
    // This is an ordinary function, so has no template args.
    // We just need to put params into the environment.

    val env1 =
      env0.addVariables(params2.map(param2 => param2.name -> LocalVariable2(param2.tyype)).toMap)

    InnerFunctionTemplar.innerEvaluateFunctionForPrototype(
      env1,
      temputs0,
      function1,
      templateArgs,
      params2)
  }

  private def evaluateFunctionParams(
      env: LocalEnvironment,
      temputs0: Temputs,
      params1: List[Parameter1]):
  (Temputs, List[Parameter2]) = {
    val paramNames = params1.map(_.capturePattern.name);
    val paramVirtualities = params1.map(_.virtuality);

    val (temputs2, paramTypes2) =
      PatternTemplar.getParameterType2s(env, temputs0, params1);

    val params2 =
      (paramNames zip paramVirtualities zip paramTypes2)
          .map({ case ((name, virtuality), tyype) => Parameter2(name, virtuality, tyype) });

    // todo: at this point, to allow for recursive calls, add a callable type to the environment
    // for everything inside the body to use

    (temputs2, params2)
  }

  // Returns *all* the template args, not just the inferred ones.
  private def evaluateFunctionTemplateArgs(
      env: TerryEnvironment,
      temputs0: Temputs,
      function1: scout.Function1,
      argTypes: List[Reference2],
      explicitTemplateArgs: List[CoercedTemplateArg2]):
  Option[List[CoercedTemplateArg2]] = {
    val paramTypes = PatternTemplar.getParameterType1s(function1.params)

    // Note, function1.templateParams might be longer than explicitTemplateArgs
    // and thats okay, thats why we're inferring the rest
    val maybeInferredTemplateArgs =
      InferTemplateTemplar.inferTemplatas(
        env,
        temputs0,
        function1.templateParams,
        explicitTemplateArgs,
        argTypes.map(ReferenceTemplata),
        paramTypes)

    maybeInferredTemplateArgs match {
      case Some(inferredTemplateArgsByName) => {
        val templateParamsNeedingInferring =
          function1.templateParams.map(_.name).drop(explicitTemplateArgs.size)
        val templateArgs =
          explicitTemplateArgs ++
          templateParamsNeedingInferring.map(inferredTemplateArgsByName);
        if (templateArgs.size != function1.templateParams.size) {
          throw new RuntimeException("Couldnt infer all the template args! " + function1.templateParams + " args: " + templateArgs);
        }
        Some(templateArgs)
      }
      case None => None
    }
  }

  def evaluateClosureStruct(
      env: LocalEnvironment,
      temputs0: Temputs,
      function1: scout.Function1):
  (Temputs, StructRef2) = {
    InnerFunctionTemplar.evaluateClosureStruct(env, temputs0, function1)
  }

  def scanAbstractFunctions(env0: GlobalEnvironment, temputs0: Temputs, abstractFunctions1: List[NTVFunction1]):
  (GlobalEnvironment, Temputs) = {
    abstractFunctions1.foldLeft((env0, temputs0))({
      case ((env1, temputs1), function1) => {
        scanAbstractFunction(env1, temputs1, function1)
      }
    })
  }

  private def scanAbstractFunction(
      env0: GlobalEnvironment,
      temputs0: Temputs,
      ntvFunction1: NTVFunction1):
  (GlobalEnvironment, Temputs) = {
    if (ntvFunction1.templateParams.isEmpty) {
      val (temputs2, banner) = evaluateOrdinaryLightBannerForScan(env0, temputs0, ntvFunction1.origin)

      val env1 = env0.addFunctionBanner(banner.humanName, banner);
      val temputs3 = temputs2.declareFunctionSignature(banner.toSignature)
      assert(temputs3.exactDeclaredSignatureExists(banner.humanName, banner.templateArgs, banner.paramTypes))

      val temputs4 =
        ntvFunction1.origin.ret match {
          case None => temputs2
          case Some(returnType1) => {
            val (temputs4, returnReferenceType2) =
              TypeTemplar.evaluateAndReferencifyType(env1, temputs3, returnType1, Own);
            temputs4.declareFunctionReturnType(banner.toSignature, returnReferenceType2)
          }
        }
      (env1, temputs4)
    } else {
      val env1 = env0.addFunctionTemplate(ntvFunction1.origin)
      (env1, temputs0)
    }
  }

  def scanImplementedFunctions(env0: GlobalEnvironment, temputs0: Temputs, functions1: List[Function1]):
  (GlobalEnvironment, Temputs) = {
    functions1.foldLeft((env0, temputs0))({
      case ((env1, temputs1), function1) => {
        scanImplementedFunction(env1, temputs1, function1)
      }
    })
  }

  private def scanImplementedFunction(
      env0: GlobalEnvironment,
      temputs0: Temputs,
      function1: scout.Function1):
  (GlobalEnvironment, Temputs) = {
    if (function1.templateParams.isEmpty) {
      val (temputs2, banner) = evaluateOrdinaryLightBannerForScan(env0, temputs0, function1)

      val env1 = env0.addFunctionBanner(banner.humanName, banner);
      val temputs3 = temputs2.declareFunctionSignature(banner.toSignature)
      assert(temputs3.exactDeclaredSignatureExists(banner.humanName, banner.templateArgs, banner.paramTypes))

      val temputs5 =
        function1.ret match {
          case None => temputs3
          case Some(returnType1) => {
            val (temputs4, returnPointerType2) =
              TypeTemplar.evaluateAndReferencifyType(env1, temputs3, returnType1, Own);
            temputs4.declareFunctionReturnType(banner.toSignature, returnPointerType2)
          }
        }
      (env1, temputs5)
    } else {
      val env1 = env0.addFunctionTemplate(function1)
      (env1, temputs0)
    }
  }

  def scanExternFunctions(env0: GlobalEnvironment, temputs0: Temputs, externs: Map[String, List[FunctionHeader2]]) = {
    externs.foldLeft((env0, temputs0))({
      case ((env3, temputs3), (name, headers)) => {
        headers.foldLeft((env3, temputs3))({
          case ((env4, temputs4), header) => {
            val env5 = env4.addFunctionBanner(name, header.toBanner)
            val temputs5 = temputs4.declareFunctionSignature(header.toSignature)
            val temputs6 = temputs5.declareFunctionReturnType(header.toSignature, header.returnType)

            val function2 = InnerFunctionTemplar.makeExternFunction(header);
            val temputs7 = temputs6.addFunction(function2)

            assert(temputs7.exactDeclaredSignatureExists(header.toBanner.humanName, header.toBanner.templateArgs, header.toBanner.paramTypes))

            (env5, temputs7)
          }
        })
      }
    })
  }
}