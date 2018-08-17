package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc.scout.Function1

import scala.collection.immutable.List

sealed trait Callability
case class NotCallable() extends Callability
case class Callable(infix: Boolean) extends Callability

object Callabilities {
  def getCallabilities(env: LocalEnvironment, types: List[Referend2]): List[Callability] = {
    types.map(tyype => getCallability(env, tyype))
  }

  def getCallability(env: LocalEnvironment, tyype: Referend2): Callability = {
    tyype match {
      case FunctionT2(params, args) => {
        Callable(params.length == 2)
      }
      case PackT2(innerTypes, underlyingStruct) => {
        if (innerTypes.size == 1) {
          throw new RuntimeException("wat " + innerTypes); // curious, can we ever have a size-1 pack?
//          getCallability(temputs, innerTypes.head.referend)
        } else {
          NotCallable()
        }
      }
      case Void2() => NotCallable()
      case Int2() => NotCallable()
      case Bool2() => NotCallable()
      case Str2() => NotCallable()
      case Float2() => NotCallable()
      case Nothing2() => NotCallable()
      case ArrayT2(_, _) => NotCallable()
      case TemplatedClosure2(_, terry) => {
        Callable(terry.function1.params.size == 3)
      }
      case OrdinaryClosure2(_, signature2) => {
        // 3 because the first parameter will be the closure vars struct.
        Callable(signature2.functionType.paramTypes.length == 3)
      }
      case structRef @ StructRef2(_, _) => {
        // TODO: look for any __call
        NotCallable()
      }
      case interfaceRef : InterfaceRef2 => {
        // TODO: look for any __call
        NotCallable()
      }
      case GlobalFunctionGroup2(understruct, name, alreadySpecifiedTemplateArgs) => {
        val ordinarySignaturesCallabilities =
          if (alreadySpecifiedTemplateArgs.size == 0) {
            val ordinaryBanners = env.globalEnv.ordinaryBanners.getOrElse(name, List())
            ordinaryBanners.map(getCallability)
          } else {
            List()
          }
        val templatedBanners =
          env.globalEnv.functionTemplates.getOrElse(name, List())
            .filter(_.templateParams.size >= alreadySpecifiedTemplateArgs.size)
        val templatedFunctionsCallabilities = templatedBanners.map(getCallability)

        combineCallabilities(
          ordinarySignaturesCallabilities.toList ++ templatedFunctionsCallabilities)
      }
      case _ => throw new RuntimeException("wat " + tyype)
    }
  }

  private def getCallability(banner: FunctionBanner2): Callability = {
    return Callable(banner.paramTypes.size == 2);
  }

  private def getCallability(signature2: Signature2): Callability = {
    Callable(signature2.paramTypes.size == 2);
  }

  private def getCallability(function1: Function1): Callability = {
    Callable(function1.params.size == 2);
  }

  private def combineCallabilities(callabilities: List[Callability]): Callability = {
    callabilities match {
      case Nil => throw new RuntimeException("wat")
      case first :: Nil => first
      case first :: rest => {
        (first, combineCallabilities(rest)) match {
          // The ordering of these matters
          case (Callable(true), _) => Callable(true)
          case (_, Callable(true)) => Callable(true)
          case (Callable(false), _) => Callable(false)
          case (_, Callable(false)) => Callable(false)
          case (NotCallable(), _) => NotCallable()
          case (_, NotCallable()) => NotCallable()
          case _ => throw new RuntimeException("wat")
        }
      }
    }
  }
//
//  private def getCallability(
//      env: LocalEnvironment,
//      temputs: Temputs,
//      element: Referend2):
//  Callability = {
//    element match {
//      case TemplataValue(_) => NotCallable()
//      case TemplataReferenceExpression(expr2) => getCallability(temputs, expr2.resultRegister.reference.referend)
//      case TemplataAddressExpression(expr2) => getCallability(temputs, expr2.resultRegister.reference.referend)
////      case TemplataOrdinaryLambdaFunction(_, prototype2) => getCallability(prototype2.toSignature)
//      case TemplataLightTemplateLambdaFunction(_, terry) => getCallability(terry.function1)
//      case TemplataGlobalFunctionGroup(name, alreadySpecifiedTemplateArgs) => {
//        val ordinaryBanners = env.globalEnv.ordinaryBanners.getOrElse(name, List())
//        val ordinarySignaturesCallabilities = ordinaryBanners.map(getCallability)
//        val templatedBanners = env.globalEnv.functionTemplates.getOrElse(name, List())
//        val templatedFunctionsCallabilities = templatedBanners.map(getCallability)
//        combineCallabilities(
//          ordinarySignaturesCallabilities.toList ++ templatedFunctionsCallabilities)
//      }
//    }
//  }
//
//  def getCallabilities(env: LocalEnvironment, temputs: Temputs, elements: List[IEvaluatedTemplata]): List[Callability] = {
//    elements match {
//      case Nil => List()
//      case first :: rest => getCallability(env, temputs, first) :: getCallabilities(env, temputs, rest)
//    }
//  }
}
