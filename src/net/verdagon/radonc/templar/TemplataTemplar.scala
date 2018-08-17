package net.verdagon.radonc.templar

import scala.collection.immutable.{List, Nil}

object TemplataTemplar {

  def coerceTemplatasToReferences(
      temputs0: Temputs,
      templatas: List[ITemplata],
      ownershipIfMutable: Ownership):
  List[Reference2] = {
    templatas.map({
      case ReferenceTemplata(reference) => reference
      case ReferendTemplata(referend) => {
        TypeTemplar.pointifyReferend(temputs0, referend, ownershipIfMutable)
      }
      case _ => throw new RuntimeException("not yet")
    })
  }

//  def coerceTemplataToExpression(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      templata: IEvaluatedTemplata):
//  (Temputs, Expression2) = {
//    templata match {
//      case TemplataAddressExpression(expr) => (temputs0, expr)
//      case TemplataValue(value) => (temputs0, coerceValueTemplataToReferenceExpression(value))
////      case TemplataOrdinaryLambdaFunction(understructRef2, prototype) => (temputs0, FunctionLookup2(prototype))
//      case TemplataReferenceExpression(expr2) => (temputs0, expr2)
//      case gfg @ TemplataGlobalFunctionGroup(_, _) => {
//        val (temputs1, header) = coerceGlobalFunctionGroupToPrototype(env, temputs0, gfg)
//        (temputs1, FunctionLookup2(header.toPrototype))
//      }
//      case _  => throw new RuntimeException("wat " + templata)
//    }
//  }

//  def coerceTemplataToAddressExpression(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      templata: IEvaluatedTemplata):
//  (Temputs, AddressExpression2) = {
//    templata match {
//      case TemplataAddressExpression(expr) => (temputs0, expr)
//      case _  => throw new RuntimeException("wat " + templata)
//    }
//  }
//
//  def coerceTemplataToReferenceExpression(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      templata: IEvaluatedTemplata):
//  (Temputs, ReferenceExpression2) = {
//    templata match {
//      case TemplataValue(value) => (temputs0, coerceValueTemplataToReferenceExpression(value))
////      case TemplataOrdinaryLambdaFunction(understructRef2, prototype) => (temputs0, FunctionLookup2(prototype))
//      case TemplataReferenceExpression(expr2) => (temputs0, expr2)
//      case TemplataAddressExpression(expr2) => {
//        (temputs0, SoftLoad2(expr2, expr2.resultRegister.reference.ownership))
//      }
////      case gfg @ TemplataGlobalFunctionGroup(_, _) => {
////        val (temputs1, header) = coerceGlobalFunctionGroupToPrototype(env, temputs0, gfg)
////        (temputs1, FunctionLookup2(header.toPrototype))
////      }
////      case TemplataLightTemplateLambdaFunction(understructRef2, terry) => {
////        val (temputs1, templata, exporteds, hoistees) =
////          ExpressionTemplar.evaluateClosure(env, temputs0, terry.function1)
////        println("throwing away exporteds and hoistees?")
////        coerceTemplataToReferenceExpression(env, temputs1, templata)
////      }
//      case _  => throw new RuntimeException("wat or not yet " + templata)
//    }
//  }

//  def coerceTemplatasToReferenceExpressions(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      templatas: List[IEvaluatedTemplata]):
//  (Temputs, List[ReferenceExpression2]) = {
//    templatas match {
//      case Nil => (temputs0, Nil)
//      case headTemplata :: tailTemplatas => {
//        val (temputs1, headExpr2) = coerceTemplataToReferenceExpression(env, temputs0, headTemplata)
//        val (temputs2, tailExprs2) = coerceTemplatasToReferenceExpressions(env, temputs1, tailTemplatas)
//        (temputs2, headExpr2 :: tailExprs2)
//      }
//    }
//  }

//  def coerceTemplatasToExpressions(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      templatas: List[IEvaluatedTemplata]):
//  (Temputs, List[Expression2]) = {
//    templatas match {
//      case Nil => (temputs0, Nil)
//      case headTemplata :: tailTemplatas => {
//        val (temputs1, headExpr2) = coerceTemplataToExpression(env, temputs0, headTemplata)
//        val (temputs2, tailExprs2) = coerceTemplatasToExpressions(env, temputs1, tailTemplatas)
//        (temputs2, headExpr2 :: tailExprs2)
//      }
//    }
//  }

//  def coerceValueTemplataToReferenceExpression(v: IValueTemplata): ReferenceExpression2 = {
//    v match {
//      case BooleanTemplata(value) => BoolLiteral2(value)
//      case IntegerTemplata(value) => IntLiteral2(value)
//      case NoneTemplata() => NoneLiteral2()
//    }
//  }

}
