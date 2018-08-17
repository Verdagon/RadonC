package net.verdagon.radonc.parser

import net.verdagon.radonc._
import net.verdagon.radonc._
import net.verdagon.radonc.{VParser, scout}
import net.verdagon.radonc.scout._

object ExpressionTests {
  private def check[T](testNumber: Int, code: String, expected: T) {
    VParser.parse(VParser.expression, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        println("No expression " + testNumber + " " + msg)
      }
      case VParser.Success(function, rest) => {
        if (function.toString().equals(expected.toString())) {
//          println("Yes!")
        } else {
          println("Uh expression " + testNumber + "...");
          println(function.toString());
        }
      }
    }
  }
  
  def runTests() {
    check(1, "4", IntLiteral1(4));
//    check(2, "4+5", Scramble0(List(IntLiteral1(4), Lookup0("+"), IntLiteral1(5))));
    check(3, "4 + 5", Scramble0(List(IntLiteral1(4), Lookup0("+"), IntLiteral1(5))));
    check(4, "()", Pack0(List()));
    check(5, "+(4, 5)", Scramble0(List(Lookup0("+"), Pack0(List(IntLiteral1(4), IntLiteral1(5))))));
    check(6, "x(y)", Scramble0(List(Lookup0("x"), Pack0(List(Lookup0("y"))))));
    check(7, "x y", Scramble0(List(Lookup0("x"), Lookup0("y"))));
    check(8, "(x)y", Scramble0(List(Pack0(List(Lookup0("x"))), Lookup0("y"))));
    check(12, "let (x, y) = (4, 5)", Let0(scout.PackP1(List(scout.CaptureP1("x",false,None), scout.CaptureP1("y",false,None))),Pack0(List(IntLiteral1(4), IntLiteral1(5)))));
//    check(9, "if {true} {false}", IfLadder0(List(IfPart0(Lambda0(List(),None,Lookup0("true")),Lambda0(List(),None,Lookup0("false")))),None));
//    check(10, "if {true} {false} else {true}", IfLadder0(List(IfPart0(Lambda0(List(),None,Lookup0("true")),Lambda0(List(),None,Lookup0("false")))),Some(Lambda0(List(),None,Lookup0("true")))));
//    check(11, "if{false}{0}else if{false}{1}else{2}", IfLadder0(List(IfPart0(Lambda0(List(),None,Lookup0("false")),Lambda0(List(),None,IntLiteral1(0))), IfPart0(Lambda0(List(),None,Lookup0("false")),Lambda0(List(),None,IntLiteral1(1)))),Some(Lambda0(List(),None,IntLiteral1(2)))));
  }
}


