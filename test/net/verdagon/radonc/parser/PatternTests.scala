package net.verdagon.radonc.parser

import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.{VParser, scout}

object PatternTests {
  private def check[T](testNumber: Double, code: String, expected: T, parser: VParser.Parser[Any] = VParser.elementsPattern) {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        println("No pattern " + testNumber + " " + msg)
      }
      case VParser.Success(function, rest) => {
        if (function.toString().equals(expected.toString())) {
//          println("Yes!")
        } else {
          println("Uh pattern " + testNumber + "...");
          println(function.toString());
        }
      }
    }
  }
  
  def runTests() {
    check(1, "()", List());
    check(2, "(a) ", List(CaptureP1("a",false,None)));
    check(2.5, "(a, b) ", List(CaptureP1("a",false,None), CaptureP1("b",false,None)));
    check(3, "(a:Int) ", List(CaptureP1("a",false,Some(TypeOfP1(TypeName1("Int"))))));
    check(3.5, "(a:Int, b:Int) ", List(CaptureP1("a",false,Some(TypeOfP1(TypeName1("Int")))), CaptureP1("b",false,Some(TypeOfP1(TypeName1("Int"))))));
    check(4, "(:Int)", List(TypeOfP1(TypeName1("Int"))));
    check(5, "(_:Int) ", List(TypeOfP1(TypeName1("Int"))));
    check(6, "(_) ", List(DiscardP1()));
    check(8, "(a:...Int) ", List(CaptureP1("a",false,Some(PackRestP1(TypeOfP1(TypeName1("Int")))))));
    check(11,
        "(a:(Int)void) ",
        List(CaptureP1("a",false,Some(scout.CallableP1(List(TypeOfP1(TypeName1("Int"))), TypeName1("void"))))));
//    check(12, "#T", TemplateParamName1("T"), VParser.tyype);
//    check(13,
//        "(Int)#T",
//        patterns.Callable1(List(TypeOf1("Int")), TemplateParamName1("T")),
//        VParser.callablePattern);
//    check(14,
//        "(a:(Int)#T)",
//        List(Capture1("a",false,Some(patterns.Callable1(List(TypeOf1("Int")), TemplateParamName1("T"))))));
    check(15, "([]) ", List(scout.TupleP1(List())));
    check(16, "([a: Int]) ", List(TupleP1(List(CaptureP1("a",false,Some(TypeOfP1(TypeName1("Int"))))))));
    check(17,
        "(tuple: [a: Int, b: Int])",
        List(CaptureP1("tuple",false,Some(TupleP1(List(CaptureP1("a",false,Some(TypeOfP1(TypeName1("Int")))), CaptureP1("b",false,Some(TypeOfP1(TypeName1("Int"))))))))));
  }
}
