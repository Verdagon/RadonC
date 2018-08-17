package net.verdagon.radonc.parser

import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.Virtual2

object BiggerTests {
  private def check[T](testNumber: Int, parser: VParser.Parser[T], code: String, expected: T) {
    VParser.parse(parser, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        println("No bigger " + testNumber + " " + msg)
      }
      case VParser.Success(function, rest) => {
        if (function.toString().equals(expected.toString())) {
//          println("Yes!")
        } else {
          println("Uh bigger " + testNumber + "...");
          println(function.toString());
        }
      }
    }
  }
  
  def runTests() {
    check(1, VParser.topLevelFunction,
        "fn sum(){4}",
        Function0(Some("sum"),false,false,List(), List(),None, Some(Block0(List(IntLiteral1(4))))));
    check(3, VParser.topLevelFunction,
        "fn sum(){3;}",
        Function0(Some("sum"),false,false,List(), List(), None, Some(Block0(List(IntLiteral1(3), Pack0(List()))))));
    check(4, VParser.program,
        "fn sum(){3} fn main(){sum()}",
        Program0(List(), List(), List(
            Function0(Some("sum"),false,false,List(), List(), None, Some(Block0(List(IntLiteral1(3))))),
            Function0(Some("main"),false,false,List(), List(), None, Some(Block0(List(Scramble0(List(Lookup0("sum"), scout.Pack0(List()))))))))));
    check(5, VParser.program,
        "fn call(a:Int){a()} fn sum(){3} fn main(){call(sum)}",
        Program0(List(),  List(), List(
            Function0(
              Some("call"),
              false, false,
              List(),
              List(Parameter0(None, CaptureP1("a", false, Some(TypeOfP1(TypeName1("Int")))))),
              None,
              Some(Block0(List(Scramble0(List(Lookup0("a"), scout.Pack0(List()))))))),
            Function0(Some("sum"),false, false,List(), List(), None, Some(Block0(List(IntLiteral1(3))))),
            Function0(Some("main"),false, false,List(), List(), None, Some(Block0(List(Scramble0(List(Lookup0("call"), Pack0(List(Lookup0("sum"))))))))))));
    check(6, VParser.program,
        "fn call(a){a()} fn sum(){3} fn main(){call(sum)}",
        Program0(List(),  List(), List(
            Function0(Some("call"),false, false,List(), List(Parameter0(None, CaptureP1("a", false, None))), None, Some(Block0(List(Scramble0(List(Lookup0("a"), scout.Pack0(List()))))))),
            Function0(Some("sum"),false, false,List(), List(), None, Some(Block0(List(IntLiteral1(3))))),
            Function0(Some("main"),false, false,List(), List(), None, Some(Block0(List(Scramble0(List(Lookup0("call"), Pack0(List(Lookup0("sum"))))))))))));
    check(7, VParser.program,
        "fn main(){{(a) a+a}(3)}",
        Program0(List(), List(),  List(
            Function0(Some("main"),false, false,List(), List(), None,
              Some(Block0(List(Scramble0(List(
                    Lambda0(Function0(None, false,false,List(), List(Parameter0(None, CaptureP1("a",false,None))), None,
                      Some(Block0(List(Lookup0("a+a")))))),
                    Pack0(List(IntLiteral1(3))))))))))));
    check(8, VParser.expression, "let x = 3", Let0(CaptureP1("x", false, None), IntLiteral1(3)));
    check(9, VParser.expression, "__mutate($&x, 5)",
        Scramble0(List(
            Lookup0("__mutate"),
            Pack0(List(
                Mutable0(Lend0(Lookup0("x"))),
                IntLiteral1(5))))));
    check(11, VParser.expression, "mut x = 5",
        Mutate0(Lookup0("x"),IntLiteral1(5)))


    check(12, VParser.program,
        "struct MyStruct { a: Int; } fn main() { let ms = MyStruct(7); ms.a }",
        Program0(
          List(
            Struct0("MyStruct",Mutable,List(),List(StructMember1("a",TypeName1("Int"))))),
          List(),
          List(
            Function0(
              Some("main"),
              false,
              false,
              List(),
              List(),
              None,
              Some(
                Block0(
                  List(
                    Let0(
                      CaptureP1("ms",false,None),
                      Scramble0(List(Lookup0("MyStruct"), Pack0(List(IntLiteral1(7)))))),
                    Dot0(Lookup0("ms"),"a"))))))))

    check(13, VParser.program,
      """
        |struct ListNode:T {
        |  head: T;
        |  tail: ?ListNode;
        |}
      """.stripMargin,
      Program0(
        List(Struct0("ListNode",Mutable,List(TemplateParameter1("T", ReferenceTemplataType1)),List(StructMember1("head",TypeName1("T")), StructMember1("tail",Nullable1(TypeName1("ListNode")))))),
        List(),
        List()))

    check(14, VParser.program,
      """
        |interface Option imm :T { }
        |struct None imm { }
        |None implements Option:Nothing;
        |struct Some imm :T { value: T; }
        |Some:T implements Option:T;
      """.stripMargin,
      Program0(
        List(
          Interface0("Option",Immutable,List(TemplateParameter1("T", ReferenceTemplataType1)),List()),
          Struct0("None",Immutable,List(),List()),
          Struct0("Some",Immutable,List(TemplateParameter1("T", ReferenceTemplataType1)),List(StructMember1("value",TypeName1("T"))))),
        List(
          Impl1("None",List(),TemplateCall1("Option",List(TypeName1("Nothing")))),
          Impl1("Some",List(TemplateParameter1("T", ReferenceTemplataType1)),TemplateCall1("Option",List(TypeName1("T"))))),
        List()))

    check(20, VParser.topLevelFunction,
      "fn doThing(virtual i: I) {}",
      Function0(
        Some("doThing"),
        false,false,
        List(),
        List(Parameter0(Some(Virtual2()),CaptureP1("i",false,Some(TypeOfP1(TypeName1("I")))))),
        None,
        None))

    check(21, VParser.program,
      """
        |interface MyInterface:T { }
        |abstract fn doThing:T(virtual x: MyInterface:T);
      """.stripMargin,
      Program0(List(),List(),List()))

    // =(mut(x), 5) ? so what if mut makes a &$
    // so first lets do &$x = 5
    // = will be a function that just calls the __mutate builtin
    // so i guess first we do the __mutate then lol
  }
}
