package net.verdagon.radonc

import net.verdagon.radonc._
import net.verdagon.radonc.carpenter.Carpenter
import net.verdagon.radonc.hammer.Hammer
import net.verdagon.radonc.scout._
import net.verdagon.radonc.sculptor.Sculptor
import net.verdagon.radonc.templar.Templar;

object NamifierTests {
  private def check[T](testNumber: Int, code: String, expected: T) {
    CommonEnv.runNamifier(code) match {
      case None => {
        println("No!")
      }
      case Some(program) => {
        if (program.toString().equals(expected.toString())) {
//          println("Yes!")
        } else {
          println("Uh namifier " + testNumber + "...");
          println(program.toString());
        }
      }
    }
  }
  
  def runTests() {
    check(1,
        "fn main(){3}",
        Program1(List(), List(), List(
            Function1("main",false,false,Set(), List(),List(),None,
                Some(Block1(List(IntLiteral1(3))))))));
//    check(
//        "main{let x = -3}",
//        Program1(List(
//            Function1("main",List(),None,
//                Let1(Capture1("x", Infer1()), IntLiteral1(-3))))));
    check(3,
        "fn main(){4 - 3}",
        Program1(List(), List(), List(
            Function1("main",false,false,Set(),List(),List(),None,
              Some(Block1(List(Scramble1(List(IntLiteral1(4), Lookup1("-"), IntLiteral1(3))))))))));
    check(4,
        "fn main(){{(a:Int) +(a,a)}(3)}",
        Program1(List(), List(), List(
            Function1("main",false,false,Set(),List(),List(),None,
              Some(Block1(List(
                Scramble1(List(
                    Function1("main:lam1",false,false,Set(),List(),List(Parameter1(None,2, CaptureP1("a",false,Some(TypeOfP1(TypeName1("Int")))))),None,
                      Some(Block1(List(
                        Scramble1(List(Lookup1("+"), scout.PackE1(List(Lookup1("a"), Lookup1("a"))))))))),
                  scout.PackE1(List(IntLiteral1(3))))))))))));
    check(5,"fn main(){{(a) a - 2}(3)}",
      Program1(List(), List(), List(
        Function1("main",false,false,Set(),List(),List(),None,
          Some(Block1(List(Scramble1(List(
            Function1("main:lam1",false,false,Set("__T2"),List(),List(Parameter1(None,3,CaptureP1("a",false,Some(TypeOfP1(TypeName1("__T2")))))),None,
              Some(Block1(List(Scramble1(List(Lookup1("a"), Lookup1("-"), IntLiteral1(2))))))),
            PackE1(List(IntLiteral1(3))))))))))))
    check(6,"fn main(){{(_) 2}(3)}",
      Program1(List(), List(), List(Function1("main",false,false,Set(),List(),List(),None,Some(Block1(List(Scramble1(List(
        Function1("main:lam1",false,false,Set(),List(TemplateParameter1("__T2", ReferenceTemplataType1)),List(Parameter1(None,3,CaptureP1("__P2",false,Some(TypeOfP1(TypeName1("__T2")))))),None,
          Some(Block1(List(IntLiteral1(2))))),
        PackE1(List(IntLiteral1(3))))))))))));
    check(7,
        "fn main(){let $a = 3; mut a = 4;}",
        Program1(List(), List(), List(
            Function1("main",false,false,Set(),List(),List(),None,
              Some(Block1(List(
                    Let1(1, CaptureP1("a",true,None),IntLiteral1(3)),
                    Mutate1(Lookup1("a"),IntLiteral1(4)),
                  scout.PackE1(List()))))))));

    // Make sure it's in the lambda's capture list but not main's
    check(8,
        "fn main() { let x = 4; { print x }() }",
        Program1(List(), List(), List(
          Function1("main",false,false,Set(),List(),List(),None,
            Some(Block1(List(
              Let1(1,CaptureP1("x",false,None),IntLiteral1(4)),
              Scramble1(List(
                Function1("main:lam2",false,false,Set("x"),List(),List(),None,
                  Some(Block1(List(Scramble1(List(Lookup1("print"), Lookup1("x"))))))),
                scout.PackE1(List()))))))))))


    // Make sure the outer lambda only captures x, and main captures nothing
    check(9,
        "fn main() { let x = 4; { let y = 5; { x + y }() }() }",
        Program1(List(), List(), List(
          Function1("main",false,false,Set(),List(),List(),None,Some(Block1(List(
            Let1(1,CaptureP1("x",false,None),IntLiteral1(4)),
            Scramble1(List(
              Function1("main:lam2",false,false,Set("x"),List(),List(),None,Some(Block1(List(
                Let1(3,CaptureP1("y",false,None),IntLiteral1(5)),
                Scramble1(List(
                  Function1("main:lam4",false,false,Set("x", "y"),List(),List(),None,
                    Some(Block1(List(Scramble1(List(Lookup1("x"), Lookup1("+"), Lookup1("y"))))))),
                  scout.PackE1(List()))))))),
              scout.PackE1(List()))))))))))

//    check(10,
//        """
//          |interface Car {
//          |	fn doCivicDance(this)Int;
//          |}
//        """.stripMargin,
//      List())
  }
}
