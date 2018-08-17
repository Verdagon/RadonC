package net.verdagon.radonc

import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.vivem._
import org.scalatest.{FunSuite, Matchers}

class ArrayTests extends FunSuite with Matchers {
  test("Simple arraysequence and compiletime index lookup") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let a = [2, 3, 4, 5, 6];
        |  a.3
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.only({
      case Let2("a", _, expr) => {
        expr.resultRegister.reference.referend match {
          case ArraySequenceT2(5, ArrayT2(Reference2(Share, Int2()), Immutable)) =>
        }
      }
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

//  test("Returning array from function and dotting it") {
//    val compile = new Compilation(
//      """
//        |fn makeArray() { [2, 3, 4, 5, 6] }
//        |fn main() {
//        |  makeArray().3
//        |}
//      """.stripMargin)
//
//    println(compile.getScoutput().implementedFunctions.find(_.name == "main").get)
//
//    compile.evalForReferend(Vector()) shouldEqual IntV(5)
//  }

  test("Simple arraysequence and runtime index lookup") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let i = 2;
        |  let a = [2, 3, 4, 5, 6];
        |  a.(i)
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.only({
      case ArrayLookup2(_, _, _) => {
      }
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(4)
  }

  test("Simple array map") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let a = __ImmArray(10, {_});
        |  a.3
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.only({
      case ConstructArray2(ArrayT2(Reference2(Share, Int2()), Immutable), _, _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }

  test("Simple array map with runtime index lookup") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let a = __ImmArray(10, {_});
        |  let i = 5;
        |  a.(i)
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Nested array") {
    val compile = new Compilation(
      """
        |fn main() {
        |  [[2]].0.0
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual IntV(2)
  }


  test("Two dimensional array") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let board = __MutArray(20, {(row) __MutArray(20, {(col) row + col})});
        |  board.8.9
        |}
      """.stripMargin)

    // It's given a TemplatedClosure2. We can plug in an int, and see
    // what it returns.
    // fn __MutArray:F(size: Int, callable: F) {
    //   Templated
    // }

    compile.evalForReferend(Vector()) shouldEqual IntV(17)
  }

  // if we want to make sure that our thing returns an int, then we can
  // try and cast it to a callable:
  // fn makeArray:T(size: Int, callable: (Int):T) {
}
