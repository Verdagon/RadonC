package net.verdagon.radonc

import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.vivem._
import org.scalatest.{FunSuite, Matchers}

class IfTests extends FunSuite with Matchers {
  test("Simple true branch returning an int") {
    val compile = new Compilation(
      """
        |fn main() {
        |  if {true} { 3 } else { 5 }
        |}
      """.stripMargin)
    val scoutput = compile.getScoutput()
    val main = scoutput.implementedFunctions.find(_.name == "main").get
    val If1(_, _, _) = main.body.get.elements.head

    val temputs = compile.getTemputs()
    temputs.only({ case If2(_, _, _) => })

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }

  test("Simple false branch returning an int") {
    val compile = new Compilation(
      """
        |fn main() {
        |  if {false} { 3 } else { 5 }
        |}
      """.stripMargin)

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Ladder") {
    val compile = new Compilation(
      """
        |fn main() {
        |  if {false} { 3 } else if {true} { 5 } else { 7 }
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.all({ case If2(_, _, _) => true }).size shouldEqual 2

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }
}
