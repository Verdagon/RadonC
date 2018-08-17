package net.verdagon.radonc

import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.vivem._
import org.scalatest.{FunSuite, Matchers}

class StringTests extends FunSuite with Matchers {
  test("Simple arraysequence and compiletime index lookup") {
    val compile = new Compilation(
      """
        |fn main() {
        |  "sprogwoggle"
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.only({ case StrLiteral2("sprogwoggle") => })

    compile.evalForReferend(Vector()) shouldEqual StrV("sprogwoggle")
  }
}
