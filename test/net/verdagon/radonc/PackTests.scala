package net.verdagon.radonc

import net.verdagon.radonc.carpenter.TetrisTable
import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.Immutable
import net.verdagon.radonc.templar.{Int2, PackE2, PackT2, Share}
import net.verdagon.radonc.vivem._
import org.scalatest.{FunSuite, Matchers}

class PackTests extends FunSuite with Matchers {
  test("Simple pack with one int") {
    val compile = new Compilation(
      """
        |fn main() {
        |  (5)
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").header.returnType.referend shouldEqual Int2()
    temputs.all({ case PackE2(_, _, _) => }).size shouldEqual 0

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Pack with two ints") {
    val compile = new Compilation(
      """
        |fn main() {
        |  (5, 9)
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").header.returnType.referend.only({ case PackT2(_, _) => })
    temputs.only({ case PackE2(_, _, _) => })

    compile.evalForReferend(Vector()) match {
      case
        StructInstanceV(
          StructDefinition3(
            _,
            "__Pack",
            _,
            Immutable,
            None,
            _,
            List(),
            List(
              StructMember3("0",ReferenceMemberType3(Reference3(Share,Int3()))),
              StructMember3("1",ReferenceMemberType3(Reference3(Share,Int3())))),
            List()),
          Vector(
            ReferenceMemberV(ReferenceV(RRReference(Reference3(Share,Int3())),_)),
            ReferenceMemberV(ReferenceV(RRReference(Reference3(Share,Int3())),_)))) =>
    }
  }

  test("Extract pack") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let (x, y, z) = (5, 6, 7);
        |  x
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.all({ case PackE2(_, _, _) => }).size shouldEqual 1

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Nested packs flatten") {
    val compile = new Compilation(
      """
        |fn main() {
        |  let (x, y, z) = (5, (6, 7));
        |  x
        |}
      """.stripMargin)

    val temputs = compile.getTemputs()
    temputs.all({ case PackE2(_, _, _) => }).size shouldEqual 1

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

}
