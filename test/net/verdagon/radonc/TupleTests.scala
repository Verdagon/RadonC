package net.verdagon.radonc

import net.verdagon.radonc.templar._
import net.verdagon.radonc.vivem.{BoolV, IntV}
import org.scalatest.{FunSuite, Matchers}

class TupleTests extends FunSuite with Matchers {
  test("Simple tuple with one int") {
    val compile = new Compilation("fn main() { [9].0 }")

    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").header.returnType.referend shouldEqual Int2()
    // Funny story, theres no such thing as a one element tuple! It becomes a one element array.
    temputs.only({ case ArraySequenceE2(_, _, _) => })

    compile.evalForReferend(Vector()) shouldEqual IntV(9)
  }

  test("Tuple with two things") {
    val compile = new Compilation("fn main() { [9, true].1 }")
    compile.evalForReferend(Vector()) shouldEqual BoolV(true)
  }

  // todo: indexing into it with a variable, to get a union type
}
