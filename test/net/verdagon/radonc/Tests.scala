package net.verdagon.radonc

import net.verdagon.radonc.carpenter.CarpenterTests
import net.verdagon.radonc.parser.BiggerTests
import net.verdagon.radonc.parser.ExpressionTests
import net.verdagon.radonc.parser.PatternTests
import net.verdagon.radonc.templar.{TemplarTests, VirtualTests}

object Tests {
  def main(args: Array[String]) {
    PatternTests.runTests();
    ExpressionTests.runTests();
    BiggerTests.runTests();
    NamifierTests.runTests();
    println("Done")
  }
}
