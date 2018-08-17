package net.verdagon.radonc.carpenter

import net.verdagon.radonc.{CommonEnv, Compilation}
import net.verdagon.radonc.hammer.{Hammer, Hinputs}
import net.verdagon.radonc.sculptor.Sculptor
import net.verdagon.radonc.templar._
import org.scalatest.{FunSuite, Matchers}

class CarpenterTests extends FunSuite with Matchers {
  test("Function param has virtual") {
    val compile = new Compilation(
      """
        |interface I {}
        |fn doThing(virtual i: I) {4}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()

    assert(temputs.getAllNonExternFunctions.size == 2)
    assert(temputs.lookupFunction("main").header.returnType == Reference2(Share, Int2()))

    val doThing = temputs.lookupFunction(Signature2("doThing", List(), List(Reference2(Own, InterfaceRef2("I", List()))))).get
    assert(doThing.header.params(0).virtuality.get == Virtual2())
  }

  // There is no doAThing which takes in a MyInterface, so there should be no
  // virtual shenanigans going on.
  test("No virtual/override/abstract means no function families") {
    val compile = new Compilation(
      """
        |interface MyInterface { }
        |
        |struct SomeStruct { }
        |SomeStruct implements MyInterface;
        |fn doAThing(a: SomeStruct) { }
        |
        |struct OtherStruct { }
        |OtherStruct implements MyInterface;
        |fn doAThing(b: OtherStruct) { }
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    assert(temputs.functionFamiliesByRootBanner.isEmpty)
  }

  // Make sure we generate a covariant family for doAThing.
  test("Function with virtual makes a function family") {
    val compile = new Compilation(
      """
        |interface MyInterface {
        |  fn doAThing(virtual i: MyInterface);
        |}
        |
        |struct SomeStruct { }
        |SomeStruct implements MyInterface;
        |fn doAThing(override a: SomeStruct) { }
        |
        |struct OtherStruct { }
        |OtherStruct implements MyInterface;
        |fn doAThing(override b: OtherStruct) { }
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    assert(temputs.functionFamiliesByRootBanner.size == 1)
    val family = temputs.functionFamiliesByRootBanner.values.head
    assert(family.rootBanner.paramTypes(0).referend == InterfaceRef2("MyInterface", List()))

    family.memberSignaturesByVirtualRoots(List(StructRef2("OtherStruct", List()))).paramTypes(0).referend == StructRef2("OtherStruct", List());
    family.memberSignaturesByVirtualRoots(List(StructRef2("SomeStruct", List()))).paramTypes(0).referend == StructRef2("SomeStruct", List());
  }


  test("Unrelated structs don't share function families") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |fn doThing:T(virtual x: MyInterface:T) {}
        |
        |struct MyStruct:T { }
        |MyStruct:T implements MyInterface:T;
        |fn doThing:T(override x: MyStruct:T) {}
        |
        |
        |interface OtherInterface:T { }
        |fn doThing:T(virtual x: OtherInterface:T) {}
        |
        |struct OtherStruct:T { }
        |OtherStruct:T implements OtherInterface:T;
        |fn doThing:T(override x: OtherStruct:T) {}
        |
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  doThing(x);
        |  let y = OtherStruct:Int();
        |  doThing(y);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    // No merging should happen because MyStruct and OtherStruct are completely unrelated
    assert(temputs.functionFamiliesByRootBanner.size == 2)
  }


  test("Function can override for two separate ancestors") {
    val compile = new Compilation(
      """
        |interface I:T { }
        |interface J:T { }
        |
        |struct MyStruct:T { }
        |MyStruct:T implements I:T;
        |MyStruct:T implements J:T;
        |
        |fn doThing:T(virtual x: I:T) {}
        |fn doThing:T(virtual x: J:T) {}
        |fn doThing:T(override x: MyStruct:T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  doThing(x);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    assert(temputs.functionFamiliesByRootBanner.size == 2)
  }


  test("Virtual function is added to overridden families and gets its own family") {
    val compile = new Compilation(
      """
        |interface I:T { }
        |interface J:T { }
        |
        |struct MyStruct:T { }
        |MyStruct:T implements I:T;
        |MyStruct:T implements J:T;
        |
        |fn doThing:T(virtual x: I:T) {}
        |fn doThing:T(virtual x: J:T) {}
        |fn doThing:T(virtual x: MyStruct:T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  doThing(x);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    // same as the previous test, but all three are virtual. should still be merged.
    assert(temputs.functionFamiliesByRootBanner.size == 3)
    assert(hinputs.superFamilyRootBannersByRootBanner.size == 3)
    assert(hinputs.superFamilyRootBannersByRootBanner.values.toSet.size == 2)
  }


  test("Struct is stamped and function families created") {
    val compile = new Compilation(
      """
        |interface I:T { }
        |
        |interface J:T { }
        |J:T implements I:T;
        |
        |interface K:T { }
        |K:T implements J:T;
        |
        |struct MyStruct:T { }
        |MyStruct:T implements K:T;
        |
        |fn doThing:T(virtual x: T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  doThing(x);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()
    val hinputs = compile.getHinputs()
    assert(temputs.functionFamiliesByRootBanner.size == 4)
    assert(hinputs.superFamilyRootBannersByRootBanner.size == 4)
    // This also tests the transitive super families, see SuperFamilyCarpenter
    assert(hinputs.superFamilyRootBannersByRootBanner.values.toSet.size == 1)
  }

  // Make sure it keeps doAThing and doBThing separate, even though they have the same signatures.
  // This tripped up the superfamilycarpenter once when it forgot to compare the names.
  test("Functions with same params but different name dont share families") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |struct MyStruct:T { }
        |MyStruct:T implements MyInterface:T;
        |
        |fn doAThing:T(virtual x: T) {}
        |fn doBThing:T(virtual x: T) {}
        |
        |fn main() {
        |  doAThing(MyStruct:Int());
        |  doBThing(MyStruct:Int());
        |}
      """.stripMargin)
    val hinputs = compile.getHinputs()
    val temputs = compile.getTemputs()
  }

}
