package net.verdagon.radonc.templar

import net.verdagon.radonc.{Compilation}
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.Set

class VirtualTests extends FunSuite with Matchers {

  test("Simple program containing a virtual function") {
    val compile = new Compilation(
      """
        |interface I {}
        |fn doThing(virtual i: I) {4}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.getAllNonExternFunctions.size == 2)
    assert(temputs.lookupFunction("main").header.returnType == Reference2(Share, Int2()))

    val doThing = temputs.lookupFunction(Signature2("doThing", List(), List(Reference2(Own, InterfaceRef2("I", List()))))).get
    assert(doThing.header.params(0).virtuality.get == Virtual2())
  }

  test("Simple program containing a virtual function taking an interface") {
    val compile = new Compilation(
      """
        |interface I {}
        |struct S {}
        |S implements I;
        |fn doThing(virtual i: I) {4}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.getAllNonExternFunctions.size == 3) // including constructor
    assert(temputs.lookupFunction("main").header.returnType == Reference2(Share, Int2()))

    val doThing = temputs.lookupFunction(Signature2("doThing", List(), List(Reference2(Own, InterfaceRef2("I", List()))))).get
    assert(doThing.header.params(0).virtuality.get == Virtual2())
  }

  test("Simple override") {
    val compile = new Compilation(
      """
        |interface I {}
        |struct S {}
        |S implements I;
        |fn doThing(virtual i: I) {4}
        |fn doThing(override s: S) {4}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.getAllNonExternFunctions.size == 4) // including constructor
    assert(temputs.lookupFunction("main").header.returnType == Reference2(Share, Int2()))

    val doThingI = temputs.lookupFunction(Signature2("doThing", List(), List(Reference2(Own, InterfaceRef2("I", List()))))).get
    assert(doThingI.header.params(0).virtuality.get == Virtual2())

    val doThingS = temputs.lookupFunction(Signature2("doThing", List(), List(Reference2(Own, StructRef2("S", List()))))).get
    assert(doThingS.header.params(0).virtuality.get == Override2())

    assert(temputs.functionFamiliesByRootBanner.size == 1)
    assert(temputs.functionFamiliesByRootBanner.head._2.memberSignaturesByVirtualRoots.size == 2)
  }

  test("Two functions overriding another function") {
    val compile = new Compilation(
      """
        |interface I {}
        |struct S1 {}
        |S1 implements I;
        |struct S2 {}
        |S2 implements I;
        |fn doThing(virtual i: I) {4}
        |fn doThing(override s: S1) {5}
        |fn doThing(override s: S2) {6}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.functionFamiliesByRootBanner.size == 1)
    assert(temputs.functionFamiliesByRootBanner.head._2.memberSignaturesByVirtualRoots.size == 3)
  }

  test("Function taking an interface overriding another function") {
    val compile = new Compilation(
      """
        |interface I1 {}
        |interface I2 {}
        |I2 implements I1;
        |struct S {}
        |S implements I2;
        |fn doThing(virtual i: I1) {4}
        |fn doThing(override i: I2) {5}
        |fn doThing(override s: S) {6}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.lookupInterface(InterfaceRef2("I2", List())).superInterfaces.nonEmpty)

    assert(temputs.functionFamiliesByRootBanner.size == 1)

    val doThingI1Root =
      temputs.functionFamiliesByRootBanner.keys.find({
        case FunctionBanner2(_, "doThing", List(), List(Parameter2("i", Some(Virtual2()), Reference2(Own, InterfaceRef2("I1", List()))))) => true
      }).get;
    val doThingI1Family = temputs.functionFamiliesByRootBanner(doThingI1Root);
    assert(doThingI1Family.memberSignaturesByVirtualRoots.size == 3)
  }

  test("Function taking an interface overriding another function with virtual") {
    val compile = new Compilation(
      """
        |interface I1 {}
        |interface I2 {}
        |I2 implements I1;
        |struct S {}
        |S implements I2;
        |fn doThing(virtual i: I1) {4}
        |fn doThing(virtual i: I2) {5}
        |fn doThing(override s: S) {6}
        |fn main() {3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.lookupInterface(InterfaceRef2("I2", List())).superInterfaces.nonEmpty)

    assert(temputs.functionFamiliesByRootBanner.size == 2)

    val doThingI1Root =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(), List(Parameter2("i", Some(Virtual2()), Reference2(Own, InterfaceRef2("I1", List()))))) => b
      }).head;
    val doThingI1Family = temputs.functionFamiliesByRootBanner(doThingI1Root);
    assert(doThingI1Family.memberSignaturesByVirtualRoots.size == 3)

    val doThingI2Root =
      temputs.functionFamiliesByRootBanner.keys.find({
        case FunctionBanner2(_, "doThing", List(), List(Parameter2("i", Some(Virtual2()), Reference2(Own, InterfaceRef2("I2", List()))))) => true
      }).get;
    val doThingI2Family = temputs.functionFamiliesByRootBanner(doThingI2Root);
    assert(doThingI2Family.memberSignaturesByVirtualRoots.size == 2)
  }

  test("Stamps ancestor structs when we declare a child struct") {
    val compile = new Compilation(
      """
        |interface I:T { }
        |fn dance:T(virtual i: I:T) {
        |   print(1);
        |}
        |
        |struct SA:T { }
        |SA:T implements I:T;
        |fn dance:T(override a: SA:T) {
        |   print(2);
        |}
        |
        |fn main() {
        |  dance(SA:Int());
        |}
        |
        |struct SB:T { }
        |SB:T implements I:T;
        |fn dance:T(override b: SB:T) {
        |   print(3);
        |}
        |
        |fn thing() {
        |  let x = SB:Int();
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.functionFamiliesByRootBanner.size == 1)
    val family = temputs.functionFamiliesByRootBanner.head._2;
    assert(family.memberSignaturesByVirtualRoots.size == 3);
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, StructRef2("SA", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, StructRef2("SB", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
  }

  test("Calls an overriding function") {
    val compile = new Compilation(
      """
        |interface I:T { }
        |fn dance:T(virtual i: I:T) {
        |   print(1);
        |}
        |
        |struct SA:T { }
        |SA:T implements I:T;
        |fn dance:T(override a: SA:T) {
        |   print(2);
        |}
        |
        |struct SB:T { }
        |SB:T implements I:T;
        |fn dance:T(override b: SB:T) {
        |   print(3);
        |}
        |
        |fn main() {
        |  let x = SB:Int();
        |  dance(SA:Int());
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    assert(temputs.functionFamiliesByRootBanner.size == 1)
    val family = temputs.functionFamiliesByRootBanner.head._2;
    assert(family.memberSignaturesByVirtualRoots.size == 3);
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, StructRef2("SA", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
    assert(family.memberSignaturesByVirtualRoots.values.toList.contains(
      Signature2("dance", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, StructRef2("SB", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))));
  }

  // When we call a function with a virtual parameter, try stamping for all ancestors in its
  // place.
  // We're stamping all ancestors, and all ancestors have virtual.
  // Virtual starts a function family.
  // So, this checks that it and its three ancestors are all stamped and all get their own
  // function families.
  test("Stamp multiple function families") {
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

    println(temputs.functionFamiliesByRootBanner.size)
    temputs.functionFamiliesByRootBanner.foreach(println)

    assert(temputs.functionFamiliesByRootBanner.size == 4)

    val getParamArgTypeByVirtualRoot =
      (family: FunctionFamily) => {
        family.memberSignaturesByVirtualRoots.collect({
          case (List(virtualRoot: CitizenRef2), Signature2(_, _, List(Reference2(_, param: CitizenRef2)))) => (virtualRoot.humanName -> param.humanName)
        })
      };

    val doThingFamilyRootForI =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForI = temputs.functionFamiliesByRootBanner(doThingFamilyRootForI);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForI) == Map("I" -> "I", "J" -> "J", "K" -> "K", "MyStruct" -> "MyStruct"))

    val doThingFamilyRootForJ =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Own, InterfaceRef2("J", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("J", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForJ = temputs.functionFamiliesByRootBanner(doThingFamilyRootForJ);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForJ) == Map("J" -> "J", "K" -> "K", "MyStruct" -> "MyStruct"))

    val doThingFamilyRootForK =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Own, InterfaceRef2("K", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("K", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForK = temputs.functionFamiliesByRootBanner(doThingFamilyRootForK);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForK) == Map("K" -> "K", "MyStruct" -> "MyStruct"))

    val doThingFamilyRootForMyStruct =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Own, StructRef2("MyStruct", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, StructRef2("MyStruct", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForMyStruct =
      temputs.functionFamiliesByRootBanner(doThingFamilyRootForMyStruct);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForMyStruct) == Map("MyStruct" -> "MyStruct"))
  }

  // We manifest a doThing:Int, which takes in an I:Int.
  // If you thought it should spawn a doThing(:MyStruct:Int), youre wrong; the function
  // specifically says its parameter is an I:T, not a MyStruct:T.
  // There should never be a doThing(:MyStruct:Int), doThing(:K:Int), or doThing(:J:Int).
  // That said, we can still pass a MyStruct:Int arg to an I:Int parameter.
  // This should only spawn that one function family.
  // However, each struct is required to have a vtable entry for that function... they'll all
  // point to the original doThing:Int(:I:Int).
  test("Stamps virtual functions on a templated interface") {
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
        |fn doThing:T(virtual x: I:T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  doThing(x);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    println(temputs.functionFamiliesByRootBanner.size)
    temputs.functionFamiliesByRootBanner.foreach(println)

    assert(temputs.functionFamiliesByRootBanner.size == 1)

    val doThingFamilyRoot =
      temputs.functionFamiliesByRootBanner.keys.find({
        case FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => true
      }).get;
    val doThingFamily =
      temputs.functionFamiliesByRootBanner(doThingFamilyRoot);
    assert(doThingFamily.memberSignaturesByVirtualRoots.size == 4)

    assert(
      doThingFamily.memberSignaturesByVirtualRoots.values.toSet ==
          Set(Signature2("doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))))
  }


  test("Virtual creates function family roots") {
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

    assert(temputs.functionFamiliesByRootBanner.size == 3)
    // See the next test, which does the same thing but with override.
    // Also, even though theres three function families, the SuperFamilyCarpenter
    // will merge the MyStruct:T families into the others.
  }

  // Should only be two function families.
  test("Override doesnt make a function family root") {
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

    assert(temputs.functionFamiliesByRootBanner.size == 2)

    val getParamArgTypeByVirtualRoot =
      (family: FunctionFamily) => {
        family.memberSignaturesByVirtualRoots.collect({
          case (List(virtualRoot: CitizenRef2), Signature2(_, _, List(Reference2(_, param: CitizenRef2)))) => (virtualRoot.humanName -> param.humanName)
        })
      };

    val doThingFamilyRootForI =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("I", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForI = temputs.functionFamiliesByRootBanner(doThingFamilyRootForI);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForI) ==
        Map("I" -> "I", "MyStruct" -> "MyStruct"));

    val doThingFamilyRootForJ =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("J", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForJ = temputs.functionFamiliesByRootBanner(doThingFamilyRootForJ);
    assert(getParamArgTypeByVirtualRoot(doThingFamilyForJ) ==
        Map("J" -> "J", "MyStruct" -> "MyStruct"));
  }


  // These are two completely different types, so shouldnt share a family.
  // There should be two families.
  // This is a case where two functions with the same name+params return
  // something different; theyre only distinguished by their template args.

  test("Functions with same signatures but different template params make different families") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |fn doThing:T(virtual x: MyInterface:T) {}
        |
        |struct MyStruct:T { }
        |MyStruct:T implements MyInterface:T;
        |fn doThing:T(override x: MyStruct:T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  let y = MyStruct:Str();
        |  doThing(x);
        |  doThing(y);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    println(temputs.functionFamiliesByRootBanner);
    assert(temputs.functionFamiliesByRootBanner.size == 2)
  }

  test("Stamps different families for different template args") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |abstract fn doThing:T(virtual x: MyInterface:T)Void;
        |
        |struct MyStruct:T { }
        |MyStruct:T implements MyInterface:T;
        |fn doThing:T(override x: MyStruct:T) {}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  let y = MyStruct:Str();
        |  doThing(x);
        |  doThing(y);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    val doThingFamilyRootForI =
      temputs.functionFamiliesByRootBanner.keys.collect({
        case b@FunctionBanner2(_, "doThing", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("MyInterface", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))))))) => b
      }).head;
    val doThingFamilyForI = temputs.functionFamiliesByRootBanner(doThingFamilyRootForI);
    assert(doThingFamilyForI.memberSignaturesByVirtualRoots.size == 2)

    temputs.functions.collectFirst({
      case Function2(FunctionHeader2("doThing", true, false, List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), List(Parameter2("x", Some(Virtual2()), Reference2(Own, InterfaceRef2("MyInterface", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))))))), Reference2(Share, Void2()), _), _) => {}
    }).get

    assert(temputs.all({
      case f@FunctionHeader2("doThing", _, _, _, _, _, _) => f
    }).size == 4)
  }
}
