package net.verdagon.radonc.templar

import net.verdagon.radonc.{CommonEnv, Compilation}
import net.verdagon.radonc.scout.{Immutable, Mutable, Program1, Scout}
import net.verdagon.radonc.vivem._
import org.scalatest.{FunSuite, Matchers, _}

class TemplarTests extends FunSuite with Matchers {
  test("Simple program returning an int") {
    val compile = new Compilation("fn main(){3}")
    val temputs = compile.getTemputs()
    temputs.only({
      case FunctionHeader2("main", false, false, List(), List(), Reference2(Share, Int2()), _) => true
    })
    temputs.only({ case IntLiteral2(3) => true })

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }

  test("Hardcoding negative numbers") {
    val compile = new Compilation("fn main(){-3}")
    compile.getTemputs().only({ case IntLiteral2(-3) => true })
    compile.evalForReferend(Vector()) shouldEqual IntV(-3)
  }

  test("Taking an argument and returning it") {
    val compile = new Compilation("fn main(a:Int){a}")
    val temputs = compile.getTemputs()
    temputs.lookupFunction("main").onlyOf(classOf[Parameter2]).tyype == Reference2(Share, Int2())
    temputs.lookupFunction("main").onlyOf(classOf[SoftLoad2]);
    val lookup = temputs.lookupFunction("main").onlyOf(classOf[LocalLookup2]);
    lookup.name shouldBe "a"
    lookup.reference shouldBe Reference2(Share, Int2())
    compile.evalForReferend(Vector(IntV(5))) shouldEqual IntV(5)
  }

  test("Tests adding two numbers") {
    val compile = new Compilation("fn main(){ +(2, 3) }")
    val temputs = compile.getTemputs()
    temputs.only({ case IntLiteral2(2) => true })
    temputs.only({ case IntLiteral2(3) => true })
    temputs.only({
      case FunctionPointerCall2(
        FunctionLookup2(
          Prototype2("__addIntInt", _, _)),
            List(
              IntLiteral2(2),
              IntLiteral2(3))) =>
    })
    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Tests inline adding") {
    val compile = new Compilation("fn main(){ 2 + 3 }")
    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }

  test("Simple lambda") {
    val compile = new Compilation("fn main(){{7}()}")
    val temputs = compile.getTemputs()

    // Make sure it inferred the param type and return type correctly
    val lambda = temputs.functions.find(_.header.humanName == "main:lam1").get;
    temputs.getAllNonExternFunctions.foreach(p => {
      assert(p.header.returnType == Reference2(Share, Int2()))
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(7)
  }

  test("Lambda with one magic arg") {
    val compile = new Compilation("fn main(){{_}(3)}")
    val temputs = compile.getTemputs()

    // Make sure it inferred the param type and return type correctly
    val lambda = temputs.functions.find(_.header.humanName == "main:lam1").get;
    lambda.only({ case Parameter2(_, None, Reference2(Share, Int2())) => {} })

    temputs.getAllNonExternFunctions.foreach(p => {
      assert(p.header.returnType == Reference2(Share, Int2()))
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }


  // Test that the lambda's arg is the right type, and the name is right
  test("Lambda with a type specified param") {
    val compile = new Compilation("fn main(){{(a:Int) +(a,a)}(3)}");
    val temputs = compile.getTemputs()

    val functions = temputs.allOf(classOf[Function2]);

    val lambda = functions.find(_.header.humanName == "main:lam1").get;

    // Check that the param type is right
    lambda.only({ case Parameter2("a", None, Reference2(Share, Int2())) => {} })
    // Check the name is right
    assert(lambda.header.humanName == "main:lam1");

    val main = functions.find(_.header.humanName == "main").get;
    assert(main.onlyOf(classOf[FunctionLookup2]).prototype.humanName == "main:lam1");

    compile.evalForReferend(Vector()) shouldEqual IntV(6)
  }

  test("Test overloads") {
    val compile = new Compilation(
      """
        |fn ~(a:Int, b:Int){+(a, b)}
        |fn ~(a:Str, b:Str){+(a, b)}
        |fn main(){3 ~ 3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Tests that we use the right overload, and both the overloads made it into temputs
    assert(temputs.getAllNonExternFunctions.size == 3)
    assert(
      temputs.getAllNonExternFunctions.find(_.header.humanName == "main").get.header.returnType ==
          Reference2(Share, Int2()))

    compile.evalForReferend(Vector()) shouldEqual IntV(6)
  }

  test("Test block") {
    val compile = new Compilation("fn main(){true; 200; 300}")
    compile.evalForReferend(Vector()) shouldEqual IntV(300)
  }

  test("Test templates") {
    val compile = new Compilation(
      """
        |fn ~:T(a:T, b:T)T{a}
        |fn main(){true ~ false; 2 ~ 2; 3 ~ 3}
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Tests that we reuse existing stamps
    assert(temputs.getAllNonExternFunctions.size == 3)

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }

  test("Test mutating a local var") {
    val compile = new Compilation("fn main(){let mut a = 3; mut a = 4; }")
    val temputs = compile.getTemputs();
    temputs.only({ case Mutate2(LocalLookup2("a", _), IntLiteral2(4)) => })
    compile.evalForReferend(Vector()) match { case StructInstanceV(_, Vector()) => }
  }

  test("Test returning a local mutable var") {
    val compile = new Compilation("fn main(){let mut a = 3; mut a = 4; a}")
    compile.evalForReferend(Vector()) shouldEqual IntV(4)
  }

  test("Test taking a callable param") {
    val compile = new Compilation(
      """
        |fn do(callable) {callable()}
        |fn main() {do({ 3 })}
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Tests that we use the right overload, and both the overloads made it into temputs
    temputs.functions.find(_.header.humanName == "do").get.header.returnType == Reference2(Share, Int2())

    compile.evalForReferend(Vector()) shouldEqual IntV(3)
  }

  test("Test matching a single-member pack") {
    val compile = new Compilation("fn main() { let (x) = (4); x }")
    val temputs = compile.getTemputs()
    temputs.only({ case Let2(_, false, IntLiteral2(4)) => })
    temputs.only({ case Let2("x", false, _) => })
    compile.evalForReferend(Vector()) shouldEqual IntV(4)
  }

  test("Test matching a multiple-member pack") {
    // Checks that the 5 made it into y, and it was an int
    val compile = new Compilation("fn main() { let (x, y) = (4, 5); y }")
    val temputs = compile.getTemputs()
    temputs.functions.head.header.returnType == Reference2(Share, Int2())
    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }


  test("Test returning a nonmutable closured variable from the closure") {
    val compile = new Compilation("fn main() { let x = 4; {x}() }")
    val temputs = compile.getTemputs()
    // The struct should have an int x in it.
    val closuredVarsStruct = temputs.structs.find(_.humanName == "__Closure:main:lam2").get;
    val expectedMembers = List(StructMember2("x", AddressMemberType2(Reference2(Share, Int2()))));
    assert(closuredVarsStruct.members == expectedMembers)

    // Make sure there's a function that takes in the closured vars struct, and returns an int
    temputs.only({
      case Function2(
      FunctionHeader2(
      "main:lam2",
      _,
      _,
      List(),
      List(Parameter2(_, None, Reference2(Share, StructRef2("__Closure:main:lam2", List())))),
      Reference2(Share, Int2()),
      _),
      _) =>
    })

    // Make sure we make it with a function pointer and a constructed vars struct
    val main = temputs.functions.find(_.header.humanName == "main").get;
    main.only({
      case Construct2(StructRef2("__Closure:main:lam2", List()), _, _) =>
    })

    // Make sure we call the function somewhere
    main.onlyOf(classOf[FunctionPointerCall2])


    val lambda = temputs.functions.find(_.header.humanName == "main:lam2").get;

    println(lambda.allOf(classOf[LocalLookup2]))
    lambda.only({
      case LocalLookup2("__Closure", _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(4)
  }

  test("Mutates from inside a closure") {
    val compile = new Compilation("fn main() { let mut x = 4; { mut x = x + 1 }(); x }")
    val temputs = compile.getTemputs()

    temputs.only({
      case Mutate2(
      AddressMemberLookup2(
      "x", _, Reference2(Share, Int2())),
      _) =>
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(5)
  }


  test("Stamps an interface template via a function parameter") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |fn doAThing:T(i: MyInterface:T) { }
        |
        |struct SomeStruct:T { }
        |fn doAThing:T(s: SomeStruct:T) { }
        |SomeStruct:T implements MyInterface:T;
        |
        |fn main(a: SomeStruct:Int) {
        |  doAThing:Int(a);
        |}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    val heap = new Heap()
    val ref =
      heap.add(Own, StructInstanceV(
        hamuts.structs.find(_.humanName == "SomeStruct").get,
        Vector()))
    compile.evalForReferend(heap, Vector(ref))
  }

  test("Reads a struct member") {
    val compile = new Compilation(
      """
        |struct MyStruct { a: Int; }
        |fn main() { let ms = MyStruct(7); ms.a }
      """.stripMargin)
    val temputs = compile.getTemputs()

    // Check the struct was made
    temputs.only({
      case StructDefinition2(
      "MyStruct",
      Mutable,
      List(),
      List(),
      List(StructMember2("a", ReferenceMemberType2(Reference2(Share, Int2()))))) =>
    })
    // Check there's a constructor
    temputs.only({
      case FunctionHeader2(
      "MyStruct",
      _,
      _,
      List(),
      List(Parameter2("a", None, Reference2(Share, Int2()))),
      Reference2(Own, StructRef2("MyStruct", List())),
      _) =>
    })
    // Check that we call the constructor
    temputs.only({
      case FunctionPointerCall2(
      FunctionLookup2(Prototype2("MyStruct", _, _)),
      List(IntLiteral2(7))) =>
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(7)
  }

  test("Tests defining an interface and an implementing struct") {
    val compile = new Compilation(
      """
        |interface MyInterface { }
        |struct MyStruct { }
        |MyStruct implements MyInterface;
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.only({
      case InterfaceDefinition2("MyInterface", Mutable, List(), _) =>
    })
    temputs.only({
      case StructDefinition2(
      "MyStruct",
      Mutable,
      List(),
      List(InterfaceRef2("MyInterface", List())),
      _) =>
    })
  }

  test("Tests stamping an interface template from a function param") {
    val compile = new Compilation(
      """
        |interface MyOption:T { }
        |fn main(a: MyOption:Int) { }
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.interfaces match {
      case List(InterfaceDefinition2("MyOption", Mutable, List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), _)) =>
    }
    assert(temputs.getAllNonExternFunctions.head.header.params.head.tyype ==
        Reference2(Own,InterfaceRef2("MyOption",List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))))))

    // Can't run it because there's nothing implementing that interface >_>
  }

  test("Tests stamping a struct and its implemented interface from a function param") {
    val compile = new Compilation(
      """
        |interface MyOption imm :T { }
        |struct MySome imm :T { value: T; }
        |MySome:T implements MyOption:T;
        |fn main(a: MySome:Int) { }
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.only({
      case InterfaceDefinition2("MyOption", Immutable, List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))), _) =>
    })
    val struct = temputs.lookupStruct(StructRef2("MySome", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))));
    assert(struct.superInterfaces == List(InterfaceRef2("MyOption", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))))));

    val hamuts = compile.getHamuts()

    val heap = new Heap()
    val intRef = heap.add(Share, IntV(5))
    heap.incrementReferenceRefCount(intRef)
    val structRef =
      heap.add(Share, StructInstanceV(
        hamuts.structs.find(_.humanName == "MySome").get,
        Vector(ReferenceMemberV(intRef))))
    compile.evalForReferend(heap, Vector(structRef))
  }

  test("Tests calling a struct's constructor") {
    val compile = new Compilation(
      """
        |struct MySome:T { value: T; }
        |fn main() {
        |  MySome:Int(4)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.lookupStruct(StructRef2("MySome", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2()))))));

    temputs.only({
      case
        FunctionHeader2(
        "MySome",
        _,
        _,
        _,
        _,
        Reference2(Own, StructRef2("MySome", List(CoercedTemplateArg2(ReferenceTemplata(Reference2(Share, Int2())))))),
        _) =>
    })

    temputs.onlyOf(classOf[FunctionPointerCall2])

    compile.evalForReferend(Vector())
  }

  test("Tests upcasting from a struct to an interface") {
    val compile = new Compilation(
      """
        |interface MyInterface { }
        |struct MyStruct { value: Int; }
        |MyStruct implements MyInterface;
        |fn main() {
        |  let x: MyInterface = MyStruct(9);
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    val upcast = temputs.onlyOf(classOf[StructToInterfaceUpcast2])
    assert(upcast.resultRegister.reference == Reference2(Own, InterfaceRef2("MyInterface", List())))
    assert(upcast.innerExpr.resultRegister.reference == Reference2(Own, StructRef2("MyStruct", List())))

    val hamuts = compile.getHamuts()
    println("bork")
    hamuts.lookupFunction("main").nodes.foreach(println)

    compile.evalForReferend(Vector())
  }

  test("Tests calling a virtual function") {
    val compile = new Compilation(
      """
        |interface Car {
        |	fn doCivicDance(virtual this: Car)Int;
        |}
        |
        |struct Civic {}
        |Civic implements Car;
        |fn doCivicDance(override civic: Civic)Int {
        |	4
        |}
        |
        |struct Toyota {}
        |Toyota implements Car;
        |fn doCivicDance(override toyota: Toyota)Int {
        |	7
        |}
        |
        |fn main()Int {
        |	let x: Car = Toyota();
        |	doCivicDance(x)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.only({
      case up @ StructToInterfaceUpcast2(innerExpr, InterfaceRef2("Car", List())) => {
        innerExpr.resultRegister.only({
          case StructRef2("Toyota", List()) =>
        })
        assert(up.resultRegister.reference.referend == InterfaceRef2("Car", List()))
      }
    })

    compile.evalForReferend(Vector()) shouldEqual IntV(7)
  }

  test("Tests making a variable with a pattern") {
    // Tests putting MyOption:Int as the type of x.
    val compile = new Compilation(
      """
        |interface MyOption:T { }
        |
        |struct MySome:T {}
        |MySome:T implements MyOption:T;
        |
        |fn doSomething(opt: MyOption:Int) Int {
        |  9
        |}
        |
        |fn main()Int {
        |	let x: MyOption:Int = MySome:Int();
        |	doSomething(x)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    compile.evalForReferend(Vector()) shouldEqual IntV(9)
  }

  test("Tests a linked list") {
    val compile = new Compilation(
      """
        |// expected output begin
        |// 77
        |// 10
        |// expected output end
        |
        |interface MyOption imm :T { }
        |
        |struct MySome imm :T {
        |  value: T;
        |}
        |MySome:T implements MyOption:T;
        |
        |struct MyNone imm :T { }
        |MyNone:T implements MyOption:T;
        |
        |
        |struct MyList imm :T {
        |  value: T;
        |  next: MyOption:MyList:T;
        |}
        |
        |fn printValues(list: MyList:Int) {
        |	 print(list.value);
        |	 printNextValue(list.next);
        |}
        |
        |fn printNextValue(virtual opt: MyOption:MyList:Int) { }
        |fn printNextValue(override opt: MyNone:MyList:Int) { }
        |fn printNextValue(override opt: MySome:MyList:Int) {
        |	 printValues(opt.value);
        |}
        |
        |
        |fn main()Int {
        | 	let list = MyList:Int(10, MySome:MyList:Int(MyList:Int(20, MySome:MyList:Int(MyList:Int(30, MyNone:MyList:Int())))));
        | 	printValues(list);
        | 	0
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    compile.evalForReferend(Vector())
  }

  test("Tests calling an abstract function") {
    val compile = new Compilation(
      """
        |interface MyInterface:T { }
        |abstract fn doThing:T(virtual x: MyInterface:T)Int;
        |
        |struct MyStruct:T { }
        |MyStruct:T implements MyInterface:T;
        |fn doThing:T(override x: MyStruct:T)Int {4}
        |
        |fn main() {
        |  let x = MyStruct:Int();
        |  let y = MyStruct:Str();
        |  doThing(x);
        |  doThing(y)
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    temputs.getAllNonExternFunctions.collectFirst({
      case Function2(FunctionHeader2(_, true, _, _, _, _, _), _) => true
    }).get

    compile.evalForReferend(Vector()) shouldEqual IntV(4)
  }

  test("Tests a foreach for a linked list") {
    val compile = new Compilation(
      """
        |interface MyOption:T { }
        |
        |struct MySome:T {
        |  value: T;
        |}
        |MySome:T implements MyOption:T;
        |
        |struct MyNone:T { }
        |MyNone:T implements MyOption:T;
        |
        |
        |struct MyList:T {
        |  value: T;
        |  next: MyOption:MyList:T;
        |}
        |
        |abstract fn forEach:(F, T)(virtual opt: MyOption:MyList:T, func: F)Int;
        |fn forEach:(F, T)(override opt: MyNone:MyList:T, func: F) Int { 0 }
        |fn forEach:(F, T)(override opt: MySome:MyList:T, func: F) Int {
        |   forEach:(F, T)(opt.value, func);
        |   0
        |}
        |fn forEach:(F, T)(list: MyList:T, func: F) Int {
        |  func(list.value);
        |  forEach:(F, T)(list.next, func);
        |  0
        |}
        |
        |fn main()Int {
        |  let list = MyList:Int(10, MySome:MyList:Int(MyList:Int(20, MySome:MyList:Int(MyList:Int(30, MyNone:MyList:Int())))));
        |  forEach(list, print);
        |  0
        |}
      """.stripMargin)
    val temputs = compile.getTemputs()

    compile.evalForStdout(Vector()) shouldEqual "102030"
  }
}
