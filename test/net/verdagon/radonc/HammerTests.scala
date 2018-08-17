package net.verdagon.radonc

import net.verdagon.radonc._
import net.verdagon.radonc._
import net.verdagon.radonc._
import net.verdagon.radonc.carpenter.Carpenter
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.hammer._
import net.verdagon.radonc.sculptor.Sculptor
import org.scalatest.{FunSuite, Matchers}

import scala.collection.immutable.ListMap

class HammerTests extends FunSuite with Matchers {
  // Hammer tests only test the general structure of things, not the generated nodes.
  // The generated nodes will be tested by end-to-end tests.

  test("Simple main") {
    val compile = new Compilation(
      "fn main(){3}")
    val hamuts = compile.getHamuts()

    assert(hamuts.nonExternFunctions.size == 1)
    assert(hamuts.nonExternFunctions.head.prototype.humanName == "main");
  }

  // Make sure a ListNode struct made it out
  test("Templated struct makes it into hamuts") {
    val compile = new Compilation(
      """
        |struct ListNode:T {
        |  tail: ListNode:T;
        |}
        |fn main(a: ListNode:Int) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.structs.find(_.humanName == "ListNode").get;
  }

  test("Two templated structs make it into hamuts") {
    val compile = new Compilation(
      """
        |interface MyOption:T { }
        |struct MyNone:T { }
        |MyNone:T implements MyOption:T;
        |struct MySome:T { value: T; }
        |MySome:T implements MyOption:T;
        |
        |fn main(a: MySome:Int, b: MyNone: Int) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.interfaces.find(_.humanName == "MyOption").get;

    val mySome = hamuts.structs.find(_.humanName == "MySome").get;
    assert(mySome.members.size == 1);
    assert(mySome.members.head.tyype.expectReferenceMember().reference == Reference3(Share, Int3()))

    val myNone = hamuts.structs.find(_.humanName == "MyNone").get;
    assert(myNone.members.isEmpty);
  }

  test("Generated etables are size 1") {
    val compile = new Compilation(
      """
        |interface MyOption:T { }
        |struct MyNone:T { }
        |MyNone:T implements MyOption:T;
        |struct MySome:T { value: T; }
        |MySome:T implements MyOption:T;
        |
        |fn main(a: MySome:Int, b: MyNone: Int) {}
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.structs
        .filter(s => s.humanName == "MySome" || s.humanName == "MyNone")
        .foreach(struct => {
          assert(struct.eTable.table.directory.size == 1)
          assert(struct.eTable.table.combinedBuckets.size == 1)
          assert(struct.eTable.table.combinedBuckets(0).get._2 == hamuts.interfaces(0).getRef)
        })
  }

  test("Directory for 4 interfaces is still size 1") {
    val compile = new Compilation(
      """
        |interface Blark {}
        |interface Bloop {}
        |interface Blorg {}
        |interface Blerp {}
        |struct MyStruct {}
        |MyStruct implements Blark;
        |MyStruct implements Bloop;
        |MyStruct implements Blorg;
        |MyStruct implements Blerp;
      """.stripMargin)
    val hamuts = compile.getHamuts()
    val struct = hamuts.structs.find(_.humanName == "MyStruct").get;
    // Our tetris table uses ceil(N/4) directory size, bumped up to the next power of 2. If this doesnt work, perhaps investigate rounding issues...
    assert(struct.eTable.table.directory.size == 1);

    hamuts.interfaces.foreach(interface => {
      assert(struct.eTable.table.combinedBuckets.flatMap(_.toList.map(_._2)).contains(interface.getRef))
    })
  }

  test("Virtual and override functions make it into hamuts") {
    val compile = new Compilation(
      """
        |interface Blark {
        |  fn wot(virtual b: Blark)Int;
        |}
        |struct MyStruct {}
        |MyStruct implements Blark;
        |fn wot(override b: MyStruct)Int { 9 }
      """.stripMargin)
    val hamuts = compile.getHamuts()
    hamuts.nonExternFunctions.find(f => f.prototype.humanName == "wot").get;
    hamuts.nonExternFunctions.find(f => f.prototype.humanName == "MyStruct").get;
    assert(hamuts.abstractFunctions.size == 1)
    assert(hamuts.implementedFunctions.size == 2)
    assert(hamuts.nonExternFunctions.size == 3)
  }

}
