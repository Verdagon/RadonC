package net.verdagon.radonc.templar

import net.verdagon.radonc.scout._

//
//sealed trait Value2 extends Queriable2 { // someday, extends BaseType2?
//  def order: Int;
//}

sealed trait Ownership extends Queriable2 {
  def order: Int;
}
case object Own extends Ownership {
  override def order: Int = 2;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case object Borrow extends Ownership {
  override def order: Int = 3;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case object Share extends Ownership {
  override def order: Int = 4;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case object Raw extends Ownership {
  override def order: Int = 5;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class Reference2(ownership: Ownership, referend: Referend2) extends Queriable2 {
  referend match {
    case Int2() | Bool2() | Str2() | Void2() | Float2() | Any2() | Nothing2() => {
      assert(ownership == Share)
    }
    case _ =>
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ ownership.all(func) ++ referend.all(func)
  }
}
sealed trait Referend2 extends Queriable2 {
  def order: Int;
}

// like Scala's Nothing
case class Nothing2() extends Referend2 {
  override def order: Int = 6;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

// like Scala's Any
case class Any2() extends Referend2 {
  override def order: Int = 7;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

// Mostly for interoperability with extern functions
case class Void2() extends Referend2 {
  override def order: Int = 16;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

case class Int2() extends Referend2 {
  override def order: Int = 8;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

case class Bool2() extends Referend2 {
  override def order: Int = 9;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

case class Str2() extends Referend2 {
  override def order: Int = 10;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

case class Float2() extends Referend2 {
  override def order: Int = 11;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = List(this).collect(func)
}

case class PackT2(members: List[Reference2], underlyingStruct: StructRef2) extends Referend2 {
  override def order: Int = 21;

  underlyingStruct.all({
    case AddressMemberType2(_) => throw new RuntimeException("Packs' underlying structs cant have addressibles in them!")
  })

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ members.flatMap(_.all(func)) ++ underlyingStruct.all(func)
  }
}

case class TupleT2(members: List[Reference2], underlyingStruct: StructRef2) extends Referend2 {
  override def order: Int = 20;

  underlyingStruct.all({
    case AddressMemberType2(_) => throw new RuntimeException("Tuples' underlying structs cant have addressibles in them!")
  })

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ members.flatMap(_.all(func)) ++ underlyingStruct.all(func)
  }
}

case class ArrayT2(memberType: Reference2, mutability: Mutability) extends Referend2 {
  override def order: Int = 19;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ memberType.all(func)
  }
}

case class ArraySequenceT2(size: Int, array: ArrayT2) extends Referend2 {
  override def order: Int = 12;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ array.all(func)
  }
}

case class FunctionT2(paramTypes: List[Reference2], returnType: Reference2) extends Referend2 {
  override def order: Int = 13;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ paramTypes.flatMap(_.all(func)) ++ returnType.all(func)
  }
}

case class StructMember2(name: String, tyype: IMemberType2) extends Queriable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ tyype.all(func)
  }
}

sealed trait IMemberType2 extends Queriable2 {
  def reference: Reference2

  def expectReferenceMember(): ReferenceMemberType2 = {
    this match {
      case r @ ReferenceMemberType2(_) => r
      case a @ AddressMemberType2(_) => throw new RuntimeException("Expected reference member, was address member!")
    }
  }
  def expectAddressMember(): AddressMemberType2 = {
    this match {
      case r @ ReferenceMemberType2(_) => throw new RuntimeException("Expected reference member, was address member!")
      case a @ AddressMemberType2(_) => a
    }
  }
}
case class AddressMemberType2(reference: Reference2) extends IMemberType2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
case class ReferenceMemberType2(reference: Reference2) extends IMemberType2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}

trait CitizenDefinition2 {
  def getRef: CitizenRef2;
  def mutability: Mutability;
  def getRootInterfaces(program2: Program2): List[InterfaceRef2];
  def getAncestorCitizens(program2: Program2, includeSelf: Boolean): List[CitizenRef2];
  def getAncestorInterfacesNotIncludingSelf(program2: Program2): List[InterfaceRef2];

  def superInterfaces: List[InterfaceRef2]
  def implementsInterface(program2: Program2, interfaceRef2: InterfaceRef2): Boolean;
  private def getChildDefs(program2: Program2): Set[CitizenDefinition2] = {
    program2.getAllCitizens.filter(_.superInterfaces.contains(this.getRef))
  }
  def getDescendants(program2: Program2, includeSelf: Boolean): Set[CitizenRef2] = {
    (if (includeSelf) Set(this.getRef) else Set[CitizenRef2]()) ++
        getChildDefs(program2).flatMap(_.getDescendants(program2, includeSelf = true))
  }

  def inherits(program2: Program2, needle: CitizenRef2): Boolean = {
    measureDistance(program2, needle).nonEmpty
  }

  def measureDistance(program2: Program2, needle: CitizenRef2): Option[Int] = {
    if (getRef == needle) {
      Some(0)
    } else {
      val distancesThroughParents =
        superInterfaces.map(program2.lookupCitizen).map(_.measureDistance(program2, needle))
      val nope: Option[Int] = None
      val bestDistance =
        distancesThroughParents.foldLeft(nope)({
          case (None, None) => None
          case (None, Some(distance)) => Some(distance)
          case (Some(distance), None) => Some(distance)
          case (Some(distanceA), Some(distanceB)) => Some(Math.min(distanceA, distanceB))
        })
      bestDistance.map(_ + 1)
    }
  }
}

// This is basically a wrapper for an ITemplata. Wrapping an ITemplata in one of these
// is a promise that the contents were coerced to match the expected templata param type.
// Only TypeTemplar.coerceTemplateArg can instantiate these.
case class CoercedTemplateArg2(templata: ITemplata) extends Queriable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templata.all(func)
  }
}

// We include templateArgTypes to aid in looking this up... same reason we have name
case class StructDefinition2(
    humanName: String,
    mutability: Mutability,
    templateArgs: List[CoercedTemplateArg2],
    superInterfaces: List[InterfaceRef2],
    members: List[StructMember2]) extends CitizenDefinition2 with Queriable2 {

  all({
    case StructMember2(_, ReferenceMemberType2(Reference2(_, PackT2(_, _)))) => {
      throw new RuntimeException("Structs can't have packs in them!")
    }
  })

  override def getRef(): StructRef2 = StructRef2(humanName, templateArgs)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func)) ++ superInterfaces.flatMap(_.all(func)) ++ members.flatMap(_.all(func))
  }

  def getMember(memberName: String): StructMember2 = {
    members.find(p => p.name.equals(memberName)) match {
      case None => throw new RuntimeException("wat")
      case Some(member) => member
    }
  }

  private def getIndex(memberName: String): Int = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => throw new RuntimeException("wat")
      case Some((member, index)) => index
    }
  }

  private def getMemberAndIndex(memberName: String): (StructMember2, Int) = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => throw new RuntimeException("wat")
      case Some((member, index)) => (member, index)
    }
  }

  override def getRootInterfaces(program2: Program2): List[InterfaceRef2] = {
    superInterfaces.map(program2.lookupInterface).flatMap(_.getRootInterfaces(program2))
  }

  override def getAncestorCitizens(program2: Program2, includeSelf: Boolean): List[CitizenRef2] = {
    (if (includeSelf) List(getRef) else List()) ++
    getAncestorInterfacesNotIncludingSelf(program2)
  }

  override def getAncestorInterfacesNotIncludingSelf(program2: Program2): List[InterfaceRef2] = {
    superInterfaces.map(program2.lookupInterface).flatMap(_.getAncestorInterfacesIncludingSelf(program2))
  }

  override def implementsInterface(program2: Program2, needleInterfaceRef2: InterfaceRef2): Boolean = {
    superInterfaces.exists(superInterface => {
      program2.lookupInterface(superInterface).implementsInterface(program2, needleInterfaceRef2)
    })
  }
}

case class InterfaceDefinition2(
    humanName: String,
    mutability: Mutability,
    templateArgs: List[CoercedTemplateArg2],
    superInterfaces: List[InterfaceRef2]) extends CitizenDefinition2 with Queriable2 {
  def getRef(): InterfaceRef2 = InterfaceRef2(humanName, templateArgs)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func)) ++ superInterfaces.flatMap(_.all(func))
  }

  override def getRootInterfaces(program2: Program2): List[InterfaceRef2] = {
    if (superInterfaces.nonEmpty) {
      superInterfaces.map(program2.lookupInterface).flatMap(_.getRootInterfaces(program2))
    } else {
      List(getRef)
    }
  }

  override def getAncestorCitizens(program2: Program2, includeSelf: Boolean): List[CitizenRef2] = {
    (if (includeSelf) List(getRef) else List()) ++
    getAncestorInterfacesNotIncludingSelf(program2)
  }

  def getAncestorInterfacesIncludingSelf(program2: Program2): List[InterfaceRef2] = {
    getRef :: getAncestorInterfacesNotIncludingSelf(program2)
  }

  override def getAncestorInterfacesNotIncludingSelf(program2: Program2): List[InterfaceRef2] = {
    val ancestorInterfaces = superInterfaces.map(program2.lookupInterface).flatMap(_.getAncestorInterfacesIncludingSelf(program2));
    // .toSeq.toList doesnt preserve order
    // .distinct doesnt define whether the first or last gets removed, we want last to be removed
    val deduped = ancestorInterfaces.foldLeft(List[InterfaceRef2]())({
      case (previous, current) if previous.contains(current) => previous
      case (previous, current) => previous :+ current
    })
    deduped
  }

  override def implementsInterface(program2: Program2, needleInterfaceRef2: InterfaceRef2): Boolean = {
    needleInterfaceRef2 == getRef ||
        superInterfaces.exists(superInterface => {
          program2.lookupInterface(superInterface).implementsInterface(program2, needleInterfaceRef2)
        })
  }
}

trait CitizenRef2 extends Referend2 {
  def humanName: String;
  def templateArgs: List[CoercedTemplateArg2];
}

// These should only be made by struct templar, which puts the definition into temputs at the same time
case class StructRef2(humanName: String, templateArgs: List[CoercedTemplateArg2]) extends CitizenRef2 {
  override def order: Int = 14;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func))
  }
}

case class TemplatedClosure2(
    structRef: StructRef2,
    terry: TemplataFunctionTerry) extends Referend2 {
  override def order: Int = 18;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structRef.all(func)
  }
}

// Lowers to an empty struct.
// This can one day be removed, instead of making one of these we can make
// a TemplatedClosure2 which perfect forwards.
// doThing(map) can become doThing({(args...) map(...args...)})
case class GlobalFunctionGroup2(
    understructRef2: StructRef2,
    name: String,
    alreadySpecifiedTemplateArgs: List[ITemplata]) extends Referend2 {
  override def order: Int = 19;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class OrdinaryClosure2(
    structRef: StructRef2,
    prototype2: Prototype2) extends Referend2 {
  override def order: Int = 17;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ structRef.all(func)
  }
}

case class InterfaceRef2(humanName: String, templateArgs: List[CoercedTemplateArg2]) extends CitizenRef2 with Queriable2 {
  override def order: Int = 15;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ templateArgs.flatMap(_.all(func))
  }
}
//
//case class TraitInfoRef(fullName: String, methods: List[Prototype3])
//case class getRef()(fullName: String)
//case class EdgeTableRef(traitRef: TraitInfoRef, structRef: getRef())
//
//case class TraitInfo(name: String, parentTraits: List[TraitInfoRef])
//case class StructInfo(name: String, parent: Option[getRef()], implementedTraits: Map[TraitInfo, EdgeTableRef])
//// Order of methods is specified by trait.methods
//case class EdgeTable(structRef: getRef(), traitRef: TraitInfoRef, methods: Map[Prototype3, String])

object ReferenceComparator extends Ordering[Reference2] {
  override def compare(a: Reference2, b: Reference2): Int = {
    val orderDiff = a.ownership.order compare b.ownership.order;
    if (orderDiff != 0) {
      orderDiff
    } else {
      ReferendComparator.compare(a.referend, b.referend)
    }
  }
}

object ReferendComparator extends Ordering[Referend2] {
  override def compare(a: Referend2, b: Referend2): Int = {
    val orderDiff = a.order compare b.order;
    if (orderDiff != 0) {
      orderDiff
    } else {
      a match {
        case Int2() => 0
        case Bool2() => 0
        case Str2() => 0
        case PackT2(innerTypes, underlyingStruct) => compare(underlyingStruct, b.asInstanceOf[PackT2].underlyingStruct)
        case FunctionT2(paramTypes, returnType) => {
          val diff = ReferenceListComparator.compare(paramTypes, b.asInstanceOf[FunctionT2].paramTypes);
          if (diff != 0)
            return diff;
          ReferenceComparator.compare(returnType, b.asInstanceOf[FunctionT2].returnType)
        }
        case StructRef2(thisName, thisTemplateArgs) => {
          val StructRef2(thatName, thatTemplateArgs) = b.asInstanceOf[StructRef2];
          if (thisName != thatName) {
            return thisName.compare(thatName);
          }
          TemplataTypeListComparator.compare(thisTemplateArgs, thatTemplateArgs)
        }
        case _ => throw new RuntimeException("wat " + a)
      }
    }
  }
}

object TemplataTypeComparator extends Ordering[ITemplata] {
  override def compare(a: ITemplata, b: ITemplata):Int = {
    if (a.order != b.order) {
      Math.signum(a.order - b.order).toInt
    } else {
      (a, b) match {
        case (StructTemplateTemplata(struct1A), StructTemplateTemplata(struct1B)) => {
          Math.signum(struct1A.struct1Id - struct1B.struct1Id).toInt
        }
        case (InterfaceTemplateTemplata(interface1A), InterfaceTemplateTemplata(interface1B)) => {
          Math.signum(interface1A.interface1Id - interface1B.interface1Id).toInt
        }
      }
    }
  }
}

object ReferenceListComparator extends Ordering[List[Reference2]] {
  override def compare(a: List[Reference2], b: List[Reference2]):Int = {
    if (a.length == 0) {
      if (b.length == 0) {
        0
      } else {
        -1
      }
    } else {
      if (b.length == 0) {
        1
      } else {
        val firstDiff = ReferenceComparator.compare(a.head, b.head);
        if (firstDiff != 0) {
          firstDiff
        } else {
          compare(a.tail, b.tail)
        }
      }
    }
  }
}

object TemplataTypeListComparator extends Ordering[List[CoercedTemplateArg2]] {
  override def compare(a: List[CoercedTemplateArg2], b: List[CoercedTemplateArg2]):Int = {
    if (a.length == 0) {
      if (b.length == 0) {
        0
      } else {
        -1
      }
    } else {
      if (b.length == 0) {
        1
      } else {
        val firstDiff = TemplataTypeComparator.compare(a.head.templata, b.head.templata);
        if (firstDiff != 0) {
          firstDiff
        } else {
          compare(a.tail, b.tail)
        }
      }
    }
  }
}