package net.verdagon.radonc.vivem
import net.verdagon.radonc.hammer._
import net.verdagon.radonc.templar.{Own, Ownership}

// RR = Runtime Result. Don't use these to determine behavior, just use
// these to check that things are as we expect.
case class RRReference(hamut: Reference3)
case class RRReferend(hamut: Referend3)

class Allocation(
    val objectId: Int, // note that this cannot change
    val referend: ReferendV // note that this cannot change
) {
  var refCount = 0

  def incrementRefCount(): Int = {
    refCount = refCount + 1
    refCount
  }

  def decrementRefCount(): Int = {
    assert(refCount > 0)
    refCount = refCount - 1
    refCount
  }

  override def finalize(): Unit = {
    assert(refCount == 0)
  }

  def unapply(arg: Allocation): Option[ReferendV] = Some(referend)
}

object Allocation {
  def unapply(arg: Allocation): Option[ReferendV] = {
    Some(arg.referend)
  }
}

sealed trait ReferendV {
  def tyype: RRReferend
}
sealed trait PrimitiveReferendV extends ReferendV
case class IntV(value: Int) extends PrimitiveReferendV {
  override def tyype = RRReferend(Int3())
}
case class VoidV() extends PrimitiveReferendV {
  override def tyype = RRReferend(Void3())
}
case class BoolV(value: Boolean) extends PrimitiveReferendV {
  override def tyype = RRReferend(Bool3())
}
case class FloatV(value: Float) extends PrimitiveReferendV {
  override def tyype = RRReferend(Float3())
}
case class StrV(value: String) extends PrimitiveReferendV {
  override def tyype = RRReferend(Str3())
}

case class FunctionReferendV(function: Function3) extends ReferendV {
  override def tyype = RRReferend(function.prototype.functionType)
}

case class ViewV(edge: Edge3, reference: ReferenceV) extends ReferendV {
  override def tyype = RRReferend(edge.interface)
}

case class StructInstanceV(
    struct3: StructDefinition3,
    var members: Vector[MemberV]
) extends ReferendV {
  override def tyype = RRReferend(struct3.getRef())
}

case class ArrayInstanceV(
    elementType3: Reference3,
    private val size: Int,
    private var elements: Vector[ReferenceV]
) extends ReferendV {
  override def tyype = RRReferend(ArrayT3(elementType3))

  // This only checks the initial values, doesn't check anything
  // that comes in afterwards.
  elements.foreach(element => assert(element.tyype.hamut == elementType3))

  def getElement(index: Int) = {
    // Make sure we're initialized
    assert(elements.size == size)
    elements(index)
  }

  def setElement(index: Int, ref: ReferenceV) = {
    // Make sure we're initialized
    assert(elements.size == size)
    elements = elements.updated(index, ref)
  }

  def initializeElement(index: Int, ref: ReferenceV) = {
    // Make sure we're not yet initialized
    assert(elements.size < size)
    // Make sure we're initializing the *next* empty slot
    assert(index == elements.size)
    elements = elements :+ ref
  }

  def getSize() = {
    // Make sure we're initialized
    assert(elements.size == size)
    size
  }
}

sealed trait MemberV
case class VariableAddressMemberV(address: VariableAddressV) extends MemberV
case class ReferenceMemberV(reference: ReferenceV) extends MemberV

case class ReferenceV(tyype: RRReference, objectId: Int)

sealed trait AddressV
case class VariableAddressV(variableId: Int) extends AddressV
case class MemberAddressV(objectId: Int, fieldIndex: Int) extends AddressV
case class ElementAddressV(objectId: Int, elementIndex: Int) extends AddressV

case class VariableV(var reference: ReferenceV, expectedType: Reference3) {
  // Number of VariableAddressMemberV and VariableAddressRegisterV
  // referring to this
  var refCount = 0

  def incrementRefCount(): Int = {
    refCount = refCount + 1
    refCount
  }
  def decrementRefCount(): Int = {
    refCount = refCount - 1
    assert(refCount >= 0)
    refCount
  }
}

sealed trait RegisterV
case class MemberAddressRegisterV(address: MemberAddressV) extends RegisterV
case class ElementAddressRegisterV(address: ElementAddressV) extends RegisterV
case class ReferenceRegisterV(reference: ReferenceV) extends RegisterV
case class VariableAddressRegisterV(address: VariableAddressV) extends RegisterV
