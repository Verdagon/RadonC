package net.verdagon.radonc.vivem

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.Mutability
import net.verdagon.radonc.templar.Ownership

import scala.collection.immutable.Stack
import scala.collection.mutable

// Just keeps track of all active objects
class Heap {
  private var objectsById = Map[Int, Allocation]()

  private var localsById = Map[Int, VariableV]()
  private var locals = Stack[VariableV]()

  def addLocal(reference: ReferenceV, expectedType: Reference3): Int = {
    val variable = VariableV(reference, expectedType)
    incrementReferenceRefCount(reference)
    locals = locals.push(variable)
    val id = newId()
    localsById = localsById.updated(id, variable)
    id
  }

  def removeLocal(variableId: Int) = {
    val variable = localsById(variableId)
    assert(locals.top == variable)
    decrementReferenceRefCount(variable.reference)
    locals = locals.pop
    localsById = localsById - variableId
  }

  def mutate(address: AddressV, reference: ReferenceV, expectedType: Reference3) = {
    address match {
      case VariableAddressV(variableId) => {
        val variable = localsById(variableId)
        TypeVivem.checkReference(this, variable.expectedType, reference)
        assert(variable.expectedType == expectedType)
        decrementReferenceRefCount(variable.reference)
        variable.reference = reference
        incrementReferenceRefCount(reference)
      }
      case MemberAddressV(objectId, fieldIndex) => {
        objectsById(objectId).referend match {
          case si @ StructInstanceV(structDef3, members) => {
            members(fieldIndex) match {
              case VariableAddressMemberV(_) => {
                // Can't happen, we don't mutate these directly, we get the variable address
                // out of it and then mutate through that.
                throw new RuntimeException("wat")
              }
              case ReferenceMemberV(actualReference) => {
                decrementReferenceRefCount(actualReference)
              }
            }
            structDef3.members(fieldIndex).tyype match {
              case AddressMemberType3(_) => {
                // Can't happen, we don't mutate these directly, we get the variable address
                // out of it and then mutate through that.
                throw new RuntimeException("wat")
              }
              case ReferenceMemberType3(ref3) => {
                assert(ref3 == expectedType)
              }
            }
            si.members = si.members.updated(fieldIndex, ReferenceMemberV(reference))
            incrementReferenceRefCount(reference)
          }
        }
      }
    }
  }

  def dereferenceAddress(address: AddressV, expectedType: Reference3): ReferenceV = {
    address match {
      case VariableAddressV(variableId) => {
        val actualReference = localsById(variableId).reference
        TypeVivem.checkReference(this, expectedType, actualReference)
        actualReference
      }
      case MemberAddressV(objectId, fieldIndex) => {
        objectsById(objectId).referend match {
          case StructInstanceV(_, members) => {
            members(fieldIndex) match {
              case VariableAddressMemberV(address) => throw new RuntimeException("wat")
              case ReferenceMemberV(actualReference) => {
                TypeVivem.checkReference(this, expectedType, actualReference)
                actualReference
              }
            }
          }
        }
      }
      case ElementAddressV(objectId, elementIndex) => {
        objectsById(objectId).referend match {
          case ArrayInstanceV(_, size, elements) => {
            val ref = elements(elementIndex)
            TypeVivem.checkReference(this, expectedType, ref)
            ref
          }
        }
      }
    }
  }

  def dereference(reference: ReferenceV): ReferendV = {
    assert(objectsById.contains(reference.objectId))
    objectsById(reference.objectId).referend
  }

  def incrementReferenceRefCount(reference: ReferenceV) = {
    incrementObjectRefCount(reference.objectId)
  }

  def decrementReferenceRefCount(reference: ReferenceV) = {
    decrementObjectRefCount(reference.objectId)
  }

  def deallocate(objectId: Int): Unit = {
    val allocation = objectsById(objectId)
    allocation.referend match {
      case StructInstanceV(struct3, members) => {
        members.foreach(member => {
          member match {
            case ReferenceMemberV(reference) => {
              decrementReferenceRefCount(reference)
            }
            case VariableAddressMemberV(address) => {
              decrementVariableRefCount(address.variableId)
            }
          }
        })
      }
      case ArrayInstanceV(_, _, members) => {
        members.foreach(decrementReferenceRefCount)
      }
      case ViewV(edge, reference) => {
        decrementReferenceRefCount(reference)
      }
      case _ => {}
    }
    objectsById = objectsById - objectId
    print(" o" + objectId + "dealloc")
  }

  def incrementObjectRefCount(objectId: Int) = {
    assert(objectsById.contains(objectId))
    val newRefCount = objectsById(objectId).incrementRefCount()
    print(" o" + objectId + "rc" + (newRefCount - 1) + "->" + newRefCount)
  }

  def decrementObjectRefCount(objectId: Int): Int = {
    if (!objectsById.contains(objectId)) {
      throw new RuntimeException("Can't decrement object " + objectId + ", not in heap!")
    }
    val newRefCount = objectsById(objectId).decrementRefCount()
    print(" o" + objectId + "rc" + (newRefCount + 1) + "->" + newRefCount)
    if (newRefCount == 0) {
      deallocate(objectId)
    }
    newRefCount
  }

  def incrementVariableRefCount(variableId: Int) = {
    assert(localsById.contains(variableId))
    val newRefCount = localsById(variableId).incrementRefCount()
    print(" v" + variableId + "rc" + (newRefCount - 1) + "->" + newRefCount)
  }

  def decrementVariableRefCount(variableId: Int) = {
    assert(localsById.contains(variableId))
    val newRefCount = localsById(variableId).decrementRefCount()
    print(" v" + variableId + "rc" + (newRefCount + 1) + "->" + newRefCount)
  }

  def getRefCount(reference: ReferenceV): Int = {
    assert(objectsById.contains(reference.objectId))
    val allocation = objectsById(reference.objectId)
    allocation.refCount
  }

  def add(ownership: Ownership, referend: ReferendV): ReferenceV = {
    val allocation = new Allocation(newId(), referend)
    objectsById = objectsById.updated(allocation.objectId, allocation)
    ReferenceV(RRReference(Reference3(ownership, referend.tyype.hamut)), allocation.objectId)
  }

  private var nextId = 501;
  private def newId() = {
    val id = nextId;
    nextId = nextId + 1
    id
  }

  def isEmpty: Boolean = {
    objectsById.isEmpty
  }

  def printAll() = {
    objectsById.foreach({
      case (id, allocation) => println(id + " (" + allocation.refCount + " refs) = " + allocation.referend)
    })
  }

  def countUnreachableAllocations(roots: Vector[ReferenceV]) = {
    val numReachables = findReachableAllocations(roots).size
    assert(numReachables <= objectsById.size)
    objectsById.size - numReachables
  }

  def findReachableAllocations(
      inputReachables: Vector[ReferenceV]): Map[Int, Allocation] = {
    val destinationMap = mutable.Map[Int, Allocation]()
    inputReachables.foreach(inputReachable => {
      innerFindReachableAllocations(destinationMap, inputReachable)
    })
    destinationMap.toMap
  }

  private def innerFindReachableAllocations(
      destinationMap: mutable.Map[Int, Allocation],
      inputReachable: ReferenceV): Unit = {
    // Doublecheck that all the inputReachables are actually in this heap...
    assert(objectsById.contains(inputReachable.objectId))
    assert(objectsById(inputReachable.objectId).referend.tyype.hamut == inputReachable.tyype.hamut.innerType)

    val allocation = objectsById(inputReachable.objectId)
    if (destinationMap.contains(inputReachable.objectId)) {
      return
    }

    destinationMap.put(inputReachable.objectId, allocation)
    allocation.referend match {
      case IntV(_) =>
      case BoolV(_) =>
      case FloatV(_) =>
      case ViewV(_, _) =>
      case StructInstanceV(structDef3, members) => {
        members.zip(structDef3.members).foreach({
          case (ReferenceMemberV(reference), StructMember3(_, ReferenceMemberType3(reference3))) => {
            innerFindReachableAllocations(destinationMap, reference)
          }
          case (VariableAddressMemberV(address), StructMember3(_, AddressMemberType3(reference3))) => {
            val reference = dereferenceAddress(address, reference3)
            innerFindReachableAllocations(destinationMap, reference)
          }
        })
      }
    }
  }
}
