package net.verdagon.radonc.vivem

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.Mutability
import net.verdagon.radonc.templar.{Ownership, Raw}
import net.verdagon.radonc.vivem.ExpressionVivem.Return

import scala.collection.immutable.Stack
import scala.collection.mutable

// A wrapper that represents us holding a reference count +1 to this reference.
case class ReturnV(reference: ReferenceV)

class AdapterForExterns(
    private val heap: Heap,
    val stdout: ((String) => Unit)
) {
  def dereference(reference: ReferenceV) = {
    heap.dereference(reference)
  }

  def addAllocationForReturn(ownership: Ownership, referend: ReferendV): ReturnV = {
    val ref = heap.add(ownership, referend)
    heap.incrementReferenceRefCount(ref) // incrementing because putting it in a return
    ReturnV(ref)
  }
}

class StackFrame(
    private val heap: Heap,
    private val argsArray: Vector[ReferenceV],
    stdout: String => Unit
) {
  // Every argument can only be used once. It's destroyed on first use.
  var decrementedArgs: Vector[Boolean] = argsArray.map(_ => false)
  argsArray.foreach(argRef => incrementReferenceRefCount(argRef))

  def dispose() = {
    assert(!decrementedArgs.contains(false))
    thisCallLocals.foreach(variableId => heap.removeLocal(variableId))
  }

  private var thisCallLocals = Stack[Int]()
  private var registersById = Map[String, RegisterV]()

  private def clearRegister(registerId: String): Unit = {
    assert(registersById.contains(registerId))
    registersById = registersById - registerId
  }

  def moveArgumentIntoRegister(registerId: String, argumentIndex: Int, expectedType: Reference3) = {
    assert(!decrementedArgs(argumentIndex))
    decrementedArgs = decrementedArgs.updated(argumentIndex, true)
    val reference = argsArray(argumentIndex)
    checkReference(expectedType, reference)
    setReferenceRegister(registerId, reference) // this increments it
    decrementReferenceRefCount(reference) // decrementing because taking it out of arg
    // Now, the register is the only one that has this reference.
  }

  def returnFromRegister(registerId: String, expectedType: Reference3) = {
    val ref = getReferenceFromRegister(registerId, expectedType)
    incrementReferenceRefCount(ref) // incrementing because putting it into the return slot
    ReturnV(ref)
  }

  def getAdapterForExterns() = {
    new AdapterForExterns(heap, stdout)
  }

  def spawnNewStackFrame(
      argsReferences: Vector[ReferenceV]) = {
    new StackFrame(heap, argsReferences, stdout)
  }

  // For example, for the integer we pass into the array generator
  def allocateTransient(ownership: Ownership, referend: ReferendV) = {
    val ref = heap.add(ownership, referend)
    print(" o" + ref.objectId + "=")
    printReferend(referend)
    ref
  }

  def allocateIntoRegister(registerId: String, ownership: Ownership, referend: ReferendV) = {
    val ref = heap.add(ownership, referend)
    print(" o" + ref.objectId + "=")
    printReferend(referend)
    setReferenceRegister(registerId, ref)
  }

  def printReferend(referend: ReferendV) = {
    referend match {
      case IntV(value) => print(value)
      case BoolV(value) => print(value)
      case StrV(value) => print(value)
      case FloatV(value) => print(value)
      case FunctionReferendV(function3) => print(function3.humanName + "(...)")
      case ViewV(edge, reference) => print(reference.objectId + ":" + edge.interface.globalName)
      case StructInstanceV(struct3, members) => print(struct3.globalName + "{" + members.map(printStructMember).mkString(", ") + "}")
      case ArrayInstanceV(memberType3, size, elements) => print("array:" + size + ":" + memberType3 + "{" + elements.map(_.objectId).mkString(", ") + "}")
    }
  }

  def printStructMember(member: MemberV) = {
    member match {
      case VariableAddressMemberV(VariableAddressV(variableId)) => "v" + variableId
      case ReferenceMemberV(reference) => "o" + reference.objectId
    }
  }

  def setReferenceRegister(registerId: String, reference: ReferenceV) = {
    assert(!registersById.contains(registerId))
    heap.incrementReferenceRefCount(reference) // incrementing because putting it into a register
    registersById = registersById.updated(registerId, ReferenceRegisterV(reference))
    print(" r" + registerId + "<-" + reference.objectId)
  }

  def setReferenceRegisterFromReturn(registerId: String, ret: ReturnV) = {
    assert(!registersById.contains(registerId))
    // Not incrementing ref count, we're conceptually moving this return value
    registersById = registersById.updated(registerId, ReferenceRegisterV(ret.reference))
  }

  def initializeArrayElementFromReturn(arrayInstance: ArrayInstanceV, index: Int, ret: ReturnV) = {
    // Not incrementing ref count, we're conceptually moving this return value
    arrayInstance.initializeElement(index, ret.reference)
  }

  def setMemberAddressRegister(registerId: String, memberAddress: MemberAddressV) = {
    assert(!registersById.contains(registerId))
    registersById = registersById.updated(registerId, MemberAddressRegisterV(memberAddress))
    heap.incrementObjectRefCount(memberAddress.objectId)
  }

  def setElementAddressRegister(registerId: String, elementAddress: ElementAddressV) = {
    assert(!registersById.contains(registerId))
    registersById = registersById.updated(registerId, ElementAddressRegisterV(elementAddress))
    heap.incrementObjectRefCount(elementAddress.objectId)
  }

  def setVariableAddressRegister(registerId: String, variableId: Int) = {
    assert(!registersById.contains(registerId))
    registersById = registersById.updated(registerId, VariableAddressRegisterV(VariableAddressV(variableId)))
    heap.incrementVariableRefCount(variableId)
  }

  def clearAddressRegister(dyingRegisterId: String, expectedType: Reference3) = {
    val address = getAddressFromRegister(dyingRegisterId, expectedType)
    address match {
      case VariableAddressV(variableId) => heap.decrementVariableRefCount(variableId)
      case MemberAddressV(objectId, fieldIndex) => heap.decrementObjectRefCount(objectId)
      case ElementAddressV(objectId, elementIndex) => heap.decrementObjectRefCount(objectId)
    }
    clearRegister(dyingRegisterId)
  }

  def clearReferenceRegister(dyingRegisterId: String, expectedType: Reference3) = {
    val ref = getReferenceFromRegister(dyingRegisterId, expectedType)
    heap.decrementReferenceRefCount(ref)
    clearRegister(dyingRegisterId)
  }



  def getReferenceFromRegister(registerId: String, expectedType: Reference3) = {
    TypeVivem.checkReferenceRegister(heap, expectedType, registersById(registerId)).reference
  }

  def getReferencesFromRegisters(registerIds: List[String], expectedTypes: List[Reference3]): List[ReferenceV] = {
    registerIds.zip(expectedTypes).map({
      case (argRegisterId, expectedType) => getReferenceFromRegister(argRegisterId, expectedType)
    })
  }

  def getFunctionReferenceFromRegister(functionLine: String, expectedFunctionType: FunctionT3) = {
    val functionReference =
      getReferenceFromRegister(functionLine, Reference3(Raw, expectedFunctionType))
    val functionReferend = TypeVivem.checkFunctionReference(heap, expectedFunctionType, functionReference)
    functionReferend.function
  }

  def getVariableAddressFromRegister(registerId: String, expectedType: Reference3) = {
    TypeVivem.checkVariableAddress(heap, expectedType, registersById(registerId))
  }

  def getAddressFromRegister(registerId: String, expectedType: Reference3) = {
    TypeVivem.checkAddress(heap, expectedType, registersById(registerId))
  }

  def mutate(address: AddressV, reference: ReferenceV, expectedType: Reference3) = {
    heap.mutate(address, reference, expectedType)
  }

  def dereferenceAddress(address: AddressV, expectedType: Reference3) = {
    heap.dereferenceAddress(address, expectedType)
  }

  def dereference(reference: ReferenceV): ReferendV = {
    heap.dereference(reference)
  }

  def decrementReferenceRefCount(reference: ReferenceV) = {
    heap.decrementReferenceRefCount(reference)
  }

  def incrementReferenceRefCount(reference: ReferenceV) = {
    heap.incrementReferenceRefCount(reference)
  }

  def checkReference(tyype: Reference3, reference: ReferenceV) = {
    TypeVivem.checkReference(heap, tyype, reference)
  }

  def addLocal(reference: ReferenceV, tyype: Reference3): Int = {
    val variableId = heap.addLocal(reference, tyype)
    thisCallLocals = thisCallLocals.push(variableId)
    variableId
  }

  def newStruct(
      program3: Program3,
      sourceRegisterIds: List[String],
      structRef3: StructRef3,
      mutability: Mutability):
  StructInstanceV = {
    val structDef3 = program3.structs.find(_.getRef() == structRef3).get
    assert(structDef3.mutability == mutability)
    val argStructMembers =
      sourceRegisterIds.zip(structDef3.members.map(_.tyype)).map({
        case (sourceLine, AddressMemberType3(innerType3)) => {
          val variableAddress = getVariableAddressFromRegister(sourceLine, innerType3)
          heap.incrementVariableRefCount(variableAddress.variableId)
          VariableAddressMemberV(variableAddress)
        }
        case (sourceLine, ReferenceMemberType3(reference)) => {
          val ref = getReferenceFromRegister(sourceLine, reference)
          heap.incrementReferenceRefCount(ref)
          ReferenceMemberV(ref)
        }
      })
    StructInstanceV(structDef3, argStructMembers.toVector)
  }

  def newArray(
      sourceRegisterIds: List[String],
      memberType3: Reference3):
  ArrayInstanceV = {
    val elements =
      sourceRegisterIds.map(sourceRegisterId => {
        val ref = getReferenceFromRegister(sourceRegisterId, memberType3)
        heap.incrementReferenceRefCount(ref)
        ref
      })
    ArrayInstanceV(memberType3, elements.size, elements.toVector)
  }

  def newUninitializedArray(
      memberType3: Reference3,
      size: Int):
  ArrayInstanceV = {
    ArrayInstanceV(memberType3, size, Vector())
  }
}
