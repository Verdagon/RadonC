package net.verdagon.radonc.hammer

import net.verdagon.radonc.carpenter.{ETable2, TetrisTable}
import net.verdagon.radonc.scout.Mutability
import net.verdagon.radonc.templar._

import scala.collection.immutable.ListMap

trait IRegister3 {
  def expectReferenceRegister(): ReferenceRegister3 = {
    this match {
      case r @ ReferenceRegister3(_) => r
      case a @ AddressRegister3(_) => throw new RuntimeException("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddressRegister(): AddressRegister3 = {
    this match {
      case a @ AddressRegister3(_) => a
      case r @ ReferenceRegister3(_) => throw new RuntimeException("Expected an address as a result, but got a reference!")
    }
  }
}
case class ReferenceRegister3(reference: Reference3) extends IRegister3
case class AddressRegister3(reference: Reference3) extends IRegister3

trait Node3 {
  def registerId: String;
  def resultRegister: IRegister3;
  def accessedRegisterIds: List[String]
}

trait NonProducingNode3 extends Node3 {
  override def resultRegister: IRegister3 = throw new RuntimeException("Non producing node, doesn't have a result register")
}

trait ReferenceNode3 extends Node3 {
  def resultRegister: ReferenceRegister3;
}

case class ExtendedReferenceNode3(registerId: String, extension: IExtendedReferenceNode3)
trait IExtendedReferenceNode3

trait AddressNode3 extends Node3 {
  def resultRegister: AddressRegister3;
}

case class LoadFunction3(registerId: String, functionRef3: FunctionRef3) extends ReferenceNode3 {
  def functionType = functionRef3.prototype.functionType
  override def resultRegister = ReferenceRegister3(Reference3(Raw, functionType))
  override def accessedRegisterIds: List[String] = List()
}
case class ConstantBool3(registerId: String, value: Boolean) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, Bool3()))
  override def accessedRegisterIds: List[String] = List()
}
case class ConstantI643(registerId: String, value: Int) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, Int3()))
  override def accessedRegisterIds: List[String] = List()
}
case class ConstantStr3(registerId: String, value: String) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, Str3()))
  override def accessedRegisterIds: List[String] = List()
}
case class ConstantF643(registerId: String, value: Float) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, Float3()))
  override def accessedRegisterIds: List[String] = List()
}
case class Argument3(registerId: String, resultReference: Reference3, argumentIndex: Int) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultReference)
  override def accessedRegisterIds: List[String] = List()
}
case class Stackify3(registerId: String, resultPointerType: Reference3, sourceRegisterId: String, name: String) extends AddressNode3 {
  override def resultRegister = AddressRegister3(resultPointerType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class DestroyStruct3(registerId: String, sourceRegisterId: String, structId: StructRef3, sourceType: Reference3) extends ReferenceNode3 {
  override def resultRegister = throw new RuntimeException("this has no result type")
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class Store3(registerId: String, tyype: Reference3, destinationRegisterId: String, valueRegisterId: String) extends NonProducingNode3 {
  override def accessedRegisterIds: List[String] = List(destinationRegisterId, valueRegisterId)
}
case class LocalLookup3(registerId: String, varType: Reference3, sourceRegisterId: String, name: String) extends AddressNode3 {
  override def resultRegister = AddressRegister3(varType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class SoftLoad3(registerId: String, resultType: Reference3, sourceRegisterId: String, sourceLineType: Reference3) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class StructToInterfaceUpcast3(registerId: String, resultType: Reference3, sourceStructRef: StructRef3, targetInterfaceRef: InterfaceRef3, sourceRegisterId: String, sourceLineType: Reference3) extends ReferenceNode3 {
  assert(resultType.ownership == sourceLineType.ownership)
  // In the case of borrow ones, we should borrow the struct FIRST, and then make that into
  // a view.

  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class InterfaceToInterfaceUpcast3(registerId: String, resultType: Reference3, sourceInterfaceRef: InterfaceRef3, targetInterfaceRef: InterfaceRef3, sourceRegisterId: String) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class Panic3(registerId: String) extends NonProducingNode3 {
  override def accessedRegisterIds: List[String] = List()
}
case class StructLookup3(registerId: String, resultType: Reference3, sourceRegisterId: String, sourceLineType: Reference3, structType: StructRef3, memberIndex: Int) extends AddressNode3 {
  override def resultRegister = AddressRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class AddressMemberLookup3(registerId: String, resultType: Reference3, sourceRegisterId: String, sourceLineType: Reference3, structType: StructRef3, memberIndex: Int) extends AddressNode3 {
  override def resultRegister = AddressRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}
case class ElementLookup3(
    registerId: String,
    resultType: Reference3,
    sourceRegisterId: String,
    sourceLineType: Reference3,
    arrayType: ArrayT3,
    elementIndexRegister: String) extends AddressNode3 {
  override def resultRegister = AddressRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId, elementIndexRegister)
}
case class Call3(
    registerId: String,
    resultType: Reference3,
    functionRegisterId: String,
    functionType: FunctionT3,
    argsRegisterIds: List[String],
    paramTypes: List[Reference3]
) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = functionRegisterId :: argsRegisterIds
}
case class ExternCall3(
    registerId: String,
    resultType: Reference3,
    functionRef3: FunctionRef3,
    argsRegisterIds: List[String],
    paramTypes: List[Reference3]
) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = argsRegisterIds
}
case class InterfaceCall3(
    registerId: String,
    resultType: Reference3,
    virtualParamIndex: Int,
    interfaceRef3: InterfaceRef3,
    interfaceId: Int,
    indexInEdge: Int,
    functionType: FunctionT3,
    argsRegisterIds: List[String],
    paramTypes: List[Reference3]
) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = argsRegisterIds
}
case class Return3(registerId: String, returnType: Reference3, sourceRegisterId: String) extends NonProducingNode3 {
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}

// May seem unintuitive, but remember the body of every if statement is a block/closure.
// The args here are simply to assemble the closure struct for it.
// Even though we hammer the args to before the if-call, LLVM optimizes them into the
// if case bodies.
case class IfCall3(
    registerId: String,
    conditionRegisterId: String,
    resultType: Reference3,
    trueFunctionRegisterId: String,
    trueFunctionType: FunctionT3,
    trueArgsRegisterIds: List[String],
    trueParamTypes: List[Reference3],
    falseFunctionRegisterId: String,
    falseFunctionType: FunctionT3,
    falseArgsRegisterIds: List[String],
    falseParamTypes: List[Reference3]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(conditionRegisterId, trueFunctionRegisterId, falseFunctionRegisterId) ++ trueArgsRegisterIds ++ falseArgsRegisterIds
}
case class ConstructArrayCall3(
    registerId: String,
    sizeRegisterId: String,
    arrayRefType: Reference3,
    arrayType: ArrayT3,
    generatorFunctionRegisterId: String,
    generatorFunctionType: FunctionT3,
    generatorArgsRegisterIds: List[String],
    generatorParamTypes: List[Reference3]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(arrayRefType)
  override def accessedRegisterIds: List[String] = List(sizeRegisterId, generatorFunctionRegisterId) ++ generatorArgsRegisterIds
}
case class NewMutableStruct3(registerId: String, structType: StructRef3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Own, structType))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class NewImmutableStruct3(registerId: String, structType: StructRef3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, structType))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class NewMutableArrayFromValues3(registerId: String, memberType3: Reference3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Own, ArrayT3(memberType3)))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class NewImmutableArrayFromValues3(registerId: String, memberType3: Reference3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, ArrayT3(memberType3)))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class NewMutableArrayFromFunction3(registerId: String, memberType3: Reference3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Own, ArrayT3(memberType3)))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class NewImmutableArrayFromFunction3(registerId: String, memberType3: Reference3, sourceRegisterIds: List[String]) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(Reference3(Share, ArrayT3(memberType3)))
  override def accessedRegisterIds: List[String] = sourceRegisterIds
}
case class Alias3(registerId: String, resultType: Reference3, structType: StructRef3, sourceRegisterId: String, sourceType: Reference3) extends ReferenceNode3 {
  override def resultRegister = ReferenceRegister3(resultType)
  override def accessedRegisterIds: List[String] = List(sourceRegisterId)
}

case class Prototype3(functionId: Int, humanName: String, globalName: String, params: List[Reference3], returnType: Reference3) {
  def functionType = FunctionT3(params, returnType)
}

case class Function3(
    prototype: Prototype3,
    isAbstract: Boolean,
    isExtern: Boolean,
    nodes: Vector[Node3]) {
  def getRef() = FunctionRef3(prototype)
  def humanName = prototype.humanName
  assert(nodes.map(_.registerId).toSet.size == nodes.size)
}


case class InterfaceDefinition3(
    interfaceId: Int,
    humanName: String,
    globalName: String,
    mutability: Mutability,
    superInterfaces: List[InterfaceRef3],
    prototypes: List[Prototype3]) {
  def getRef() = InterfaceRef3(interfaceId, globalName)
}

case class FunctionRef3(prototype: Prototype3) {
  def functionType = prototype.functionType
  def globalName = prototype.globalName
}

case class Edge3(
    struct: StructRef3,
    interface: InterfaceRef3,
    structPrototypesByInterfacePrototype: ListMap[Prototype3, Prototype3])

case class ETable3(struct: StructRef3, table: TetrisTable[InterfaceRef3, InterfaceRef3])

case class StructMember3(name: String, tyype: IMemberType3)

sealed trait IMemberType3 {
  def reference: Reference3

  def expectReferenceMember(): ReferenceMemberType3 = {
    this match {
      case r @ ReferenceMemberType3(_) => r
      case a @ AddressMemberType3(_) => throw new RuntimeException("Expected reference member, was address member!")
    }
  }
  def expectAddressMember(): AddressMemberType3 = {
    this match {
      case r @ ReferenceMemberType3(_) => throw new RuntimeException("Expected reference member, was address member!")
      case a @ AddressMemberType3(_) => a
    }
  }
}
case class AddressMemberType3(reference: Reference3) extends IMemberType3
case class ReferenceMemberType3(reference: Reference3) extends IMemberType3

case class StructDefinition3(
    structId: Int,
    humanName: String,
    globalName: String,
    mutability: Mutability,
    base: Option[StructRef3],
    eTable: ETable3,
    edges: List[Edge3],
    members: List[StructMember3],
    methods: List[FunctionRef3]) {

  def getRef(): StructRef3 = StructRef3(structId, globalName)

  // These functions are tightly coupled with StructSculptor.declareStructInfo
  def getInterfacePtrElementIndex(interfaceRef: InterfaceRef3): Int = {
    val index = edges.indexWhere(_.interface == interfaceRef)
    assert(index >= 0)
    index
  }
  def getSInfoPtrElementIndex(): Int = {
    edges.size + 1
  }

  def getMemberLlvmIndex(memberIndex: Int): Int = {
    assert(memberIndex < members.size)
    edges.size + 2 + memberIndex
  }

  def getTypeAndIndex(memberName: String): (IMemberType3, Int) = {
    members.zipWithIndex.find(p => p._1.name.equals(memberName)) match {
      case None => throw new RuntimeException("wat " + this + " " + memberName)
      case Some((member, index)) => (member.tyype, index)
    }
  }
}

case class Program3(
    interfaces: List[InterfaceDefinition3],
    structs: List[StructDefinition3],
    externs: List[Prototype3],
    functions: List[Function3]) {
  def externFunctions = functions.filter(_.isExtern)
  def abstractFunctions = functions.filter(_.isAbstract)
  // Functions that are neither extern nor abstract
  def implementedFunctions = functions.filter(f => !f.isExtern && !f.isAbstract)
  // Abstract or implemented
  def nonExternFunctions = functions.filter(!_.isExtern)
  def main() = {
    val matching = functions.filter(_.humanName == "main")
    assert(matching.size == 1)
    matching.head
  }

  def lookupFunction(humanName: String) = {
    val matches = functions.filter(_.humanName == humanName)
    assert(matches.size == 1)
    matches.head
  }
}

