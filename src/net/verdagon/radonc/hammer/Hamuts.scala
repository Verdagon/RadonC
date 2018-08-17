package net.verdagon.radonc.hammer

import net.verdagon.radonc.templar._

// Identifier names need to come from the Templar output because some things are erased
// by Midas types, such as pointer ownership. Midas will sometimes output many functions
// with the same signature because of this. Thats one reason why functions are keyed by
// Prototype2.

case class Hamuts(
    structRefs: Map[StructRef2, StructRef3],
    structDefs: Map[StructRef2, StructDefinition3],
    interfaceRefs: Map[InterfaceRef2, InterfaceRef3],
    interfaceDefs: Map[InterfaceRef2, InterfaceDefinition3],
    functionRefs: Map[Prototype2, FunctionRef3],
    functionDefs: Map[Prototype2, Function3]) {
  private def addPackStruct(pack: PackT2, structDef: StructDefinition3): Hamuts = {
    Hamuts(
      structRefs,
      structDefs,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareStruct(structRef2: StructRef2, structRef3: StructRef3): Hamuts = {
    Hamuts(
      structRefs + (structRef2 -> structRef3),
      structDefs,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addStruct(structRef2: StructRef2, structDef3: StructDefinition3): Hamuts = {
    assert(structRefs.contains(structRef2))
    Hamuts(
      structRefs,
      structDefs + (structRef2 -> structDef3),
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def forwardDeclareInterface(interfaceRef2: InterfaceRef2, interfaceRef3: InterfaceRef3): Hamuts = {
    Hamuts(
      structRefs,
      structDefs,
      interfaceRefs + (interfaceRef2 -> interfaceRef3),
      interfaceDefs,
      functionRefs,
      functionDefs)
  }

  def addInterface(interfaceRef2: InterfaceRef2, interfaceDef3: InterfaceDefinition3): Hamuts = {
    assert(interfaceRefs.contains(interfaceRef2))
    Hamuts(
      structRefs,
      structDefs,
      interfaceRefs,
      interfaceDefs + (interfaceRef2 -> interfaceDef3),
      functionRefs,
      functionDefs)
  }

  def forwardDeclareFunction(functionRef2: Prototype2, functionRef3: FunctionRef3): Hamuts = {
    Hamuts(
      structRefs,
      structDefs,
      interfaceRefs,
      interfaceDefs,
      functionRefs + (functionRef2 -> functionRef3),
      functionDefs)
  }

  def addFunction(functionRef2: Prototype2, functionDef3: Function3): Hamuts = {
    assert(functionRefs.contains(functionRef2))
    Hamuts(
      structRefs,
      structDefs,
      interfaceRefs,
      interfaceDefs,
      functionRefs,
      functionDefs + (functionRef2 -> functionDef3))
  }
}
