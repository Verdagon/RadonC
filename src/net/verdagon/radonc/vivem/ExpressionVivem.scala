package net.verdagon.radonc.vivem

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.{Immutable, Mutable}
import net.verdagon.radonc.templar.{Borrow, Own, Raw, Share}
import net.verdagon.radonc.vivem.Vivem.{innerExecute, getExternFunction}

object ExpressionVivem {
  sealed trait IExecuteResult
  case class Continue() extends IExecuteResult
  case class Return(returnRef: ReturnV) extends IExecuteResult

  def executeNode(
      program3: Program3,
      memoryContext: StackFrame,
      callDepth: Int,
      node: Node3):
  IExecuteResult = {
    val registerId = node.registerId
    node match {
      case ConstantI643(_, value) => {
        memoryContext.allocateIntoRegister(registerId, Share, IntV(value))
      }
      case ConstantStr3(_, value) => {
        memoryContext.allocateIntoRegister(registerId, Share, StrV(value))
      }
      case ConstantBool3(_, value) => {
        memoryContext.allocateIntoRegister(registerId, Share, BoolV(value))
      }
      case Argument3(_, resultType, argumentIndex) => {
        memoryContext.moveArgumentIntoRegister(registerId, argumentIndex, resultType)
      }
      case Return3(_, returnType, sourceLine) => {
        return Return(memoryContext.returnFromRegister(sourceLine, returnType))
      }
      case SoftLoad3(_, resultType, sourceLine, sourceLineType) => {
        val variableAddress = memoryContext.getAddressFromRegister(sourceLine, sourceLineType)
        print(" ")
        printAddress(variableAddress)
        val referenceWithSourceType = memoryContext.dereferenceAddress(variableAddress, sourceLineType)
        val referenceWithResultType = referenceWithSourceType.copy(tyype = RRReference(resultType))
        memoryContext.setReferenceRegister(registerId, referenceWithResultType)
      }
      case Stackify3(_, resultPointerType, sourceLine, name) => {
        val reference = memoryContext.getReferenceFromRegister(sourceLine, resultPointerType)
        val variableId = memoryContext.addLocal(reference, resultPointerType)
        memoryContext.setVariableAddressRegister(registerId, variableId)
      }
      case LoadFunction3(_, functionRef3) => {
        val function3 =
          program3.functions.find(_.prototype.functionId == functionRef3.prototype.functionId).get
        assert(function3.prototype.functionType == functionRef3.functionType)
        memoryContext.allocateIntoRegister(registerId, Raw, FunctionReferendV(function3))
      }
      case ExternCall3(_, resultType, functionRef3, argLines, paramTypes) => {
        val externFunction = getExternFunction(functionRef3)
        val argReferences = memoryContext.getReferencesFromRegisters(argLines, paramTypes)
        val resultReference =
          externFunction(memoryContext.getAdapterForExterns(), argReferences.toVector)
        memoryContext.setReferenceRegisterFromReturn(registerId, resultReference)
      }
      case Call3(_, resultType, functionLine, expectedFunctionType, argLines, paramTypes) => {
        val argReferences = memoryContext.getReferencesFromRegisters(argLines, paramTypes)
        val function =
          memoryContext.getFunctionReferenceFromRegister(functionLine, expectedFunctionType)
        println()
        print("  " * callDepth + "Making new stack frame")
        val newStackFrame = memoryContext.spawnNewStackFrame(argReferences.toVector)
        val returnReference =
          Vivem.executeFunction(program3, newStackFrame, function, callDepth + 1)
        print("  " * callDepth + "Getting return reference")
        memoryContext.setReferenceRegisterFromReturn(registerId, returnReference)
      }
      case Store3(_, tyype, destinationLine, valueLine) => {
        val reference = memoryContext.getReferenceFromRegister(valueLine, tyype)
        val address = memoryContext.getAddressFromRegister(destinationLine, tyype)
        print(" ")
        printAddress(address)
        print("<-" + reference.objectId)
        memoryContext.mutate(address, reference, tyype)
      }
      case NewImmutableStruct3(_, structRef3, sourceLines) => {
        val structReference =
          memoryContext.newStruct(program3, sourceLines, structRef3, Immutable)
        memoryContext.allocateIntoRegister(registerId, Share, structReference)
      }
      case NewMutableStruct3(_, structRef3, sourceLines) => {
        val structReference =
          memoryContext.newStruct(program3, sourceLines, structRef3, Mutable)
        memoryContext.allocateIntoRegister(registerId, Own, structReference)
      }
      case NewImmutableArrayFromValues3(_, memberType3, sourceRegisterIds) => {
        val structReference =
          memoryContext.newArray(sourceRegisterIds, memberType3)
        memoryContext.allocateIntoRegister(registerId, Share, structReference)
      }
      case StructLookup3(_, resultType, sourceLine, sourceLineType, structType, memberIndex) => {
        val structReference = memoryContext.getReferenceFromRegister(sourceLine, sourceLineType)
        memoryContext.setMemberAddressRegister(registerId, MemberAddressV(structReference.objectId, memberIndex))
      }
      case ElementLookup3(_, resultType, sourceRegisterId, sourceLineType, arrayType, elementIndexRegister) => {
        val arrayReference =
          memoryContext.getReferenceFromRegister(
            sourceRegisterId, sourceLineType)
        val indexIntReference =
          memoryContext.getReferenceFromRegister(
            elementIndexRegister, Reference3(Share, Int3()))
        val index =
          memoryContext.dereference(indexIntReference) match {
            case IntV(value) => value
          }
        memoryContext.setElementAddressRegister(registerId, ElementAddressV(arrayReference.objectId, index))
      }
      case AddressMemberLookup3(_, resultType, sourceLine, sourceLineType, structType, memberIndex) => {
        val structReference = memoryContext.getReferenceFromRegister(sourceLine, sourceLineType)
        val structReferend = memoryContext.dereference(structReference)
        val address =
          structReferend match {
            case structInstance @ StructInstanceV(_, _) => {
              structInstance.members(memberIndex) match {
                case VariableAddressMemberV(addr) => addr
                case ReferenceMemberV(_) => throw new RuntimeException("Expected variable address but was reference!")
              }
            }
            case _ => throw new RuntimeException("Expected struct instance but was other referend!")
          }
        memoryContext.setVariableAddressRegister(registerId, address.variableId)
      }
      case Alias3(_, resultType, structType, sourceLine, sourceLineType) => {
        assert(Reference3(Borrow, structType) == resultType || Reference3(Share, structType) == resultType)
        val reference = memoryContext.getReferenceFromRegister(sourceLine, sourceLineType)
        // note to self: getStructInstance previously returned a StructInstanceV, that was bad.
        // make it return a reference
        memoryContext.setReferenceRegister(registerId, reference)
      }
      case StructToInterfaceUpcast3(_, resultType, sourceStructRef, targetInterfaceRef, sourceLine, sourceLineType) => {
        val sourceReference = memoryContext.getReferenceFromRegister(sourceLine, sourceLineType);
        val sourceStructDef = program3.structs.find(_.getRef() == sourceStructRef).get
        val edge = sourceStructDef.edges.find(_.interface == targetInterfaceRef).get

        (sourceLineType.ownership, resultType.ownership) match {
          case (Raw, Raw) => {
            memoryContext.allocateIntoRegister(registerId, Raw, ViewV(edge, sourceReference))
          }
          case (Share, Share) => {
            memoryContext.allocateIntoRegister(registerId, Share, ViewV(edge, sourceReference))
          }
          case (Borrow, Borrow) => {
            // We own the reference to the view, but it contains a borrow
            // reference to the source thing.
            memoryContext.allocateIntoRegister(registerId, Own, ViewV(edge, sourceReference))
          }
          case (Own, Own) => {
            memoryContext.allocateIntoRegister(registerId, Own, ViewV(edge, sourceReference))
          }
        }

        memoryContext.incrementReferenceRefCount(sourceReference)
      }
      case InterfaceCall3(_, resultType, virtualParamIndex, interfaceRef3, interfaceId, indexInEdge, functionType, argLines, paramTypes) => {
        // undeviewed = not deviewed = the virtual param is still a view and we want it to
        // be a struct.
        val undeviewedArgReferences = memoryContext.getReferencesFromRegisters(argLines, paramTypes)

        val viewArgReference = undeviewedArgReferences(virtualParamIndex)

        val (edge, vieweeRef) =
          memoryContext.dereference(viewArgReference) match {
            case ViewV(e, r) => (e, r)
          };
        if (edge.interface != interfaceRef3) {
          throw new RuntimeException("Source view's edge doesnt match expected interface!");
        }
        val prototype3 = edge.structPrototypesByInterfacePrototype.values.toList(indexInEdge)
        val function3 = program3.functions.find(_.prototype == prototype3).get;

        val actualFunctionType3 = function3.prototype.functionType
        val expectedFunctionType3 = functionType
        // We would compare function3.type to functionType directly, but
        // function3.type expects a struct and prototype3 expects an interface.

        // First, check that all the other params are correct.
        undeviewedArgReferences.zipWithIndex.zip(actualFunctionType3.paramTypes).zip(expectedFunctionType3.paramTypes).foreach({
          case (((argReference, index), actualFunctionParamType), expectedFunctionParamType) => {
            // Skip the interface line for now, we check it below
            if (index != virtualParamIndex) {
              memoryContext.checkReference(actualFunctionParamType, argReference)
              memoryContext.checkReference(expectedFunctionParamType, argReference)
              assert(actualFunctionParamType == expectedFunctionParamType)
            }
          }
        })

        // Make sure that vieweeRef is actually pointing to a struct.
        // It would be weird if a view contained an integer or something.
        memoryContext.dereference(vieweeRef) match { case StructInstanceV(_, _) => }
        val deviewedArgReferences =
          undeviewedArgReferences.updated(virtualParamIndex, vieweeRef)

        val returnRef =
          Vivem.executeFunction(
            program3,
            memoryContext.spawnNewStackFrame(deviewedArgReferences.toVector),
            function3,
            callDepth + 1)

        memoryContext.setReferenceRegisterFromReturn(registerId, returnRef)
      }
      case IfCall3(
          _, conditionRegisterId, resultType,
          trueFunctionRegisterId, trueFunctionType, trueArgsRegisterIds, trueParamTypes,
          falseFunctionRegisterId, falseFunctionType, falseArgsRegisterIds, falseParamTypes) => {
        val reference = memoryContext.getReferenceFromRegister(conditionRegisterId, Reference3(Share, Bool3()))
        val referend = memoryContext.dereference(reference)
        val BoolV(value) = referend;
        val (function, argReferences) =
        if (value) {
          val trueFunction =
            memoryContext.getFunctionReferenceFromRegister(trueFunctionRegisterId, trueFunctionType)
          val trueArgReferences = memoryContext.getReferencesFromRegisters(trueArgsRegisterIds, trueParamTypes)
          (trueFunction, trueArgReferences)
        } else {
          val falseFunction =
            memoryContext.getFunctionReferenceFromRegister(falseFunctionRegisterId, falseFunctionType)
          val falseArgReferences = memoryContext.getReferencesFromRegisters(falseArgsRegisterIds, falseParamTypes)
          (falseFunction, falseArgReferences)
        }
        println()
        print("  " * callDepth + "Making new stack frame")
        val newStackFrame = memoryContext.spawnNewStackFrame(argReferences.toVector)
        println()
        val returnReference =
          Vivem.executeFunction(program3, newStackFrame, function, callDepth + 1)
        print("  " * callDepth + "Getting return reference")
        memoryContext.setReferenceRegisterFromReturn(registerId, returnReference)
      }
      case ConstructArrayCall3(
          _, sizeRegisterId, arrayRefType, arrayType,
          generatorFunctionRegisterId, generatorFunctionType, generatorArgsRegisterIds, generatorParamTypes) => {
        val sizeReference = memoryContext.getReferenceFromRegister(sizeRegisterId, Reference3(Share, Int3()))
        val sizeReferend = memoryContext.dereference(sizeReference)
        val IntV(size) = sizeReferend;

        val arrayInstance =
          memoryContext.newUninitializedArray(arrayType.memberType, size)
        memoryContext.allocateIntoRegister(registerId, arrayRefType.ownership, arrayInstance)

        val generatorFunction =
          memoryContext.getFunctionReferenceFromRegister(
            generatorFunctionRegisterId, generatorFunctionType)
        val generatorArgReferences =
          memoryContext.getReferencesFromRegisters(
            generatorArgsRegisterIds, generatorParamTypes)
        (0 until size).foreach(i => {
          println()
          print("  " * callDepth + "Making new stack frame")

          assert(memoryContext.dereference(generatorArgReferences.last) == IntV(0))
          val indexInt = memoryContext.allocateTransient(Share, IntV(i))
          val thisIterationGeneratorArgReferences = generatorArgReferences.updated(generatorArgReferences.size - 1, indexInt)

          val newStackFrame = memoryContext.spawnNewStackFrame(thisIterationGeneratorArgReferences.toVector)
          println()
          val returnReference =
            Vivem.executeFunction(program3, newStackFrame, generatorFunction, callDepth + 1)
          print("  " * callDepth + "Getting return reference")

          // No need to increment or decrement, we're conceptually moving the return value
          // from the return slot to the array slot
          memoryContext.initializeArrayElementFromReturn(arrayInstance, i, returnReference)
        });
      }
    }
    Continue()
  }

  def printAddress(v: AddressV) = {
    v match {
      case VariableAddressV(variableId) => {
        print("v" + variableId)
      }
      case MemberAddressV(objectId, fieldIndex) => {
        print("o" + objectId + "." + fieldIndex)
      }
      case ElementAddressV(objectId, elementIndex) => {
        print("e" + objectId + "." + elementIndex)
      }
    }
  }
}
