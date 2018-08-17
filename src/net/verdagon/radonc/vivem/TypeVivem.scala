package net.verdagon.radonc.vivem

import net.verdagon.radonc.hammer._
import net.verdagon.radonc.templar.{Ownership, Own}

import scala.collection.immutable.Stack

object TypeVivem {
  def checkVariableAddress(heap: Heap, tyype: Reference3, register: RegisterV): VariableAddressV = {
    register match {
      case ReferenceRegisterV(reference) => {
        throw new RuntimeException("Expected variable address, was a reference!")
      }
      case MemberAddressRegisterV(memberAddressV) => {
        throw new RuntimeException("Expected reference, was a member address!")
      }
      case VariableAddressRegisterV(address) => {
        checkReference(heap, tyype, heap.dereferenceAddress(address, tyype))
        address
      }
    }
  }

  def checkAddress(heap: Heap, tyype: Reference3, register: RegisterV): AddressV = {
    register match {
      case ReferenceRegisterV(reference) => {
        throw new RuntimeException("Expected variable address, was a reference!")
      }
      case MemberAddressRegisterV(memberAddressV) => {
        checkReference(heap, tyype, heap.dereferenceAddress(memberAddressV, tyype))
        memberAddressV
      }
      case ElementAddressRegisterV(elementAddressV) => {
        checkReference(heap, tyype, heap.dereferenceAddress(elementAddressV, tyype))
        elementAddressV
      }
      case VariableAddressRegisterV(address) => {
        checkReference(heap, tyype, heap.dereferenceAddress(address, tyype))
        address
      }
    }
  }

  def checkReferenceRegister(heap: Heap, tyype: Reference3, register: RegisterV): ReferenceRegisterV = {
    register match {
      case rr @ ReferenceRegisterV(reference) => {
        checkReference(heap, tyype, reference)
        rr
      }
      case MemberAddressRegisterV(memberAddressV) => {
        throw new RuntimeException("Expected reference, was a member address!")
      }
      case VariableAddressRegisterV(variable) => {
        throw new RuntimeException("Expected reference, was a variable address!")
      }
    }
  }

  def checkReference(heap: Heap, expectedType: Reference3, actualReference: ReferenceV): Unit = {
    if (actualReference.tyype.hamut != expectedType) {
      throw new RuntimeException("Expected " + expectedType + " but was " + actualReference.tyype.hamut)
    }
    val actualReferend = heap.dereference(actualReference)
    checkReferend(heap, expectedType.innerType, actualReferend)
  }

  def checkReferend(heap: Heap, expectedType: Referend3, actualReferend: ReferendV): Unit = {
    (actualReferend, expectedType) match {
      case (IntV(_), Int3()) =>
      case (BoolV(_), Bool3()) =>
      case (StrV(_), Str3()) =>
      case (FloatV(_), Float3()) =>
      case (VoidV(), Void3()) =>
      case (StructInstanceV(structDef3, _), structRef3 @ StructRef3(_, _)) => {
        if (structDef3.getRef() != structRef3) {
          throw new RuntimeException("Expected " + structRef3 + " but was " + structDef3)
        }
      }
      case (ArrayInstanceV(actualElementType3, _, _), array3 @ ArrayT3(expectedElementType3)) => {
        if (actualElementType3 != expectedElementType3) {
          throw new RuntimeException("Expected " + expectedElementType3 + " but was " + actualElementType3)
        }
      }
      case (FunctionReferendV(function3), ft3 @ FunctionT3(_, _)) => {
        if (function3.prototype.functionType != ft3) {
          throw new RuntimeException("Expected a " + ft3 + " but was a " + function3.prototype.functionType)
        }
      }
      case (ViewV(_, reference), ir3 @ InterfaceRef3(interfaceId3, _)) => {
        heap.dereference(reference) match {
          case StructInstanceV(structDef3, _) => {
            val structImplementsInterface =
              structDef3.edges.exists(_.interface == ir3)
            if (!structImplementsInterface) {
              throw new RuntimeException("Struct " + structDef3.getRef() + " doesnt implement interface " + interfaceId3);
            }
          }
        }
      }
      case (a, b) => {
        throw new RuntimeException("Mismatch! " + a + " is not a " + b)
      }
    }
  }

  def checkStructId(heap: Heap, expectedStructType: StructRef3, expectedStructPointerType: Reference3, register: RegisterV): Int = {
    val reference = checkReferenceRegister(heap, expectedStructPointerType, register).reference
    heap.dereference(reference) match {
      case siv @ StructInstanceV(structDef3, _) => {
        assert(structDef3.getRef() == expectedStructType)
      }
      case _ => throw new RuntimeException("Expected a struct but was " + register)
    }
    reference.objectId
  }

  def checkStructReference(heap: Heap, expectedStructType: StructRef3, expectedStructPointerType: Reference3, register: RegisterV): StructInstanceV = {
    val reference = checkReferenceRegister(heap, expectedStructPointerType, register).reference
    heap.dereference(reference) match {
      case siv @ StructInstanceV(structDef3, _) => {
        assert(structDef3.getRef() == expectedStructType)
        siv
      }
      case _ => throw new RuntimeException("Expected a struct but was " + register)
    }
  }

  def checkStructReference(heap: Heap, expectedStructType: StructRef3, reference: ReferenceV): StructInstanceV = {
    heap.dereference(reference) match {
      case siv @ StructInstanceV(structDef3, _) => {
        assert(structDef3.getRef() == expectedStructType)
        siv
      }
      case _ => throw new RuntimeException("Expected a struct but was " + reference)
    }
  }

  def checkFunctionReference(heap: Heap, expectedFunctionType: FunctionT3, reference: ReferenceV): FunctionReferendV = {
    heap.dereference(reference) match {
      case ufr @ FunctionReferendV(function3) => {
        assert(function3.prototype.functionType == expectedFunctionType)
        ufr
      }
      case _ => throw new RuntimeException("Expected a function but was " + reference)
    }
  }
}
