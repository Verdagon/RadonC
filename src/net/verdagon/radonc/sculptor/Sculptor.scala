package net.verdagon.radonc.sculptor

import net.verdagon.radonc.hammer.{Prototype3, _}
import net.verdagon.radonc.templar.{Borrow, Own}

import scala.collection.immutable.ListMap



object Sculptor {
  def translate(program3: Program3): String = {
    StructSculptor.declareStructs(program3) +
    program3.functions.map(f => translate(program3, f)).mkString("\n\n")
  }

  private def translate(program3: Program3, function3: Function3): String = {
    val Function3(Prototype3(prototype2, humanName, globalName, params3, returnType3), _, _, nodes) = function3;

    val returnTypeStr = TypeSculptor.getReferenceLlvmType(returnType3);
    val paramsStr =
        params3.zipWithIndex
        .map({
          case (paramType, index) => TypeSculptor.getReferenceLlvmType(paramType) + " %arg" + index
        })
        .mkString(", ");
    val signature = returnTypeStr + " @\"" + globalName + "\"(" + paramsStr + ")";

    val body = translate(program3, nodes)

    "define " + signature + "{\n" +
    body +
    "}\n"
  }

  private def translate(program3: Program3, nodes: Vector[Node3]): String = {

//  for (int nextLine = 0; nextLine < function->nodes.size(); ) {
//    auto & current = function->nodes[nextLine];
//    std::cout << "Translating " << current->toString() << endl;
//    nextLine++;

    nodes.map(n => translate(program3, n)).mkString("")
  }

  private def translate(program3: Program3, node: Node3): String = {
    val saveStart = "%line" + node.registerId + " = ";

    val result =
        node match {
          case LoadFunction3(line, functionRef3) => {
            // assuming name is "+"
            // i64(i64, i64)*
            println("hardcoding owning")
            val funcType = TypeSculptor.getReferenceLlvmType(Reference3(Borrow, functionRef3.functionType));
            // %line6 = bitcast i64(i64, i64)* @"+" to i64(i64, i64)*
            val globalName = functionRef3.globalName
            saveStart + "bitcast " + funcType + " @\"" + globalName + "\" to " + funcType + "\n"
          }
//          case LoadExternFunction3(line, functionRef3) => {
//            // assuming name is "+"
//            // i64(i64, i64)*
//            println("hardcoding owning")
//            val funcType = TypeSculptor.getReferenceLlvmType(Reference3(Borrow, functionRef3.functionType));
//            val globalName = functionRef3.globalName
//            // %line6 = bitcast i64(i64, i64)* @"+" to i64(i64, i64)*
//            saveStart + "bitcast " + funcType + " @\"" + globalName + "\" to " + funcType + "\n"
//          }
          case ConstantBool3(line, value) => {
            saveStart + "and i1 1, " + value + "\n"
          }
          case ConstantI643(line, value) => {
            "%line" + line + "sizeptr = getelementptr i64, i64* null, i64 1\n" +
            "%line" + line + "size = ptrtoint i64* %line" + line + "sizeptr to i64\n" +
            "%line" + line + "raw = call i8* @malloc(i64 %line" + line + "size)\n" +
            "%line" + line + " = bitcast i8* %line" + line + "raw to i64*\n" +
            "%line" + line + "value = add i64 0, " + value + "\n" +
            "store i64 %line" + line + "value, i64* %line" + line + "\n"
          }
          case ConstantF643(line, value) => {
            saveStart + "fadd double 0.0, " + value + "\n"
          }
          case Argument3(line, tyype, argumentIndex) => {
            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
            saveStart + "alloca " + typeName + "\n" +
            "store " + typeName + " %arg" + argumentIndex + ", " + typeName + "* %line" + line + "\n"
          }
          case Stackify3(line, tyype, sourceLine, name) => {
            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
            saveStart + "alloca " + typeName + " ; NAME\n"
          }
          case s @ Store3(line, tyype, destinationLine, valueLine) => {
            val sourceTypeName = TypeSculptor.getReferenceLlvmType(tyype)
            val destinationTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegister3(tyype))
            "store " + sourceTypeName + " %line" + valueLine + ", " + destinationTypeName + " %line" + destinationLine + "\n"
          }
          case SoftLoad3(line, tyype, sourceLine, sourceLineType) => {
            val destinationTypeName = TypeSculptor.getReferenceLlvmType(tyype)
            val sourceTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegister3(tyype))
            saveStart + "load " + destinationTypeName + ", " + sourceTypeName + " %line" + sourceLine + "\n"
          }
          case LocalLookup3(line, varType, sourceLine, name) => {
            val typeName = TypeSculptor.getReferenceLlvmType(varType)
            saveStart + "bitcast " + typeName + " %line" + sourceLine + " to " + typeName + " ; NAME\n"
          }
//          case Extract3(line, tyype, sourceLine, index) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "extractvalue " + typeName + " %line" + sourceLine + ", " + index + "\n"
//          }
//          case Add3(line, tyype, leftLine, rightLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "add " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case Eq3(line, tyype, leftLine, rightLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(tyype)
//            saveStart + "icmp eq " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case And3(line, leftLine, rightLine) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(Reference3(Own, Bool3()))
//            // todo: dereference
//            saveStart + "and " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
//          case Or3(line, leftLine, rightLine) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(Reference3(Own, Bool3()))
//            // todo: dereference
//            saveStart + "or " + typeName + " %line" + leftLine + ", %line" + rightLine + "\n"
//          }
          case Call3(line, resultType, functionLine, functionType, argLines, paramTypes) => {
            val resultTypeName = TypeSculptor.getReferenceLlvmType(resultType)
            saveStart + "call " + resultTypeName + " %line" + functionLine + "(" +
            argLines.zip(paramTypes)
                .map({
                  case (argLine, paramType) => TypeSculptor.getReferenceLlvmType(paramType) + " %line" + argLine
                })
                .mkString(", ") +
            ")\n"
          }
          case Return3(line, tyype, sourceLine) => {
            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
            "ret " + typeName + " %line" + sourceLine + "\n"
          }
//          case Branch3(line, conditionLine, trueDestinationLine, falseDestinationLine) => {
//            "br i1 %line" + conditionLine + ", label %label" + trueDestinationLine + ", label %label" + falseDestinationLine + "\n"
//          }
//          case Phi3(line, tyype, cases) => {
//            val typeName = TypeSculptor.getReferenceLlvmType(tyype)
//            saveStart + "phi " + typeName + " " + cases.map(c => "[%line" + c.resultRegisterId + ", %label" + c.bodyLabelRegisterId + "]").mkString(", ") + "\n"
//          }
//          case Jump3(line, destinationLine) => {
//            "br label %label" + destinationLine + "\n"
//          }
//          case Label3(line) => {
//            "label" + line + ":\n"
//          }
//          case DerefAndCopy3(line, resultType, sourceLine) => {
//            val typeName = Typer.getLlvmTypeForRegister(resultType)
//            println("todo: copy structs here!")
//            saveStart + "load " + typeName + ", " + typeName + "* %line" + sourceLine + "\n"
//          }
          case AddressMemberLookup3(line, resultType, sourceLine, sourceLineType, structType, memberIndex) => {
            val structTypeName = TypeSculptor.getConcreteLlvmType(structType)
            val memberTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegister3(resultType))
            val memberLlvmIndex = program3.structs.find(_.getRef() == structType).get.getMemberLlvmIndex(memberIndex)
            "%line" + line + "ptr = getelementptr inbounds " + structTypeName + ", " + structTypeName + "* %line" + sourceLine + ", i32 0, i32 " + memberLlvmIndex + "\n" +
            saveStart + "load " + memberTypeName + ", " + memberTypeName + "* %line" + line + "ptr\n"
          }
          case StructLookup3(line, resultType, sourceLine, sourceLineType, structType, memberIndex) => {
            val structTypeName = TypeSculptor.getConcreteLlvmType(structType)
            val memberTypeName = TypeSculptor.getAddressRegisterLlvmType(AddressRegister3(resultType))
            val memberLlvmIndex = program3.structs.find(_.getRef() == structType).get.getMemberLlvmIndex(memberIndex)
            saveStart + "getelementptr inbounds " + structTypeName + ", " + structTypeName + "* %line" + sourceLine + ", i32 0, i32 " + memberLlvmIndex + "\n"
          }
          case NewMutableStruct3(line, structRef3, sourceLines) => {
            val llvmType = TypeSculptor.getConcreteLlvmType(structRef3)
            val typeName = structRef3.globalName
            val structDef3 = program3.structs.find(_.getRef() == structRef3).get;
            val sInfoPtrIndex = structDef3.getSInfoPtrElementIndex();
            "%line" + line + "sizeptr = getelementptr " + llvmType + ", " + llvmType + "* null, i64 1\n" +
                "%line" + line + "size = ptrtoint " + llvmType + "* %line" + line + "sizeptr to i64\n" +
                "%line" + line + "raw = call i8* @malloc(i64 %line" + line + "size)\n" +
                "%line" + line + " = bitcast i8* %line" + line + "raw to " + llvmType + "*\n" +
                "%line" + line + "vptrAddr = getelementptr " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + sInfoPtrIndex + "\n" +
                "store %__SInfo* getelementptr (%\"__SInfoWhole_" + typeName + "\", %\"__SInfoWhole_" + typeName + "\"* @\"__sInfoWhole_" + typeName + "\", i32 0, i32 1, i32 1), %__SInfo** %line" + line + "vptrAddr\n" +
                (structRef3 match {
                  case s : StructRef3 => {
                    val struct = program3.structs.find(_.getRef() == s).get;
                    struct.members.zipWithIndex.zip(sourceLines).map({
                      case ((StructMember3(name, memberType), memberIndex), sourceLine) => {
                        val index = struct.getMemberLlvmIndex(memberIndex)
                        val memberTypeName = TypeSculptor.getMemberTypeLlvmType(memberType)
                        "%line" + line + "member" + index + "ptr = getelementptr inbounds " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + index + "\n" +
                            "store " + memberTypeName + " %line" + sourceLine + ", " + memberTypeName + "* %line" + line + "member" + index + "ptr\n"
                      }
                    }).mkString("")
                  }
                })
          }
          case NewImmutableStruct3(line, structRef3, sourceLines) => {
            val llvmType = TypeSculptor.getConcreteLlvmType(structRef3)
            val typeName = structRef3.globalName
            val structDef3 = program3.structs.find(_.getRef() == structRef3).get;
            val sInfoPtrIndex = structDef3.getSInfoPtrElementIndex();
            "%line" + line + "sizeptr = getelementptr " + llvmType + ", " + llvmType + "* null, i64 1\n" +
                "%line" + line + "size = ptrtoint " + llvmType + "* %line" + line + "sizeptr to i64\n" +
                "%line" + line + "raw = call i8* @malloc(i64 %line" + line + "size)\n" +
                "%line" + line + " = bitcast i8* %line" + line + "raw to " + llvmType + "*\n" +
                "%line" + line + "vptrAddr = getelementptr " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + sInfoPtrIndex + "\n" +
                "store %__SInfo* getelementptr (%\"__SInfoWhole_" + typeName + "\", %\"__SInfoWhole_" + typeName + "\"* @\"__sInfoWhole_" + typeName + "\", i32 0, i32 1, i32 1), %__SInfo** %line" + line + "vptrAddr\n" +
                (structRef3 match {
                  case s : StructRef3 => {
                    val struct = program3.structs.find(_.getRef() == s).get;
                    struct.members.zipWithIndex.zip(sourceLines).map({
                      case ((StructMember3(name, memberType), memberIndex), sourceLine) => {
                        val index = struct.getMemberLlvmIndex(memberIndex)
                        val memberTypeName = TypeSculptor.getMemberTypeLlvmType(memberType)
                        "%line" + line + "member" + index + "ptr = getelementptr inbounds " + llvmType + ", " + llvmType + "* %line" + line + ", i32 0, i32 " + index + "\n" +
                            "store " + memberTypeName + " %line" + sourceLine + ", " + memberTypeName + "* %line" + line + "member" + index + "ptr\n"
                      }
                    }).mkString("")
                  }
                })
          }
          case InterfaceToInterfaceUpcast3(line, sourceStructRef, targetInterfaceRef, resultType, sourceLine) => {
            throw new RuntimeException("not yet")
          }
          case StructToInterfaceUpcast3(line, resultType, sourceStructRef, targetInterfaceRef, sourceLine, sourceLineType) => {
            val sourceStructDef = program3.structs.find(_.getRef() == sourceStructRef).get;
            val structInfoPointerIndex = sourceStructDef.getSInfoPtrElementIndex();
            val interfaceElementIndex = sourceStructDef.getInterfacePtrElementIndex(targetInterfaceRef);
            val interfaceId = program3.interfaces.find(_.getRef == targetInterfaceRef).get.interfaceId;

            val sourceConcreteTypeName = TypeSculptor.getConcreteLlvmType(sourceStructRef)
            val resultTypeName = targetInterfaceRef.globalName
            "%line" + line + "sInfoEndPtrPtr = getelementptr " + sourceConcreteTypeName + ", " + sourceConcreteTypeName + "* %line" + sourceLine + ", i32 0, i32 " + structInfoPointerIndex + "\n" +
            "%line" + line + "sInfoEndPtr = load %__SInfo*, %__SInfo** %line" + line + "sInfoEndPtrPtr\n" +
            "call void @__checkInterfaceCast(%__SInfo* %line" + line + "sInfoEndPtr, i32 " + interfaceId + ", %__IInfo* @\"__iinfo_" + resultTypeName + "\")\n" +
            "%line" + line + " = getelementptr " + sourceConcreteTypeName + ", " + sourceConcreteTypeName + "* %line" + sourceLine + ", i32 0, i32 " + interfaceElementIndex + "\n"
          }
          case InterfaceCall3(line, resultType, virtualParamIndex, interfaceRef3, interfaceId, indexInedge, functionType, argLines, paramTypes) => {
            "\n"
          }
//          case InterfaceMethodLookup3(line, resultType, interfaceRef3, interfaceLine, interfaceId, indexInEdge) => {
//            val interfaceName = interfaceRef3.globalName
//            val interfaceConcreteTypeName = TypeSculptor.getConcreteLlvmType(interfaceRef3)
//            val methodTypeName = TypeSculptor.getReferenceLlvmType(resultType)
//            "%line" + line + "sInfoEndPtrPtr = getelementptr " + interfaceConcreteTypeName + ", " + interfaceConcreteTypeName + "* %line" + interfaceLine + ", i32 0, i32 0, i32 0\n" +
//            "%line" + line + "sInfoEndPtr = load %__SInfo*, %__SInfo** %line" + line + "sInfoEndPtrPtr\n" +
//            "%line" + line + "generalEdgePtr = call %__Edge*(%__SInfo*, i32) @__getEdge(%__SInfo* %line" + line + "sInfoEndPtr, i32 " + interfaceId + ")\n" +
//            "%line" + line + "specificEdgePtr = bitcast %__Edge* %line" + line + "generalEdgePtr to %\"__Edge_" + interfaceName + "\"*\n"+
//            "%line" + line + "methodPtrAddr = getelementptr %\"__Edge_" + interfaceName + "\", %\"__Edge_" + interfaceName + "\"* %line" + line + "specificEdgePtr, i32 0, i32 " + (indexInEdge + 1) + "\n" +
//            "%line" + line + " = load " + methodTypeName + ", " + methodTypeName + "* %line" + line + "methodPtrAddr\n"
//          }
          case Panic3(line) => {
            "call void @exit(i32 7)\n" +
            "unreachable\n"
          }
        };
    assert(result.endsWith("\n"), "doesnt end with newline: " + result);
    result
  }
}