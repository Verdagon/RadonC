package net.verdagon.radonc.hammer

import net.verdagon.radonc.scout.{Immutable, Mutable}
import net.verdagon.radonc.templar._

import scala.collection.immutable.Range

object ExpressionHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def translate(
      hinputs: Hinputs, hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      expr2: Expression2):
      (Hamuts, Map[String, Node3], Vector[Node3], String) = {
    expr2 match {
      case IntLiteral2(value) => {
        val (nodesByLine1, resultNode) =
          addLine(nodesByLine0, ConstantI643(newId(nodesByLine0), value));
        (hamuts0, localsByName0, nodesByLine1, resultNode.registerId)
      }
      case StrLiteral2(value) => {
        val (nodesByLine1, resultNode) =
          addLine(nodesByLine0, ConstantStr3(newId(nodesByLine0), value));
        (hamuts0, localsByName0, nodesByLine1, resultNode.registerId)
      }
      case FloatLiteral2(value) => {
        val (nodesByLine1, resultNode) =
            addLine(nodesByLine0, ConstantF643(newId(nodesByLine0), value));
        (hamuts0, localsByName0, nodesByLine1, resultNode.registerId)
      }
      case BoolLiteral2(value) => {
        val (nodesByLine1, resultNode) =
            addLine(nodesByLine0, ConstantBool3(newId(nodesByLine0), value));
        (hamuts0, localsByName0, nodesByLine1, resultNode.registerId)
      }
      case let2 @ Let2(name, mutable, expr2) => {
        val (hamuts1, localsByName1, nodesByLine1, exprResultLine) =
            translate(hinputs, hamuts0, localsByName0, nodesByLine0, expr2);
        val resultReference = expr2.resultRegister
        val (hamuts2, resultBaseType3) =
          TypeHammer.translateReference(hinputs, hamuts1, resultReference.reference)
        val resultPointerType3 = resultBaseType3.asInstanceOf[Reference3]
        val (nodesByLine2, stackNode) =
          addLine(
            nodesByLine1,
            Stackify3(newId(nodesByLine1), resultPointerType3, exprResultLine, name))
        val localsByName2 = localsByName1 + (name -> stackNode);

        val (hamuts3, nodesByLine3, newEmptyPackStructNodeLine) =
          makeEmptyPackStruct(hinputs, hamuts2, nodesByLine2)

        (hamuts3, localsByName2, nodesByLine3, newEmptyPackStructNodeLine)
      }
      case Mutate2(mutableExpr2, sourceExpr2) => {
        val (hamuts1, localsByName1, nodesByLine1, destinationResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, mutableExpr2);
        val (hamuts2, localsByName2, nodesByLine2, sourceResultLine) =
            translate(hinputs, hamuts1, localsByName1, nodesByLine1, sourceExpr2);
        val (hamuts3, varBaseType) =
          TypeHammer.translateReference(hinputs, hamuts2, sourceExpr2.resultRegister.reference)
        val varPointerType = varBaseType.asInstanceOf[Reference3]
        val newStoreNode =
          Store3(newId(nodesByLine2), varPointerType, destinationResultLine, sourceResultLine)
        val (nodesByLine3, addedStoreNode) = addLine(nodesByLine2, newStoreNode);

        val (hamuts4, nodesByLine4, newEmptyPackStructNodeLine) =
          makeEmptyPackStruct(hinputs, hamuts3, nodesByLine3)

        (hamuts4, localsByName2, nodesByLine4, newEmptyPackStructNodeLine)
      }
      case LocalLookup2(name, tyype) => {
        localsByName0.get(name) match {
          case None => {
            throw new RuntimeException("cant find local: " + name)
          }
          case Some(node) => {
            (hamuts0, localsByName0, nodesByLine0, node.registerId)
          }
        }
      }
      case AddressMemberLookup2(elementName, innerExpr, resultType) => {
        val (hamuts1, localsByName1, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, innerExpr);
        val innerExprResultPointerType2 = innerExpr.resultRegister.expectReference().reference
        val (hamuts2, innerExprResultPointerType3) =
          TypeHammer.translateReference(hinputs, hamuts1, innerExprResultPointerType2)

        innerExpr.resultRegister.reference.referend match {
          case structRef2@ StructRef2(_, _) => {
            val structDef3 = hamuts1.structDefs(structRef2);
            val (tyype, index) = structDef3.getTypeAndIndex(elementName);
            val resultMemberType = tyype.expectAddressMember()
            val (nodesByLine2, structLookupNode) =
              addLine(
                nodesByLine1,
                AddressMemberLookup3(
                  newId(nodesByLine1),
                  resultMemberType.reference,
                  innerExprResultLine,
                  innerExprResultPointerType3,
                  structDef3.getRef(),
                  index));
            (hamuts2, localsByName1, nodesByLine2, structLookupNode.registerId)
          }
        };
      }
      case ReferenceMemberLookup2(elementName, innerExpr, resultType) => {
        val (hamuts1, localsByName1, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, innerExpr);
        val structPointerType2 = innerExpr.resultRegister.reference
        val (hamuts2, structPointerType3) = TypeHammer.translateReference(hinputs, hamuts1, structPointerType2)
        innerExpr.resultRegister.reference.referend match {
          case PackT2(_, structRef2 @ StructRef2(_, _)) => {
            val (hamuts3, structRef3) = StructHammer.translateStructRef(hinputs, hamuts2, structRef2)
            val structDef3 = hamuts3.structDefs(structRef2);
            val (tyype, index) = structDef3.getTypeAndIndex(elementName);
            val memberType2 = tyype.expectReferenceMember()
            val (nodesByLine2, structLookupNode) =
              addLine(nodesByLine1, StructLookup3(newId(nodesByLine1), memberType2.reference, innerExprResultLine, structPointerType3, structDef3.getRef(), index));
            (hamuts3, localsByName1, nodesByLine2, structLookupNode.registerId)
          }
          case TupleT2(_, structRef2 @ StructRef2(_, _)) => {
            val (hamuts3, structRef3) = StructHammer.translateStructRef(hinputs, hamuts2, structRef2)
            val structDef3 = hamuts3.structDefs(structRef2);
            val (tyype, index) = structDef3.getTypeAndIndex(elementName);
            val memberType2 = tyype.expectReferenceMember()
            val (nodesByLine2, structLookupNode) =
              addLine(nodesByLine1, StructLookup3(newId(nodesByLine1), memberType2.reference, innerExprResultLine, structPointerType3, structDef3.getRef(), index));
            (hamuts3, localsByName1, nodesByLine2, structLookupNode.registerId)
          }
          case structRef2 @ StructRef2(_, _) => {
            val (hamuts3, structRef3) = StructHammer.translateStructRef(hinputs, hamuts2, structRef2)
            val structDef3 = hamuts3.structDefs(structRef2);
            val (tyype, index) = structDef3.getTypeAndIndex(elementName);
            val memberType2 = tyype.expectReferenceMember()
            val (nodesByLine2, structLookupNode) =
              addLine(nodesByLine1, StructLookup3(newId(nodesByLine1), memberType2.reference, innerExprResultLine, structPointerType3, structDef3.getRef(), index));
            (hamuts3, localsByName1, nodesByLine2, structLookupNode.registerId)
          }
        };
      }
      case ArrayLookup2(arrayExpr2, arrayType, indexExpr) => {

        val (hamuts1, localsByName1, nodesByLine1, arrayExprResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, arrayExpr2);
        val arrayReference2 = arrayExpr2.resultRegister.reference
        val (hamuts2, arrayReference3) =
          TypeHammer.translateReference(hinputs, hamuts1, arrayReference2)
        val (hamuts3, arrayType3) =
          TypeHammer.translateArray(hinputs, hamuts2, arrayType)
        val (hamuts4, localsByName2, nodesByLine2, indexExprResultLine) =
          translate(hinputs, hamuts3, localsByName1, nodesByLine1, indexExpr);

        val (nodesByLine3, elementLookupNode) =
          addLine(
            nodesByLine2,
            ElementLookup3(
              newId(nodesByLine2),
              arrayType3.memberType,
              arrayExprResultLine,
              arrayReference3,
              arrayType3,
              indexExprResultLine));
        (hamuts4, localsByName2, nodesByLine3, elementLookupNode.registerId)
      }
      case Block2(exprs) => {
        val (hamuts3, exprsLocals, nodesByLine3, lastExprResultLine) =
            exprs.foldLeft((hamuts0, localsByName0, nodesByLine0, ""))({
              case ((hamuts1, previousLocals, nodesByLine1, _), expr) => {
                val (hamuts2, exprLocals, nodesByLine2, exprResultLine) =
                    translate(hinputs, hamuts1, localsByName0 ++ previousLocals, nodesByLine1, expr)
                (hamuts2, previousLocals ++ exprLocals, nodesByLine2, exprResultLine)
              }
            });
        assert(lastExprResultLine.nonEmpty);
        (hamuts3, localsByName0 ++ exprsLocals, nodesByLine3, lastExprResultLine)
      }
      case FunctionLookup2(prototype2) => {
        val (hamuts1, functionRef3) = FunctionHammer.translateFunctionRef(hinputs, hamuts0, prototype2);
        val (nodesByLine1, functionNode) =
            addLine(
              nodesByLine0,
              LoadFunction3(newId(nodesByLine0), functionRef3))
        (hamuts1, localsByName0, nodesByLine1, functionNode.registerId)
      }
      case funcPtrCall2 @ FunctionPointerCall2(callableExpr, args) => {
        CallHammer.translateFunctionPointerCall(
          hinputs, hamuts0, localsByName0, nodesByLine0, callableExpr, args, funcPtrCall2.resultRegister.reference)
      }

      case InterfaceFunctionCall2(subFamilyRootBanner, functionType2, resultType2, argsExprs2) => {
        CallHammer.translateInterfaceFunctionCall(
          hinputs, hamuts0, localsByName0, nodesByLine0, subFamilyRootBanner, functionType2, resultType2, argsExprs2)
      }

      case PackE2(exprs, resultType, resultPackType) => {
        val (hamuts1, exprsLocals, nodesByLine7, resultLines) =
          translateExpressions(hinputs, hamuts0, localsByName0, nodesByLine0, exprs, false);
        val (hamuts2, underlyingStructRef3) =
          StructHammer.translateStructRef(hinputs, hamuts1, resultPackType.underlyingStruct);
        val packStructDef2 =
          hinputs.program2.lookupStruct(resultPackType.underlyingStruct)
        val newStructNode =
          packStructDef2.mutability match {
            case Mutable => NewMutableStruct3(newId(nodesByLine7), underlyingStructRef3, resultLines)
            case Immutable => NewImmutableStruct3(newId(nodesByLine7), underlyingStructRef3, resultLines)
          }
        val (nodesByLine8, _) = addLine(nodesByLine7, newStructNode);
        // Export locals from inside the pack
        (hamuts2, exprsLocals, nodesByLine8, newStructNode.registerId)
      }

      case TupleE2(exprs, resultType, resultPackType) => {
        val (hamuts1, exprsLocals, nodesByLine7, resultLines) =
          translateExpressions(hinputs, hamuts0, localsByName0, nodesByLine0, exprs, false);
        val (hamuts2, underlyingStructRef3) =
          StructHammer.translateStructRef(hinputs, hamuts1, resultPackType.underlyingStruct);
        val packStructDef2 =
          hinputs.program2.lookupStruct(resultPackType.underlyingStruct)
        val newStructNode =
          packStructDef2.mutability match {
            case Mutable => NewMutableStruct3(newId(nodesByLine7), underlyingStructRef3, resultLines)
            case Immutable => NewImmutableStruct3(newId(nodesByLine7), underlyingStructRef3, resultLines)
          }
        val (nodesByLine8, _) = addLine(nodesByLine7, newStructNode);
        // Export locals from inside the pack
        (hamuts2, exprsLocals, nodesByLine8, newStructNode.registerId)
      }

      case ArraySequenceE2(exprs, arrayReference2, arrayType2) => {
        val (hamuts1, exprsLocals, nodesByLine7, resultLines) =
          translateExpressions(hinputs, hamuts0, localsByName0, nodesByLine0, exprs, false);
        val (hamuts2, underlyingArray3) =
          TypeHammer.translateArray(hinputs, hamuts1, arrayType2.array);
        val newStructNode =
          arrayType2.array.mutability match {
            case Mutable => NewMutableArrayFromValues3(newId(nodesByLine7), underlyingArray3.memberType, resultLines)
            case Immutable => NewImmutableArrayFromValues3(newId(nodesByLine7), underlyingArray3.memberType, resultLines)
          }
        val (nodesByLine8, _) = addLine(nodesByLine7, newStructNode);
        (hamuts2, exprsLocals, nodesByLine8, newStructNode.registerId)
      }

      case Construct2(resultStructType2, resultType2, memberExprs) => {
        val (hamuts1, exportedLocals, nodesByLine1, memberResultLines) =
          translateExpressions(hinputs, hamuts0, localsByName0, nodesByLine0, memberExprs, false);

        val structDef2 = hinputs.program2.lookupStruct(resultStructType2)
        val newStructNode =
          structDef2.mutability match {
            case Mutable => NewMutableStruct3(newId(nodesByLine1), hamuts1.structRefs(resultStructType2), memberResultLines)
            case Immutable => NewImmutableStruct3(newId(nodesByLine1), hamuts1.structRefs(resultStructType2), memberResultLines)
          }

        val (nodesByLine2, _) = addLine(nodesByLine1, newStructNode);
        (hamuts1, exportedLocals, nodesByLine2, newStructNode.registerId)
      }

      case m @ SoftLoad2(innerExpr, targetOwnership) => {
        val (hamuts1, exportedLocals, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, innerExpr);
        val (hamuts2, innerExprResultType) =
          TypeHammer.translateReference(hinputs, hamuts1, innerExpr.resultRegister.reference)
        val (hamuts3, resultType3) =
          TypeHammer.translateReference(hinputs, hamuts2, m.resultRegister.reference)
        val (nodesByLine2, derefAndCopyNode) =
          addLine(nodesByLine1, SoftLoad3(newId(nodesByLine1), Reference3(targetOwnership, resultType3.innerType), innerExprResultLine, innerExprResultType));
        (hamuts2, exportedLocals, nodesByLine2, derefAndCopyNode.registerId)
      }

      case if2 @ If2(_, _, _) => {
        CallHammer.translateIf(hinputs, hamuts0, localsByName0, nodesByLine0, if2)
      }

      case ca2 @ ConstructArray2(_, _, _) => {
        CallHammer.translateConstructArray(
          hinputs, hamuts0, localsByName0, nodesByLine0, ca2)
      }

      case TemplarReinterpret2(innerExpr, resultType2) => {
        // Check types; it's overkill because reinterprets are rather scary.
        val innerExprResultType2 = innerExpr.resultRegister.reference
        val (hamuts1, innerExprResultType3) = TypeHammer.translateReference(hinputs, hamuts0, innerExprResultType2);
        val (hamuts2, resultType3) = TypeHammer.translateReference(hinputs, hamuts1, resultType2);
        assert(innerExprResultType3 == resultType3, innerExprResultType3  + " doesnt match " + resultType3);

        // Just evaluate the inner expr and return it directly. A TemplarReinterpret2 is
        // just a passthrough to Midas.
        translate(hinputs, hamuts2, localsByName0, nodesByLine0, innerExpr)
      }

      case lend @ Alias2(innerExpr, targetOwnership) => {
        val (hamuts1, localsByName1, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, innerExpr)
        val (hamuts2, innerExprResultType3) =
          TypeHammer.translateReference(hinputs, hamuts1, innerExpr.resultRegister.reference);
        val (hamuts3, resultType3) =
          TypeHammer.translateReference(hinputs, hamuts2, lend.resultRegister.reference);

        val structType = resultType3.innerType.asInstanceOf[StructRef3]

        val (nodesByLine2, lendNode) =
          addLine(nodesByLine1, Alias3(newId(nodesByLine1), resultType3, structType, innerExprResultLine, innerExprResultType3));
        (hamuts3, localsByName1, nodesByLine2, lendNode.registerId)
      }

      case up @ InterfaceToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (hamuts1, sourcePointerType3) =
          TypeHammer.translateReference(hinputs, hamuts0, sourcePointerType2);
        val (hamuts2, targetPointerType3) =
          TypeHammer.translateReference(hinputs, hamuts1, targetPointerType2);

        val sourceStructRef3 = sourcePointerType3.innerType.asInstanceOf[InterfaceRef3]
        val targetInterfaceRef3 = targetPointerType3.innerType.asInstanceOf[InterfaceRef3]

        val (hamuts3, localsByName1, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts2, localsByName0, nodesByLine0, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val (nodesByLine2, upcastNode) =
          addLine(nodesByLine1, InterfaceToInterfaceUpcast3(newId(nodesByLine1), targetPointerType3, sourceStructRef3, targetInterfaceRef3, innerExprResultLine));
        (hamuts3, localsByName1, nodesByLine2, upcastNode.registerId)
      }

      case up @ StructToInterfaceUpcast2(innerExpr, targetInterfaceRef2) => {
        val targetPointerType2 = up.resultRegister.reference;
        val sourcePointerType2 = innerExpr.resultRegister.reference

        val (hamuts1, sourcePointerType3) =
          TypeHammer.translateReference(hinputs, hamuts0, sourcePointerType2);
        val (hamuts2, targetPointerType3) =
          TypeHammer.translateReference(hinputs, hamuts1, targetPointerType2);

        val sourceStructRef3 = sourcePointerType3.innerType.asInstanceOf[StructRef3]
        val targetInterfaceRef3 = targetPointerType3.innerType.asInstanceOf[InterfaceRef3]

        val (hamuts3, localsByName1, nodesByLine1, innerExprResultLine) =
          translate(hinputs, hamuts2, localsByName0, nodesByLine0, innerExpr);
        // Upcasting an interface is technically a no-op with our language, but the sculptor
        // will still want to do it to do some checking in debug mode.
        val (nodesByLine2, upcastNode) =
          addLine(nodesByLine1, StructToInterfaceUpcast3(newId(nodesByLine1), targetPointerType3, sourceStructRef3, targetInterfaceRef3, innerExprResultLine, sourcePointerType3));
        (hamuts3, localsByName1, nodesByLine2, upcastNode.registerId)
      }

      case ExternFunctionCall2(prototype2, argsExprs2) => {
        CallHammer.translateExternFunctionCall(hinputs, hamuts0, localsByName0, nodesByLine0, prototype2, argsExprs2)
      }

      case if2 @ If2(_, _, _) => {
        CallHammer.translateIf(hinputs, hamuts0, localsByName0, nodesByLine0, if2)
      }

      case Panic2() => {
        val (nodesByLine1, panicNode) =
          addLine(nodesByLine0, Panic3(newId(nodesByLine0)));
        (hamuts0, localsByName0, nodesByLine1, panicNode.registerId)
      }

      case _ => throw new RuntimeException("wat " + expr2)
    }
  }

  def translateExpressions(
      hinputs: Hinputs, hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      exprs2: List[Expression2],
      subsequentExprsHaveAccessToLocalsFromPrevious: Boolean):
      (Hamuts, Map[String, Node3], Vector[Node3], List[String]) = {
    exprs2 match {
      case firstExpr :: restExprs => {
        val (hamuts1, firstExportedLocals, nodesByLine1, firstResultLine) =
          translate(hinputs, hamuts0, localsByName0, nodesByLine0, firstExpr);
        val localsByNameForRest =
          if (subsequentExprsHaveAccessToLocalsFromPrevious) {
            localsByName0 ++ firstExportedLocals
          } else {
            localsByName0
          };
        val (hamuts2, restExportedLocals, nodesByLine2, restResultLines) =
          translateExpressions(hinputs, hamuts1, localsByNameForRest, nodesByLine1, restExprs, subsequentExprsHaveAccessToLocalsFromPrevious);

        val localsByName1 = localsByName0 ++ firstExportedLocals ++ restExportedLocals
        val resultLines = firstResultLine :: restResultLines
        (hamuts2, localsByName1, nodesByLine2, resultLines)
      }
      case Nil => (hamuts0, Map(), nodesByLine0, List())
    }
  }

  def addLine(nodesByLine: Vector[Node3], node: Node3): (Vector[Node3], Node3) = {
    (nodesByLine :+ node, node)
  }

  def makeEmptyPackStruct(hinputs: Hinputs, hamuts0: Hamuts, nodesByLine0: Vector[Node3]):
  (Hamuts, Vector[Node3], String) = {
    val emptyPackType = PackTemplar.emptyPackType
    val (hamuts1, underlyingStructRef3) =
      StructHammer.translateStructRef(hinputs, hamuts0, emptyPackType.underlyingStruct);
    val newEmptyStructNode =
      hinputs.program2.lookupStruct(emptyPackType.underlyingStruct).mutability match {
        case Mutable => NewMutableStruct3(newId(nodesByLine0), underlyingStructRef3, List())
        case Immutable => NewImmutableStruct3(newId(nodesByLine0), underlyingStructRef3, List())
      }
    val (nodesByLine1, _) = addLine(nodesByLine0, newEmptyStructNode);
    (hamuts1, nodesByLine1, newEmptyStructNode.registerId)
  }
}
