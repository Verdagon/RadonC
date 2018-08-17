package net.verdagon.radonc.hammer

import net.verdagon.radonc.scout.{Immutable, Mutable}
import net.verdagon.radonc.templar._

object CallHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def addLine(nodesByLine: Vector[Node3], node: Node3): (Vector[Node3], Node3) = {
    (nodesByLine :+ node, node)
  }

  def translateExternFunctionCall(hinputs: Hinputs, hamuts0: Hamuts, localsByName0: Map[String, Node3], nodesByLine0: Vector[Node3], prototype2: Prototype2, argsExprs2: List[ReferenceExpression2]): (Hamuts, Map[String, Node3], Vector[Node3], String) = {
    val (hamuts1, paramTypes) =
      TypeHammer.translateReferences(hinputs, hamuts0, prototype2.functionType.paramTypes);

    val (hamuts2, argsExportedLocals, nodesByLine1, argsResultLines) =
      ExpressionHammer.translateExpressions(hinputs, hamuts1, localsByName0, nodesByLine0, argsExprs2, false);
    val (hamuts3, functionRef3) =
      FunctionHammer.translateFunctionRef(hinputs, hamuts2, prototype2);

    val (hamuts4, resultType3) =
      TypeHammer.translateReference(hinputs, hamuts3, prototype2.functionType.returnType);

    val (nodesByLine3, callResultNode) =
      addLine(
        nodesByLine1,
        ExternCall3(
          newId(nodesByLine1),
          resultType3,
          functionRef3,
          argsResultLines,
          paramTypes));
    (hamuts4, argsExportedLocals, nodesByLine3, callResultNode.registerId)
  }

  def translateFunctionPointerCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      callableExpr: ReferenceExpression2,
      args: List[Expression2],
      resultType2: Reference2) = {
    val Reference2(Raw, ft2 @ FunctionT2(paramTypes, returnType)) = callableExpr.resultRegister.reference
    val (hamuts1, ft3) = TypeHammer.translateFunction(hinputs, hamuts0, ft2)
    val (hamuts2, paramTypes3) = TypeHammer.translateReferences(hinputs, hamuts1, paramTypes)
    val (hamuts4, argsExportedLocals, nodesByLine1, argLines) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts2, localsByName0, nodesByLine0, args, false);

    val (hamuts5, resultType3) = TypeHammer.translateReference(hinputs, hamuts4, resultType2);

    val (hamuts6, callableExprExportedLocals, nodesByLine2, functionLine) =
      ExpressionHammer.translate(hinputs, hamuts5, localsByName0, nodesByLine1, callableExpr)

    val (nodesByLine3, callResultNode) =
      addLine(
        nodesByLine2,
        Call3(
          newId(nodesByLine2),
          resultType3,
          functionLine,
          ft3,
          argLines,
          paramTypes3));

    (hamuts6, argsExportedLocals, nodesByLine3, callResultNode.registerId)
  }

  def translateConstructArray(
      hinputs: Hinputs, hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      constructArray2: ConstructArray2):
  (Hamuts, Map[String, Node3], Vector[Node3], String) = {
    val ConstructArray2(arrayType2, sizeExpr2, callExpr2) = constructArray2;

    val (hamuts1, argsExportedLocals, nodesByLine1, sizeRegisterId) =
      ExpressionHammer.translate(
        hinputs, hamuts0, localsByName0, nodesByLine0, sizeExpr2);

    val FunctionPointerCall2(callableExpr2, argsExprs2) = callExpr2
    val (hamuts2, _, nodesByLine2, generatorCallableResultLine) =
      ExpressionHammer.translate(
        hinputs, hamuts1, localsByName0, nodesByLine1, callableExpr2);
    val (hamuts3, generatorFunctionType3) =
      TypeHammer.translateFunction(hinputs, hamuts2, callExpr2.functionType)
    val (hamuts4, generatorParamTypes3) =
      TypeHammer.translateReferences(
        hinputs, hamuts3, callExpr2.args.map(_.resultRegister.reference))
    val (hamuts5, _, nodesByLine5, generatorArgsLines) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts4, localsByName0, nodesByLine2, argsExprs2, false);

    val (hamuts6, arrayRefType3) =
      TypeHammer.translateReference(
        hinputs, hamuts5, constructArray2.resultRegister.reference)

    val (hamuts7, arrayType3) =
      TypeHammer.translateArray(hinputs, hamuts6, arrayType2)

    val (nodesByLine10, constructArrayCallNode) =
      addLine(nodesByLine5,
        ConstructArrayCall3(
          newId(nodesByLine5),
          sizeRegisterId,
          arrayRefType3,
          arrayType3,
          generatorCallableResultLine,
          generatorFunctionType3,
          generatorArgsLines,
          generatorParamTypes3))

    (hamuts7, Map(), nodesByLine10, constructArrayCallNode.registerId)
  }

  def translateIf(
      hinputs: Hinputs, hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      if2: If2):
  (Hamuts, Map[String, Node3], Vector[Node3], String) = {

    val If2(condition2, trueCall2, falseCall2) = if2

    val (hamuts1, exportedLocals, nodesByLine1, conditionResultLine) =
      ExpressionHammer.translate(hinputs, hamuts0, localsByName0, nodesByLine0, condition2);

    val FunctionPointerCall2(trueCallableExpr2, trueArgsExprs2) = trueCall2
    val (hamuts2, _, nodesByLine2, trueCallableResultLine) =
      ExpressionHammer.translate(hinputs, hamuts1, localsByName0, nodesByLine1, trueCallableExpr2);
    val (hamuts3, trueFunctionType3) = TypeHammer.translateFunction(hinputs, hamuts2, trueCall2.functionType)
    val (hamuts4, trueParamTypes3) = TypeHammer.translateReferences(hinputs, hamuts3, trueCall2.args.map(_.resultRegister.reference))
    val (hamuts5, _, nodesByLine5, trueArgsLines) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts4, localsByName0, nodesByLine2, trueArgsExprs2, false);

    val FunctionPointerCall2(falseCallableExpr2, falseArgsExprs2) = falseCall2
    val (hamuts6, _, nodesByLine6, falseCallableResultLine) =
      ExpressionHammer.translate(hinputs, hamuts5, localsByName0, nodesByLine5, falseCallableExpr2);
    val (hamuts7, falseFunctionType3) = TypeHammer.translateFunction(hinputs, hamuts6, falseCall2.functionType)
    val (hamuts8, falseParamTypes3) = TypeHammer.translateReferences(hinputs, hamuts7, falseCall2.args.map(_.resultRegister.reference))
    val (hamuts9, _, nodesByLine9, falseArgsLines) =
      ExpressionHammer.translateExpressions(
        hinputs, hamuts8, localsByName0, nodesByLine6, falseArgsExprs2, false);

    val (hamuts10, resultType3) =
      TypeHammer.translateReference(hinputs, hamuts9, if2.resultRegister.reference)

    val (nodesByLine10, ifCallNode) =
      addLine(nodesByLine9,
        IfCall3(
          newId(nodesByLine9),
          conditionResultLine,
          resultType3,
          trueCallableResultLine,
          trueFunctionType3,
          trueArgsLines,
          trueParamTypes3,
          falseCallableResultLine,
          falseFunctionType3,
          falseArgsLines,
          falseParamTypes3))

    (hamuts10, Map(), nodesByLine10, ifCallNode.registerId)
  }

  def translateInterfaceFunctionCall(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      subFamilyRootBanner: FunctionBanner2,
      functionTypeReference2: Reference2,
      resultType2: Reference2,
      argsExprs2: List[Expression2]):
  (Hamuts, Map[String, Node3], Vector[Node3], String) = {
    // The function family that this interface call was originally for may have gotten
    // merged into a different family (or multiple).
    // We are guaranteed that the family it merged into can handle all uses of this though.
    val superFamilyRootBanner = hinputs.superFamilyRootBannersBySignature(subFamilyRootBanner.toSignature)
    assert(superFamilyRootBanner.params.size == argsExprs2.size);

    val (hamuts1, argsExportedLocals, nodesByLine1, argLines) =
      ExpressionHammer.translateExpressions(hinputs, hamuts0, localsByName0, nodesByLine0, argsExprs2, false);

    val functionType2 =
      functionTypeReference2 match {
        case Reference2(Raw, ft2 @ FunctionT2(_, _)) => ft2
      }
    val (hamuts1b, functionType3) = TypeHammer.translateFunction(hinputs, hamuts1, functionType2);

    val (hamuts2, resultType3) =
      TypeHammer.translateReference(hinputs, hamuts1b, resultType2);

    superFamilyRootBanner.params.zipWithIndex.collectFirst({
      case (Parameter2(_, Some(_), ref2 @ Reference2(_, interfaceRef2 : InterfaceRef2)), paramIndex) => {
        val (hamuts3, interfaceRef3) =
          StructHammer.translateInterfaceRef(hinputs, hamuts2, interfaceRef2)
        val (hamuts4, paramTypes3) =
          TypeHammer.translateReferences(
            hinputs, hamuts3, superFamilyRootBanner.paramTypes)

        val (hamuts6, nodesByLine2, functionNodeLine) =
          translateInterfaceFunctionCallWithInterface(
            hinputs,
            hamuts4,
            localsByName0,
            nodesByLine1,
            superFamilyRootBanner,
            paramIndex,
            interfaceRef3,
            functionType3,
            resultType3,
            argLines,
            paramTypes3)
        (hamuts6, localsByName0, nodesByLine2, functionNodeLine)
      }
      case (Parameter2(_, Some(_), Reference2(_, structRef2@ StructRef2(_, _))), _) => {
        val (hamuts2, nodesByLine1, functionNodeLine) =
          translateInterfaceFunctionLookupWithStruct(
            hinputs,
            hamuts1,
            nodesByLine0,
            structRef2,
            superFamilyRootBanner)
        val (hamuts3, paramTypes3) =
          TypeHammer.translateReferences(
            hinputs, hamuts2, superFamilyRootBanner.paramTypes)
        val (nodesByLine3, callResultNode) =
          addLine(
            nodesByLine1,
            Call3(
              newId(nodesByLine1),
              resultType3,
              functionNodeLine,
              functionType3,
              argLines,
              paramTypes3));
        (hamuts3, localsByName0, nodesByLine3, callResultNode.registerId)
      }
    }).get
  }

  private def translateInterfaceFunctionLookupWithStruct(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      nodesByLine0: Vector[Node3],
      structRef2: StructRef2,
      superFamilyRootBanner: FunctionBanner2):
  (Hamuts, Vector[Node3], String) = {
    val prototype2 =
      getPrototypeForStructInterfaceCall(hinputs, structRef2, superFamilyRootBanner)

    val (hamuts1, functionRef3) = FunctionHammer.translateFunctionRef(hinputs, hamuts0, prototype2);
    val (nodesByLine1, functionNode) =
      addLine(
        nodesByLine0,
        LoadFunction3(newId(nodesByLine0), functionRef3));
    (hamuts1, nodesByLine1, functionNode.registerId)
  }

  private def translateInterfaceFunctionCallWithInterface(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      localsByName0: Map[String, Node3],
      nodesByLine0: Vector[Node3],
      superFamilyRootBanner: FunctionBanner2,
      firstVirtualParamIndex: Int,
      firstVirtualParamInterface: InterfaceRef3,
      functionType3: FunctionT3,
      resultType3: Reference3,
      argLines: List[String],
      paramTypes3: List[Reference3]):
  (Hamuts, Vector[Node3], String) = {

    val interfaceId = firstVirtualParamInterface.interfaceId

    val edgeBlueprint =
      hinputs.edgeBlueprintsByInterfaceId(interfaceId)
    val indexInEdge =
      edgeBlueprint.superFamilyRootBanners.indexOf(superFamilyRootBanner)
    if (indexInEdge < 0) {
      throw new RuntimeException("Can't find:\n" + superFamilyRootBanner + "\nin:\n" + edgeBlueprint.interface)
    }

    val (nodesByLine2, methodNode) =
      addLine(
        nodesByLine0,
        InterfaceCall3(
          newId(nodesByLine0),
          resultType3,
          firstVirtualParamIndex,
          firstVirtualParamInterface,
          interfaceId,
          indexInEdge,
          functionType3,
          argLines,
          paramTypes3));

    (hamuts0, nodesByLine2, methodNode.registerId)
  }

  private def getPrototypeForStructInterfaceCall(
      hinputs: Hinputs,
      structRef2: StructRef2,
      superFamilyRootBanner: FunctionBanner2):
  Prototype2 = {

    val structDef2 = hinputs.program2.lookupStruct(structRef2)
    val ancestorInterfaces2 =
      structDef2.getAncestorInterfacesNotIncludingSelf(hinputs.program2);
    val edgeBlueprints = ancestorInterfaces2.map(hinputs.edgeBlueprintsByInterface)
    val matchingEdgeBlueprint =
      edgeBlueprints.find(_.superFamilyRootBanners.contains(superFamilyRootBanner)).get;

    val indexInEdgeBlueprint = matchingEdgeBlueprint.superFamilyRootBanners.indexOf(superFamilyRootBanner);
    assert(indexInEdgeBlueprint >= 0);

    val edge =
      hinputs.edges.find(
        edge => edge.interface == matchingEdgeBlueprint.interface && edge.struct == structRef2).get;
    val methodPrototype2 = edge.methods(indexInEdgeBlueprint)
    methodPrototype2
  }

}
