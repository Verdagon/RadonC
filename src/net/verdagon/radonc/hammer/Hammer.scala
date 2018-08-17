package net.verdagon.radonc.hammer

import net.verdagon.radonc.carpenter.{ETable2, Edge2, InterfaceEdgeBlueprint}
import net.verdagon.radonc.templar._

import scala.collection.immutable.{HashMap, ListMap, Range}

case class Hinputs(
    program2: Program2,
    superFamilyRootBannersByRootBanner: Map[FunctionBanner2, FunctionBanner2],
    superFamilyRootBannersBySignature: Map[Signature2, FunctionBanner2],
    edgeBlueprintsByInterface: Map[InterfaceRef2, InterfaceEdgeBlueprint],
    edgeBlueprintsByInterfaceId: Map[Int, InterfaceEdgeBlueprint],
    edges: Set[Edge2],
    functionIds: Map[Signature2, Int],
    structIds: Map[StructRef2, Int],
    interfaceIds: Map[InterfaceRef2, Int],
    eTables: Map[StructRef2, ETable2])

object Hammer {
  def translate(hinputs: Hinputs): Program3 = {
    val miduts0 = Hamuts(Map(), Map(), Map(), Map(), Map(), Map())
    val miduts1 = StructHammer.translateInterfaces(hinputs, miduts0);
    val miduts2 = StructHammer.translateStructs(hinputs, miduts1)
    val (miduts3, _) = FunctionHammer.translateFunctions(hinputs, miduts2, hinputs.program2.getAllFunctions.toList)

    Program3(
      miduts3.interfaceDefs.values.toList,
      miduts3.structDefs.values.toList,
      List() /* externs */,
      miduts3.functionDefs.values.toList)
  }

  def newId(nodesByLine: Vector[Node3]) = {
    "" + nodesByLine.size
  }
}
