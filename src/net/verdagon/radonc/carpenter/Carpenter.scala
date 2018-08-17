package net.verdagon.radonc.carpenter

import net.verdagon.radonc.hammer.Hinputs
import net.verdagon.radonc.templar.TypeTemplar.getCovariantParamIndices
import net.verdagon.radonc.templar._

import scala.collection.immutable.Set

case class Edge2(
    struct: StructRef2,
    interface: InterfaceRef2,
    methods: List[Prototype2])

object Carpenter {
  def translate(program2: CompleteProgram2): Hinputs = {
    val superFamilyRootBannersByRootBanner = SuperFamilyCarpenter.getSuperFamilyMap(program2)

    val superFamilyRootBannersBySignature =
      superFamilyRootBannersByRootBanner.flatMap({
        case (rootBanner, superFamilyRootBanner) => {
          val family = program2.functionFamiliesByRootBanner(rootBanner)
          val memberSignatures = family.memberSignaturesByVirtualRoots.values
          memberSignatures.map(memberSignature => (memberSignature -> superFamilyRootBanner)).toList
        }
      })

    val (edgeBlueprints, edges) = EdgeCarpenter.assembleEdges(program2, superFamilyRootBannersByRootBanner)
    val edgeBlueprintsByInterface = edgeBlueprints.map(_.interface).zip(edgeBlueprints).toMap;


    val functionIdsBySignature: Map[Signature2, Int] =
      program2.getAllFunctions.map(_.header.toSignature).zipWithIndex.toMap

    val interfaceRefs: Set[InterfaceRef2] = program2.getAllInterfaces.map(_.getRef).toSet
    val interfaceIdsByInterface: Map[InterfaceRef2, Int] = interfaceRefs.zipWithIndex.toMap
    val structIdStart = interfaceIdsByInterface.size

    val edgeBlueprintsByInterfaceId =
      edgeBlueprints.map(_.interface).map(interfaceIdsByInterface).zip(edgeBlueprints).toMap

    val structRefs: Set[StructRef2] = program2.getAllStructs.map(_.getRef).toSet
    val structIds = structIdStart until (structIdStart + structRefs.size)
    val structIdsByStruct: Map[StructRef2, Int] = structRefs.zip(structIds).toMap

    val etablesByStructRef = ETableGenerator.generateETables(interfaceIdsByInterface, edges)

    Hinputs(
      program2,
      superFamilyRootBannersByRootBanner,
      superFamilyRootBannersBySignature,
      edgeBlueprintsByInterface,
      edgeBlueprintsByInterfaceId,
      edges,
      functionIdsBySignature,
      structIdsByStruct,
      interfaceIdsByInterface,
      etablesByStructRef)
  }
}
