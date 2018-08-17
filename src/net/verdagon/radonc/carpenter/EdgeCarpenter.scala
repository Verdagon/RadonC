package net.verdagon.radonc.carpenter

import net.verdagon.radonc.templar._

import scala.collection.immutable.Map

case class InterfaceEdgeBlueprint(
    interface: InterfaceRef2,
    superFamilyRootBanners: List[FunctionBanner2])

// Someday soon, the EdgeCarpenter could be the one to make multimethods happen.
// A function family would contain all the combinations of subclasses, and the edge
// carpenter is a good place to make them into itables.

object EdgeCarpenter {
  def assembleEdges(
      program2: CompleteProgram2,
      superFamilyRootBannersByRootBanner: Map[FunctionBanner2, FunctionBanner2]):
  (Set[InterfaceEdgeBlueprint], Set[Edge2]) = {
    val edgeBlueprints =
      program2.getAllInterfaces.map(interface => makeEdgeBlueprint(program2, superFamilyRootBannersByRootBanner, interface));
    val edges =
      edgeBlueprints.flatMap(blueprint => makeEdgesForInterface(program2, blueprint))
    (edgeBlueprints, edges)
  }

  private def makeEdgeBlueprint(
      program2: CompleteProgram2,
      superFamilyRootBannersByRootBanner: Map[FunctionBanner2, FunctionBanner2],
      interface: InterfaceDefinition2):
  InterfaceEdgeBlueprint = {

    // matchingFamilyRoots is ordered (since it's a list); right here is where we declare
    // that this is the order of the functions in the edge.

    val superFamilies: Set[FunctionBanner2] = superFamilyRootBannersByRootBanner.values.toSet;

    val matchingFamilyRootBanners: List[FunctionBanner2] =
      superFamilies.flatMap(rootBanner => {
        val virtualRoots =
          rootBanner.params.filter(_.virtuality == Some(Virtual2())).collect({
            case Parameter2(_, Some(Virtual2()), Reference2(_, citizenRef : CitizenRef2)) => citizenRef
          })
        assert(virtualRoots.size == 1);
        if (virtualRoots.exists(virtualRoot => interface.inherits(program2, virtualRoot))) {
          List(rootBanner)
        } else {
          List()
        }
      }).toList;
    InterfaceEdgeBlueprint(interface.getRef, matchingFamilyRootBanners)
  }

  private def makeEdgesForInterface(
      program2: Program2,
      expectation: InterfaceEdgeBlueprint): List[Edge2] = {

    // Now, for every struct that implements the interface...
    val substructs =
        program2.getAllStructs.filter(_.implementsInterface(program2, expectation.interface));
    // ... we tell it to make an edge with these covariant families.
    substructs.map(substruct => {
      makeEdgeForStructForInterface(program2, substruct.getRef, expectation)
    }).toList
  }

  private def makeEdgeForStructForInterface(
      program2: Program2,
      structRef2: StructRef2,
      expectation: InterfaceEdgeBlueprint):
  Edge2 = {
    val prototypes =
      expectation.superFamilyRootBanners.map(superFamilyRootBanner => {
        val superFamily = program2.functionFamiliesByRootBanner(superFamilyRootBanner)
        val signatureForThisStruct = superFamily.memberSignaturesByVirtualRoots(List(structRef2))
        val prototypeForThisStruct = program2.lookupFunction(signatureForThisStruct).get.header.toPrototype;
        prototypeForThisStruct
      });
    Edge2(structRef2, expectation.interface, prototypes)
  }
}
