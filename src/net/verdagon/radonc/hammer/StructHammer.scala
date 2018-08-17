package net.verdagon.radonc.hammer

import net.verdagon.radonc.carpenter.{DirectoryEntry, ETable2, Edge2, TetrisTable}
import net.verdagon.radonc.hammer.Hammer.translate
import net.verdagon.radonc.templar._

import scala.collection.immutable.ListMap

object StructHammer {
  def translateInterfaces(hinputs: Hinputs, hamuts0: Hamuts): Hamuts = {
    hinputs.program2.getAllInterfaces.foldLeft(hamuts0)({
      case (hamuts1, interfaceDef2) => {
        val (hamuts2, interfaceDef3) = translateInterfaceRef(hinputs, hamuts1, interfaceDef2.getRef)
        hamuts2
      }
    })
  }

  private def translateInterfaceRefs(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRefs2: List[InterfaceRef2]):
  (Hamuts, List[InterfaceRef3]) = {
    interfaceRefs2 match {
      case Nil => (hamuts0, Nil)
      case head2 :: tail2 => {
        val (hamuts1, head3) = translateInterfaceRef(hinputs, hamuts0, head2)
        val (hamuts2, tail3) = translateInterfaceRefs(hinputs, hamuts1, tail2)
        (hamuts2, head3 :: tail3)
      }
    }
  }

  def translateInterfaceRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      interfaceRef2: InterfaceRef2):
  (Hamuts, InterfaceRef3) = {
    hamuts0.interfaceRefs.get(interfaceRef2) match {
      case Some(structRef3) => (hamuts0, structRef3)
      case None => {
        // This is the only place besides InterfaceDefinition3 that can make a InterfaceRef3
        val temporaryInterfaceRef3 =
          InterfaceRef3(
            hinputs.interfaceIds(interfaceRef2),
            NameTemplar.getReferendIdentifierName(interfaceRef2));
        val hamuts1 = hamuts0.forwardDeclareInterface(interfaceRef2, temporaryInterfaceRef3)
        val interfaceDef2 = hinputs.program2.lookupInterface(interfaceRef2);


        val edgeBlueprint = hinputs.edgeBlueprintsByInterface(interfaceRef2);

        val prototypes2 =
          edgeBlueprint.superFamilyRootBanners.map(superFamilyRootBanner => {
            hinputs.program2.lookupFunction(superFamilyRootBanner.toSignature).get.header.toPrototype
          })

        val (hamuts2, prototypes3) = FunctionHammer.translatePrototypes(hinputs, hamuts1, prototypes2)

        val interfaceDef3 =
          InterfaceDefinition3(
            temporaryInterfaceRef3.interfaceId,
            interfaceDef2.humanName,
            NameTemplar.getReferendIdentifierName(interfaceRef2),
            interfaceDef2.mutability,
            List() /* super interfaces */,
            prototypes3)
        val hamuts3 = hamuts2.addInterface(interfaceRef2, interfaceDef3)
        assert(interfaceDef3.getRef == temporaryInterfaceRef3)
        (hamuts3, interfaceDef3.getRef)
      }
    }
  }

  def translateStructs(hinputs: Hinputs, hamuts0: Hamuts): Hamuts = {
    hinputs.program2.getAllStructs.foldLeft(hamuts0)({
      case (hamuts1, structDef2) => {
        val (hamuts2, structDef3) = translateStructRef(hinputs, hamuts1, structDef2.getRef)
        hamuts2
      }
    })
  }

  def translateStructRef(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef2: StructRef2):
  (Hamuts, StructRef3) = {
    hamuts0.structRefs.get(structRef2) match {
      case Some(structRef3) => (hamuts0, structRef3)
      case None => {
        val structId = hinputs.structIds(structRef2)
        // This is the only place besides StructDefinition3 that can make a StructRef3
        val temporaryStructRef3 =
          StructRef3(structId, NameTemplar.getReferendIdentifierName(structRef2));
        val hamuts1 = hamuts0.forwardDeclareStruct(structRef2, temporaryStructRef3)
        val structDef2 = hinputs.program2.lookupStruct(structRef2);
        val (hamuts2, members3) =
          TypeHammer.translateMembers(hinputs, hamuts1, structDef2.members)
        
        val (hamuts3, edges3) = translateEdgesForStruct(hinputs, hamuts2, temporaryStructRef3, structRef2)

        val (hamuts4, eTable3) =
          hinputs.eTables.get(structRef2) match {
            case None => {
              (hamuts3, makeEmptyETable(hinputs, temporaryStructRef3))
            }
            case Some(eTable2) => {
              translateETable(hinputs, hamuts3, eTable2, temporaryStructRef3)
            }
          };

        val structDef3 =
          StructDefinition3(
            structId,
            structDef2.humanName,
            NameTemplar.getReferendIdentifierName(structRef2),
            structDef2.mutability,
            None /* base */,
            eTable3,
            edges3,
            members3,
            List() /* methods */);
        val hamuts5 = hamuts4.addStruct(structRef2, structDef3)
        assert(structDef3.getRef() == temporaryStructRef3)
        (hamuts5, structDef3.getRef())
      }
    }
  }

  private def makeEmptyETable(
      hinputs: Hinputs,
      temporaryStructRef3: StructRef3):
  ETable3 = {
    ETable3(
      temporaryStructRef3,
      TetrisTable[InterfaceRef3, InterfaceRef3](
        interfaceRef3 => interfaceRef3.interfaceId,
        Array(None),
        Array()))
  }

  private def translateETable(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      eTable2: ETable2,
      temporaryStructRef3: StructRef3):
  (Hamuts, ETable3) = {

    val interfaceRefs2 =
      eTable2.table.combinedBuckets.toList.flatMap(
        _.toList.flatMap(entry => List(entry._1, entry._2)))

    val (hamuts1, interfaceRefs3) =
      StructHammer.translateInterfaceRefs(hinputs, hamuts0, interfaceRefs2)

    val interfaceRefs3ByInterfaceRef2 =
      interfaceRefs2.zip(interfaceRefs3).toMap

    val eTable3 =
      ETable3(
        temporaryStructRef3,
        TetrisTable[InterfaceRef3, InterfaceRef3](
          interfaceRef3 => interfaceRef3.interfaceId,
          eTable2.table.directory,
          eTable2.table.combinedBuckets.map({
            case None => None
            case Some((keyInterfaceRef2, valueInterfaceRef2)) => {
              assert(keyInterfaceRef2 == valueInterfaceRef2)
              val interfaceRef3 = interfaceRefs3ByInterfaceRef2(keyInterfaceRef2)
              Some((interfaceRef3, interfaceRef3))
            }
          })))

    (hamuts1, eTable3)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef3: StructRef3,
      structRef2: StructRef2):
  (Hamuts, List[Edge3]) = {
    val edges2 = hinputs.edges.filter(_.struct == structRef2)
    translateEdgesForStruct(hinputs, hamuts0, structRef3, edges2.toList)
  }

  private def translateEdgesForStruct(
      hinputs: Hinputs, hamuts0: Hamuts,
      structRef3: StructRef3,
      edges2: List[Edge2]):
  (Hamuts, List[Edge3]) = {
    edges2 match {
      case Nil => (hamuts0, Nil)
      case headEdge2 :: tailEdges2 => {
        val interfaceRef2 = headEdge2.interface
        val (hamuts1, interfaceRef3) = StructHammer.translateInterfaceRef(hinputs, hamuts0, interfaceRef2)
        val interfaceDef3 = hamuts0.interfaceDefs(interfaceRef2)
        val (hamuts2, headEdge3) = translateEdge(hinputs, hamuts1, structRef3, interfaceDef3, headEdge2)
        val (hamuts3, tailEdges3) = translateEdgesForStruct(hinputs, hamuts2, structRef3, tailEdges2)
        (hamuts3, headEdge3 :: tailEdges3)
      }
    }
  }


  private def translateEdge(hinputs: Hinputs, hamuts0: Hamuts, structRef3: StructRef3, interfaceDef3: InterfaceDefinition3, edge2: Edge2):
  (Hamuts, Edge3) = {
    val interfacePrototypes3 = interfaceDef3.prototypes;
    val (hamuts1, prototypes3) = FunctionHammer.translatePrototypes(hinputs, hamuts0, edge2.methods)
    val structPrototypesByInterfacePrototype = ListMap[Prototype3, Prototype3](interfacePrototypes3.zip(prototypes3) : _*)
    (hamuts1, Edge3(structRef3, interfaceDef3.getRef, structPrototypesByInterfacePrototype))
  }
}
