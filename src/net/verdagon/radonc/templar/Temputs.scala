package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.Mutability

import scala.collection.immutable._

// A type is a "virtual type" if it's:
// - an interface, or
// - a struct that implements any interfaces.

// A type is non virtual if it's:
// - a struct that implements no interfaces.
// - a built-in type, like string, int, etc.

// Everything in the same family has the same:
// - Non-virtual param types, in the same places.
// - The remaining params are all virtual, and have the same ownership and root interface.

// A function can be in multiple families.
// struct MyStruct implements IA, IB {}
// doThing(a: IA) { }
// doThing(b: IB) { }
// doThing(m: MyStruct) { } belongs to two families, doThing(:IA) and doThing(:IB)

// if we call doThing(MyStruct)... then i suppose we can grab it from any family we want, right?
// but to be thorough, we probably should search all families that could match.


//case class FunctionFamily(rootHeader: FunctionBanner2, banners: Set[FunctionBanner2]) {
//  def add(newbieBanner2: FunctionBanner2): FunctionFamily = {
//    val sizeBefore = banners.size;
//    val result = FunctionFamily(rootHeader, banners + newbieBanner2)
//    if (result.banners.size != sizeBefore + 1) {
//      throw new RuntimeException("Couldn't put " + newbieBanner2 + " into " + banners)
//    }
//    result
//  }
//  def getAllFamilies(): List[FunctionFamily] = List(this)
//}
//
//case class FunctionTown(virtualParamRoots: List[InterfaceRef2], familiesByOwnerships: Map[List[Ownership], FunctionFamily]) {
//  def addFunctionToFamily(newbieBanner2: FunctionBanner2, familyRootBanner: FunctionBanner2): FunctionTown = {
//    val newbieParamOwnerships = newbieBanner2.paramTypes.map(_.ownership)
//    val newFamiliesByOwnerships =
//      familiesByOwnerships.mapValues(family => {
//        if (family.rootHeader.paramTypes.map(_.ownership) == newbieParamOwnerships) {
//          family.add(newbieBanner2)
//        } else {
//          family
//        }
//      });
//    assert(newFamiliesByOwnerships != familiesByOwnerships);
//    FunctionTown(virtualParamRoots, newFamiliesByOwnerships)
//  }
//  def getAllFamilies(): List[FunctionFamily] = {
//    familiesByOwnerships.values.flatMap(_.getAllFamilies()).toList
//  }
//}
//
//case class FunctionBarony(
//    nonesAndMundaneConcreteTypes: List[Option[ConcreteValue2]],
//    townsByVirtualParamRoots: Map[List[InterfaceRef2], FunctionTown]) {
//  def addFunctionFamily(banner: FunctionBanner2): FunctionBarony = {
//    val virtualParamRoots: List[InterfaceRef2] =
//      banner.params.filter(_.virtuality.nonEmpty).map(_.tyype.innerType.asInstanceOf[InterfaceRef2]);
//    assert(!townsByVirtualParamRoots.contains(virtualParamRoots))
//    FunctionBarony(
//      nonesAndMundaneConcreteTypes,
//      townsByVirtualParamRoots + (virtualParamRoots -> FunctionTown(List(), Map())))
//  }
//  def addFunctionToFamily(banner: FunctionBanner2, familyRootBanner: FunctionBanner2): FunctionBarony = {
//    val newbieVirtualParams: List[CitizenRef2] =
//      banner.params.filter(_.virtuality.nonEmpty).map(_.tyype.innerType.asInstanceOf[CitizenRef2]);
//    assert(newbieVirtualParams.nonEmpty)
//
//    val newTownsByVirtualParamRoots =
//      townsByVirtualParamRoots.mapValues(town => {
//        val matches =
//          newbieVirtualParams.zip(town.virtualParamRoots).forall({
//            case (newbieVirtualParam, townieVirtualRoot) => {
//              program2.lookupCitizen(newbieVirtualParam).implementsInterface(program2, townieVirtualRoot)
//            }
//          });
//        if (matches) {
//          town.addFunctionToFamily(banner, familyRootBanner)
//        } else {
//          throw new RuntimeException("curiosity") // does this mean an error or is this fine
//          town
//        }
//      });
//    val result = FunctionBarony(nonesAndMundaneConcreteTypes, newTownsByVirtualParamRoots)
//    assert(this != result)
//    result
//  }
//  def getAllFamilies(): List[FunctionFamily] = {
//    townsByVirtualParamRoots.values.flatMap(_.getAllFamilies()).toList
//  }
//}
//
//case class FunctionKingdom(
//    humanName: String,
//    baroniesByNonesAndMundaneParamTypes: Map[List[Option[ConcreteValue2]], FunctionBarony]) {
//  private def getNonesAndMundaneConcreteTypes(banner: FunctionBanner2): List[Option[ConcreteValue2]] = {
//    banner.params.map({
//      case Parameter2(_, None, Reference2(_, concreteType)) => Some(concreteType)
//      case Parameter2 => None
//    })
//  }
//  def addFunctionToFamily(program2: Program2, newbieBanner2: FunctionBanner2, familyRootBanner: FunctionBanner2): FunctionKingdom = {
//    val nonesAndMundaneConcreteTypes = getNonesAndMundaneConcreteTypes(newbieBanner2)
//    val barony = baroniesByNonesAndMundaneParamTypes.getOrElse(nonesAndMundaneConcreteTypes, FunctionBarony(nonesAndMundaneConcreteTypes, Map()))
//    val result =
//      FunctionKingdom(
//        humanName,
//        baroniesByNonesAndMundaneParamTypes + (nonesAndMundaneConcreteTypes -> barony.addFunctionToFamily(program2, newbieBanner2, familyRootBanner)))
//    assert(this != result)
//    result
//  }
////  def declare(header: Signature2): FunctionKingdom = {
////    FunctionKingdom(humanName, baroniesByNonesAndMundaneParamTypes, declaredHeaders + header)
////  }
//  def addFunctionFamily(newRootBanner: FunctionBanner2): FunctionKingdom = {
//    val nonesAndMundaneConcreteTypes = getNonesAndMundaneConcreteTypes(newRootBanner)
//    val barony = baroniesByNonesAndMundaneParamTypes.getOrElse(nonesAndMundaneConcreteTypes, FunctionBarony(nonesAndMundaneConcreteTypes, Map()))
//    FunctionKingdom(
//      humanName,
//      baroniesByNonesAndMundaneParamTypes + (nonesAndMundaneConcreteTypes -> barony.addFunctionFamily(newRootBanner)))
//  }
//  def getAllFamilies(): List[FunctionFamily] = {
//    baroniesByNonesAndMundaneParamTypes.values.flatMap(_.getAllFamilies()).toList
//  }
//}
//
//case class FunctionWorld(kingdoms: Map[String, FunctionKingdom]) {
//  def addFunctionToFamily(program2: Program2, newbieBanner: FunctionBanner2, familyRootBanner: FunctionBanner2): FunctionWorld = {
//    val humanName = newbieBanner.humanName;
//    val kingdom = kingdoms.getOrElse(humanName, FunctionKingdom(humanName, Map()))
//    val result =
//      FunctionWorld(kingdoms + (humanName -> kingdom.addFunctionToFamily(program2, newbieBanner, familyRootBanner)))
//    assert(result != this)
//    result
//  }
//  def addFunctionFamily(program2: Program2, header: FunctionBanner2): FunctionWorld = {
//    val humanName = header.humanName
//    val kingdom = kingdoms.getOrElse(humanName, FunctionKingdom(humanName, Map()))
//    val newKingdom = kingdom.addFunctionFamily(header)
//    FunctionWorld(kingdoms + (humanName -> newKingdom))
//  }
//  def familyExists(program2: Program2, familyRootBanner: FunctionBanner2): Boolean = {
//
//  }
//  def getAllFamilies(): List[FunctionFamily] = {
//    kingdoms.values.flatMap(_.getAllFamilies()).toList
//  }
//}

// declared banners are ones that we're currently evaluating the body for, or we've already
// finished evaluating the body for.
// We won't always have a return type for a banner... it might have not specified its return
// type, so we're currently evaluating the entire body for it right now.
// If we ever find ourselves wanting the return type for a banner, we need to:
// - Check if it's in the returnTypesByBanner map. If so, good.
// - If not, then check if the banner is in declaredBanners. If so, then we're currently in
//   the process of evaluating the entire body. In this case, throw an error because we're
//   about to infinite loop. Hopefully this is a user error, they need to specify a return
//   type to avoid a cyclical definition.
// - If not in declared banners, then tell FunctionTemplar to start evaluating it.

case class Temputs(
    declaredSignatures: Set[Signature2],

    // Not all signatures/banners will have a return type here, it might not have been processed yet.
    returnTypesBySignature: Map[Signature2, Reference2],

    // Anyone who puts something into declaredSignatures should eventually
    // put the signature in here.
    functionFamiliesByRootBanner: Map[FunctionBanner2, FunctionFamily],

    // Not all signatures/banners or even return types will have a function here, it might not have
    // been processed yet.
    functions: List[Function2],

    // One must fill this in when putting things into declaredStructs/Interfaces.
    mutabilitiesByCitizenRef: Map[CitizenRef2, Mutability],

    // declaredStructs is the structs that we're currently in the process of defining
    // Things will appear here before they appear in structDefsByRef
    // This is to prevent infinite recursion / stack overflow when templaring recursive types
    // Not too sure about the type of declaredStructs, we might need something else
    declaredStructs: Set[StructRef2],
    structDefsByRef: ListMap[StructRef2, StructDefinition2],
    // declaredInterfaces is the interfaces that we're currently in the process of defining
    // Things will appear here before they appear in interfaceDefsByRef
    // This is to prevent infinite recursion / stack overflow when templaring recursive types
    // Not too sure about the type of declaredInterfaces, we might need something else
    declaredInterfaces: Set[InterfaceRef2],
    interfaceDefsByRef: ListMap[InterfaceRef2, InterfaceDefinition2],
    // Only PackTemplar can make a PackT2
    packTypes: Map[List[Reference2], StructRef2])
  extends Program2 {
//
//  override def getAllFunctionFamilies: Map[FunctionBanner2, FunctionFamily] = {
//    virtualSignaturesByFamilyRoot.map({
//      case (familyRoot, memberSignatures) => {
//        (familyRoot -> FunctionFamily())
//      }
//    })
//  }

  override def lookupFunction(signature2: Signature2): Option[Function2] = {
    functions.find(_.header.toSignature == signature2)
  }

  // This means we've at least started to evaluate this function's body.
  // We use this to cut short any infinite looping that might happen when,
  // for example, there's a recursive function call.
  def declareFunctionSignature(signature: Signature2): Temputs = {
    Temputs(
      declaredSignatures + signature,
      returnTypesBySignature,
      functionFamiliesByRootBanner,
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def declareFunctionReturnType(signature: Signature2, returnType2: Reference2): Temputs = {
    returnTypesBySignature.get(signature) match {
      case None =>
      case Some(existingReturnType2) => assert(existingReturnType2 == returnType2)
    }
    assert(declaredSignatures.contains(signature))

    Temputs(
      declaredSignatures,
      returnTypesBySignature + (signature -> returnType2),
      functionFamiliesByRootBanner,
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def addFunctionFamily(banner: FunctionBanner2): Temputs = {
    assert(exactDeclaredSignatureExists(banner.humanName, banner.templateArgs, banner.paramTypes), declaredSignatures) // curiosity
    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner + (banner -> FunctionFamily(banner, Map())),
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def addFunctionToFamily(familyRootBanner: FunctionBanner2, virtualParams: List[CitizenRef2], newbieSignature: Signature2): Temputs = {
    assert(functionFamiliesByRootBanner.contains(familyRootBanner));
    val previousFamily = functionFamiliesByRootBanner(familyRootBanner);
    val previousSignaturesByVirtualRoots = previousFamily.memberSignaturesByVirtualRoots;
    val newSignaturesByVirtualRoots = previousSignaturesByVirtualRoots + (virtualParams -> newbieSignature)
    val newFamily = FunctionFamily(previousFamily.rootBanner, newSignaturesByVirtualRoots)

    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner + (familyRootBanner -> newFamily),
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def addFunction(function: Function2): Temputs = {
    assert(declaredSignatures.contains(function.header.toSignature))

    assert(!functions.exists(_.header == function.header))

    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner,
      function :: functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def declareStruct(humanName: String, templateArgs: List[CoercedTemplateArg2], mutability: Mutability): Temputs = {
    val temporaryStructRef = StructRef2(humanName, templateArgs)
    assert(!declaredStructs.contains(temporaryStructRef))
    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner,
      functions,
      mutabilitiesByCitizenRef + (temporaryStructRef -> mutability),
      declaredStructs + temporaryStructRef,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def declareInterface(humanName: String, templateArgs: List[CoercedTemplateArg2], mutability: Mutability):
  Temputs = {
    val temporaryInterfaceRef = InterfaceRef2(humanName, templateArgs)
    val newTemputs =
      Temputs(
        declaredSignatures,
        returnTypesBySignature,
        functionFamiliesByRootBanner,
        functions,
        mutabilitiesByCitizenRef + (temporaryInterfaceRef -> mutability),
        declaredStructs,
        structDefsByRef,
        declaredInterfaces + temporaryInterfaceRef,
        interfaceDefsByRef,
        packTypes)
    newTemputs
  }

  def declarePack(members: List[Reference2], understructRef2: StructRef2):
  Temputs = {
    val newTemputs =
      Temputs(
        declaredSignatures,
        returnTypesBySignature,
        functionFamiliesByRootBanner,
        functions,
        mutabilitiesByCitizenRef,
        declaredStructs,
        structDefsByRef,
        declaredInterfaces,
        interfaceDefsByRef,
        packTypes + (members -> understructRef2))
    newTemputs
  }

  def add(structDef: StructDefinition2): Temputs = {
    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner,
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef + (structDef.getRef -> structDef),
      declaredInterfaces,
      interfaceDefsByRef,
      packTypes)
  }

  def add(interfaceDef: InterfaceDefinition2): Temputs = {
    Temputs(
      declaredSignatures,
      returnTypesBySignature,
      functionFamiliesByRootBanner,
      functions,
      mutabilitiesByCitizenRef,
      declaredStructs,
      structDefsByRef,
      declaredInterfaces,
      interfaceDefsByRef + (interfaceDef.getRef -> interfaceDef),
      packTypes)
  }

  def structDeclared(humanName: String, templateArgs: List[CoercedTemplateArg2]): Option[StructRef2] = {
    // This is the only place besides StructDefinition2 and declareStruct thats allowed to make one of these
    val structRef = StructRef2(humanName, templateArgs)
    if (declaredStructs.contains(structRef)) {
      Some(structRef)
    } else {
      None
    }
  }

  def lookupMutability(citizenRef2: CitizenRef2): Mutability = {
    // If it has a structRef, then it already exists
    mutabilitiesByCitizenRef(citizenRef2)
  }

  override def lookupStruct(structRef: StructRef2): StructDefinition2 = {
    // If it has a structRef, then it already exists
    structDefsByRef(structRef);
  }

  override def lookupCitizen(citizenRef: CitizenRef2): CitizenDefinition2 = {
    citizenRef match {
      case s@ StructRef2(_, _) => lookupStruct(s)
      case i : InterfaceRef2 => lookupInterface(i)
    }
  }

  def interfaceDeclared(humanName: String, templateArgs: List[CoercedTemplateArg2]): Option[InterfaceRef2] = {
    // This is the only place besides InterfaceDefinition2 and declareInterface thats allowed to make one of these
    val interfaceRef = InterfaceRef2(humanName, templateArgs)
    if (declaredInterfaces.contains(interfaceRef)) {
      Some(interfaceRef)
    } else {
      None
    }
  }

  override def lookupInterface(interfaceRef: InterfaceRef2): InterfaceDefinition2 = {
    // If it has a interfaceRef, then it already exists
    interfaceDefsByRef(interfaceRef);
  }

  def exactDeclaredSignatureExists(
      name: String,
      templateArgs: List[CoercedTemplateArg2],
      paramTypes: List[Reference2]):
  Boolean = {
    declaredSignatures.contains(Signature2(name, templateArgs, paramTypes))
  }

//  def findFunction(name: String, paramTypes: List[Reference2]): Option[FunctionHeader2] = {
//    val matchingFunctions = functions.find(this, name, paramTypes)
//    assert(matchingFunctions.size < 2)
//    matchingFunctions.headOption
//  }

  override def getAllStructs(): Set[StructDefinition2] = structDefsByRef.values.toSet

  override def getAllInterfaces(): Set[InterfaceDefinition2] = interfaceDefsByRef.values.toSet

  override def getAllFunctions(): Set[Function2] = functions.toSet
}

