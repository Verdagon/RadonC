package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc._
import net.verdagon.radonc._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.sculptor._

import scala.collection.immutable.List

object StructTemplar {
  val emptyPackStructRef = StructRef2("__Pack", List())
  def addBuiltInStructs(env: GlobalEnvironment, temputs0: Temputs): Temputs = {
    temputs0.add(StructDefinition2("__Pack", Immutable, List(), List(), List()))
  }

  private def makeConstructor(structDef: StructDefinition2): Function2 = {
    val constructorParams =
      structDef.members.map({
        case StructMember2(name, ReferenceMemberType2(reference)) => {
          Parameter2(name, None, reference)
        }
      })
    val constructorReturnOwnership = if (structDef.mutability == Mutable) Own else Share
    val constructorReturnType = Reference2(constructorReturnOwnership, structDef.getRef)
    // not virtual because how could a constructor be virtual
    Function2(
      FunctionHeader2(
        structDef.humanName,
        false, false,
        structDef.templateArgs,
        constructorParams,
        constructorReturnType,
        None),
      Block2(
        List(
          Construct2(
            structDef.getRef,
            Reference2(if (structDef.mutability == Mutable) Own else Share, structDef.getRef),
            constructorParams.map(p => {
              val lookup = LocalLookup2(p.name, p.tyype)
              p.tyype match {
                case Reference2(ownership, _) => SoftLoad2(lookup, ownership)
              }
            })))))
  }

  private def makeInterface(env: LocalEnvironment, temputs0: Temputs, interface1: Interface1, uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, InterfaceDefinition2) = {

    val coercedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(
        temputs0, interface1.templateParams, uncoercedTemplateArgs)

    val impls1 = env.globalEnv.impls.getOrElse(interface1.name, List())
    val (temputs2, implementedInterfaceRefs2) =
      getImplementedInterfaces(env.globalEnv, temputs0, coercedTemplateArgs, impls1);

    val temputs3 = temputs2.declareInterface(interface1.name, coercedTemplateArgs, interface1.mutability);

    val interfaceDef2 =
      InterfaceDefinition2(
        interface1.name, interface1.mutability, coercedTemplateArgs, implementedInterfaceRefs2)
    (temputs3, interfaceDef2)
  }

  // Gets the interface part of the "impl" statement.
  private def getImplementedInterface(
      outerEnv: GlobalEnvironment,
      temputs0: Temputs,
      templateArgs: List[CoercedTemplateArg2],
      impl1: Impl1):
  (Temputs, InterfaceRef2) = {
    if (impl1.templateParamNames.size != templateArgs.size) {
      throw new RuntimeException("Num impl template params != num struct template args!")
    }

    val additions = impl1.templateParamNames.zip(templateArgs).toMap
    val innerEnv = additions.foldLeft(outerEnv.spawnLocalEnv())({
      case (env, (param, coercedTyype)) => env.addType(param.name, coercedTyype.templata)
    })

    val (temputs1, superType2) =
      TypeTemplar.evaluateType(innerEnv, temputs0, impl1.interface)
    val interfaceRef2 =
        superType2 match {
          case ReferendTemplata(ir @ InterfaceRef2(_, _)) => ir
          case what @ _ => throw new RuntimeException("can only implement interfaces, found: " + what);
        };
    (temputs1, interfaceRef2)
  }

  private def getImplementedInterfaces(env: GlobalEnvironment, temputs0: Temputs, templateArgs: List[CoercedTemplateArg2], impls1: List[Impl1]):
  (Temputs, List[InterfaceRef2]) = {
    impls1 match {
      case Nil => (temputs0, List())
      case headImpl1 :: tailImpls1 => {
        val (temputs1, headInterfaceRef2) = getImplementedInterface(env, temputs0, templateArgs, headImpl1);
        val (temputs2, tailInterfaceRefs2) = getImplementedInterfaces(env, temputs1, templateArgs, tailImpls1);
        (temputs2, headInterfaceRef2 :: tailInterfaceRefs2)
      }
    }
  }

  private def makeStruct(
      env: LocalEnvironment,
      temputs0: Temputs,
      struct1: Struct1,
      coercedTemplateArgs: List[CoercedTemplateArg2]):
  (Temputs, StructDefinition2) = {
    val impls1 = env.globalEnv.impls.getOrElse(struct1.name, List())
    val (temputs2, implementedInterfaceRefs2) =
      getImplementedInterfaces(env.globalEnv, temputs0, coercedTemplateArgs, impls1);

    val (temputs3, members) = makeStructMembers(env, temputs2, struct1.members)

    val structDef2 =
      StructDefinition2(
        struct1.name, struct1.mutability, coercedTemplateArgs, implementedInterfaceRefs2, members)
    (temputs3, structDef2)
  }

//  private def conjureAbstractFunction1(prototype1: ParamPrototype1): Function1 = {
//    val body = Block1(List(Panic1()));
//    val patternParams = prototype1.signature.params.zipWithIndex.map({
//      case (param, paramIndex) => {
//        Parameter1(param.name, param.mutable, param.virtuality, paramIndex + 1, TypeOfP1(param.tyype))
//      }
//    })
//    Function1(
//      prototype1.name,
//      prototype1.signature.templateParamNames,
//      patternParams,
//      prototype1.signature.ret,
//      isExtern = false,
//      Set(),
//      body)
//  }

  private def makeStructMembers(env0: LocalEnvironment, temputs0: Temputs, members: List[StructMember1]): (Temputs, List[StructMember2]) = {
    members match {
      case Nil => (temputs0, Nil)
      case head1 :: tail1 => {
        val (temputs1, head2) = makeStructMember(env0, temputs0, head1);
        val (temputs2, tail2) = makeStructMembers(env0, temputs1, tail1);
        (temputs2, head2 :: tail2)
      }
    }
  }

  private def makeStructMember(env0: LocalEnvironment, temputs0: Temputs, member: StructMember1): (Temputs, StructMember2) = {
    val (temputs1, reference2) =
      TypeTemplar.evaluateAndReferencifyType(env0, temputs0, member.tyype, Own)
    (temputs1, StructMember2(member.name, ReferenceMemberType2(reference2)))
  }

  private def evaluateStructDefinition(env: GlobalEnvironment, temputs0: Temputs, struct1: Struct1): (GlobalEnvironment, Temputs) = {
    if (struct1.templateParams.isEmpty) {
      evaluatePlainStructDefinition(env, temputs0, struct1)
    } else {
      (env, temputs0)
    }
  }

  private def evaluateInterfaceDefinition(env0: GlobalEnvironment, temputs0: Temputs, interface1: Interface1):
  Temputs = {
    if (interface1.templateParams.isEmpty) {
      evaluatePlainInterfaceDefinition(env0, temputs0, interface1)
    } else {
      temputs0
    }
  }

  private def scanTemplateInterfaceDefinition(env0: GlobalEnvironment, interface1: Interface1): GlobalEnvironment = {
    val env1 = env0.addType(interface1.name, InterfaceTemplateTemplata(interface1))

    interface1.members.foldLeft(env1)({
      case (env2, prototype1) => {
        env2.addFunctionTemplate(prototype1.origin)
      }
    })
  }

  private def scanTemplateStructDefinition(env0: GlobalEnvironment, struct1: Struct1): GlobalEnvironment = {
    val env1 = env0.addType(struct1.name, StructTemplateTemplata(struct1))

    val params = struct1.members.zipWithIndex.map({
      case (member, index) => {
        Parameter1(None, index + 1, CaptureP1(member.name, false, Some(TypeOfP1(member.tyype))))
      }
    })
    val lookups = struct1.members.map(member => Lookup1(member.name))
    val constructorTemplate =
      Function1(
        struct1.name,
        false,
        false,
        Set(),
        struct1.templateParams,
        params,
        Some(TemplateCall1(struct1.name, struct1.templateParams.map(_.name).map(TypeName1))),
        Some(
          Block1(
            List(
              Construct1(
                TemplateCall1(struct1.name, struct1.templateParams.map(_.name).map(TypeName1)),
                lookups)))));

    val env2 = env1.addFunctionTemplate(constructorTemplate);
    env2
  }

  private def scanInterfaceDefinition(env0: GlobalEnvironment, temputs0: Temputs, interface1: Interface1):
  (GlobalEnvironment, Temputs) = {
    if (interface1.templateParams.isEmpty) {
      val temporaryStructRef = InterfaceRef2(interface1.name, List());
      val env1 = env0.addType(interface1.name, ReferendTemplata(temporaryStructRef))
      val temputs1 = temputs0.declareInterface(interface1.name, List(), interface1.mutability)
      (env1, temputs1)
    } else {
      val env1 = scanTemplateInterfaceDefinition(env0, interface1)
      (env1, temputs0)
    }
  }

  private def scanStructDefinition(env0: GlobalEnvironment, temputs0: Temputs, struct1: Struct1): (GlobalEnvironment, Temputs) = {
    if (struct1.templateParams.isEmpty) {
      val temporaryStructRef = StructRef2(struct1.name, List());
      val env1 = env0.addType(struct1.name, ReferendTemplata(temporaryStructRef))
      val temputs1 = temputs0.declareStruct(struct1.name, List(), struct1.mutability)
      (env1, temputs1)
    } else {
      val env1 = scanTemplateStructDefinition(env0, struct1)
      (env1, temputs0)
    }
  }

  private def evaluatePlainStructDefinition(env0: GlobalEnvironment, temputs0: Temputs, struct1: Struct1): (GlobalEnvironment, Temputs) = {
    val temporaryStructRef = StructRef2(struct1.name, List());
    val (temputs1, struct2) = makeStruct(env0.spawnLocalEnv(), temputs0, struct1, List());
    assert(struct2.getRef == temporaryStructRef);
    val temputs2 = temputs1.add(struct2)

    val constructor2 = makeConstructor(struct2)
    val temputs3 =
      temputs2
        .declareFunctionSignature(constructor2.header.toSignature)
        .declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
        .addFunction(constructor2);
    val env1 = env0.addFunctionBanner(constructor2.header.humanName, constructor2.header.toBanner)

    assert(temputs3.exactDeclaredSignatureExists(constructor2.header.humanName, constructor2.header.templateArgs, constructor2.header.toBanner.paramTypes))

    (env1, temputs3)
  }

  // Returns the temputs, the struct definition, and the constructor
  private def stampStructTemplate(outerEnv: GlobalEnvironment, temputs0: Temputs, struct1: Struct1, uncoercedTemplateArgs: List[ITemplata]):
  (Temputs, StructRef2, FunctionBanner2) = {
    val coercedTemplateArgs =
      TypeTemplar.coerceTemplateArgs(temputs0, struct1.templateParams, uncoercedTemplateArgs)

    val paramTemplataTypesByName = struct1.templateParams.zip(coercedTemplateArgs).toMap;
    val innerEnv = paramTemplataTypesByName.foldLeft(outerEnv.spawnLocalEnv())({
      case (env, (templateParam1, coercedTypeTemplata)) => {
        env.addType(templateParam1.name, coercedTypeTemplata.templata)
      }
    })

    val temputs1 = temputs0.declareStruct(struct1.name, coercedTemplateArgs, struct1.mutability);

    val (temputs2, structDefinition2) =
      makeStruct(innerEnv, temputs1, struct1, coercedTemplateArgs);

    val temputs3 = temputs2.add(structDefinition2);

    val constructor2 = makeConstructor(structDefinition2);

    val temputs4 =
      temputs3
        .declareFunctionSignature(constructor2.header.toSignature)
        .declareFunctionReturnType(constructor2.header.toSignature, constructor2.header.returnType)
        .addFunction(constructor2);

    val temputs5 = VirtualTemplar.findAndStampIntoFamilies(outerEnv.spawnLocalEnv(), temputs4, structDefinition2.getRef)

    (temputs5, structDefinition2.getRef, constructor2.header.toBanner)
  }

  private def evaluatePlainInterfaceDefinition(env0: GlobalEnvironment, temputs0: Temputs, interface1: Interface1): Temputs = {
    val temporaryStructRef = InterfaceRef2(interface1.name, List());
    val (temputs1, interface2) = makeInterface(env0.spawnLocalEnv(), temputs0, interface1, List());
    assert(interface2.getRef == temporaryStructRef);
    temputs1.add(interface2)
  }

  // Returns the temputs, the interface definition
  private def stampInterfaceTemplate(outerEnv: GlobalEnvironment, temputs0: Temputs, interface1: Interface1, templateArgs: List[ITemplata]):
  (Temputs, InterfaceRef2) = {
    val paramTemplataTypesByName = interface1.templateParams.zip(templateArgs).toMap;
    val innerEnv = paramTemplataTypesByName.foldLeft(outerEnv.spawnLocalEnv())({
      case (env, (templateParam1, typeTemplata)) => env.addType(templateParam1.name, typeTemplata)
    })

    val (temputs1, interfaceDefinition2) = makeInterface(innerEnv, temputs0, interface1, templateArgs);

    val temputs2 = temputs1.add(interfaceDefinition2);

    (temputs2, interfaceDefinition2.getRef)
  }

  def scanTypeDefinitions(env0: GlobalEnvironment, temputs0: Temputs, structs: List[TypeDefinition1]):
  (GlobalEnvironment, Temputs) = {
    structs.foldLeft((env0, temputs0))({
      case ((env1, temputs1), struct1 : Struct1) => {
        scanStructDefinition(env1, temputs1, struct1)
      }
      case ((env1, temputs1), interface1 : Interface1) => {
        scanInterfaceDefinition(env1, temputs1, interface1)
      }
    })
  }

  // This modifies GlobalEnvironment; it adds structs' constructors to it.
  // todo: maybe split that out into a separate phase?
  def evaluateTypeDefinitions(env0: GlobalEnvironment, temputs0: Temputs, structs: List[TypeDefinition1]):
  (GlobalEnvironment, Temputs) = {
    structs.foldLeft((env0, temputs0))({
      case ((env1, temputs1), struct1 : Struct1) => {
        evaluateStructDefinition(env1, temputs1, struct1)
      }
      case ((env1, temputs1), interface1 : Interface1) => {
        (env1, evaluateInterfaceDefinition(env1, temputs1, interface1))
      }
    })
  }
//
//  def getConstructorRef(env: GlobalEnvironment, temputs0: Temputs, struct1: Struct1, combinedTemplateArgs2: List[Reference2]):
//  (Temputs, FunctionHeader2) = {
//    // This stamps it if it wasn't stamped already
//    val (temputs1, structRef2) = getStructRef(env, temputs0, struct1.name, combinedTemplateArgs2)
//    // From here on out, we're just trying to recover the stamped stuff
//    val structDef2 = temputs1.lookupStruct(structRef2);
//    val constructorParamTypes = structDef2.members.map(m => TypeUtils.softDecay(m.tyype))
//    val constructorHeader2 = temputs1.exactDeclaredSignatureExists(structDef2.humanName, constructorParamTypes);
//    (temputs1, constructorHeader2)
//  }

  // Assumes it already exists:
  // - if it's templated, its StructDef1 and constructor are already in the env,
  // - if it's not templated, its StructDef2 and constructor are already in the env
  def getStructRef(env: GlobalEnvironment, temputs0: Temputs, structName: String, templateArgs2: List[ITemplata]):
  (Temputs, StructRef2) = {

    val coercedTemplatas =
      env.lookupType(structName) match {
        case StructTemplateTemplata(struct1) => {
          TypeTemplar.coerceTemplateArgs(temputs0, struct1.templateParams, templateArgs2)
        }
      }

    temputs0.structDeclared(structName, coercedTemplatas) match {
      case Some(structRef2) => {
        (temputs0, structRef2)
      }
      case None => {
        env.lookupType(structName) match {
          case ReferendTemplata(referend) => {
            referend match {
              case sr @ StructRef2(_, _) => {
                // If the env contains a StructRef2, then that means a StructRef2 was already created
                // and those can only come from StructDef2, which means a StructDef2 exists, which
                // means it's in the temputs.
                temputs0.lookupStruct(sr); // Make sure it is in the temputs
                // anyway, just return the structref
                (temputs0, sr)
              }
            }
          }
          case StructTemplateTemplata(struct1) => {
            // It doesnt exist yet (we would have seen it in structDeclared), stamp it
            val (temputs1, structRef, constructorRef) =
              stampStructTemplate(env, temputs0, struct1, templateArgs2)
            (temputs1, structRef)
          }
        }
      }
    }
  }


  // Assumes it already exists:
  // - if it's templated, its StructDef1 and constructor are already in the env,
  // - if it's not templated, its StructDef2 and constructor are already in the env
  def getInterfaceRef(env: GlobalEnvironment, temputs0: Temputs, interfaceName: String, templateArgs2: List[ITemplata]):
  (Temputs, InterfaceRef2) = {

    val coercedTemplatas =
      env.lookupType(interfaceName) match {
        case StructTemplateTemplata(struct1) => {
          TypeTemplar.coerceTemplateArgs(temputs0, struct1.templateParams, templateArgs2)
        }
        case InterfaceTemplateTemplata(interface1) => {
          TypeTemplar.coerceTemplateArgs(temputs0, interface1.templateParams, templateArgs2)
        }
      }

    temputs0.interfaceDeclared(interfaceName, coercedTemplatas) match {
      case Some(interfaceRef2) => {
        (temputs0, interfaceRef2)
      }
      case None => {
        env.lookupType(interfaceName) match {
          case ReferendTemplata(interfaceRef @ InterfaceRef2(_, _)) => {
            // If the env contains a InterfaceRef2, then that means a InterfaceRef2 was already created
            // and those can only come from InterfaceDef2, which means a InterfaceDef2 exists, which
            // means it's in the temputs.
            temputs0.lookupInterface(interfaceRef); // Make sure it is in the temputs
            // anyway, just return the interfaceref
            (temputs0, interfaceRef)
          }
          case InterfaceTemplateTemplata(interface1) => {
            // It doesnt exist yet (we would have seen it in interfaceDeclared), stamp it
            val (temputs1, interfaceRef) =
              stampInterfaceTemplate(env, temputs0, interface1, templateArgs2)
            (temputs1, interfaceRef)
          }
        }
      }
    }
  }

  private def isStructConvertible(
      program2: Program2,
      sourceStructRef: StructRef2,
      targetInterfaceRef: InterfaceRef2):
  Boolean = {
    val ancestorInterfaces =
      program2.lookupStruct(sourceStructRef).getAncestorInterfacesNotIncludingSelf(program2);

    ancestorInterfaces.exists(ancestorInterface => {
      isInterfaceConvertible(program2, ancestorInterface, targetInterfaceRef)
    })
  }


  // for example, a ListNode:Civic is convertible to ListNode:Honda.
  private def isInterfaceConvertible(
      program2: Program2,
      sourceInterfaceRef: InterfaceRef2,
      targetInterfaceRef: InterfaceRef2):
  Boolean = {
    if (sourceInterfaceRef.humanName == targetInterfaceRef.humanName) {
      val sourceAndTargetInterfaceRefs =
        sourceInterfaceRef.templateArgs.zip(targetInterfaceRef.templateArgs);
      sourceAndTargetInterfaceRefs.forall({
        case (CoercedTemplateArg2(ReferenceTemplata(s)), CoercedTemplateArg2(ReferenceTemplata(i))) =>
          TypeTemplar.isTypeConvertible(program2, s, i)
      })
    } else {
      false
    }
  }

  def convert(
      env: GlobalEnvironment,
      temputs: Temputs,
      sourceExpr: ReferenceExpression2,
      sourceStructRef: StructRef2,
      targetInterfaceRef: InterfaceRef2):
  ReferenceExpression2 = {
    if (isStructConvertible(temputs, sourceStructRef, targetInterfaceRef)) {
      StructToInterfaceUpcast2(sourceExpr, targetInterfaceRef)
    } else {
      val sourceStructDef = temputs.lookupStruct(sourceStructRef)
      println("source struct def interfaces: " + sourceStructDef.superInterfaces + " target interface: " + targetInterfaceRef)
      throw new RuntimeException("Can't upcast a " + sourceStructRef + " to a " + targetInterfaceRef)
    }
  }

  def isCitizenConvertible(program2: Program2, sourceCitizenRef: CitizenRef2, targetCitizenRef: CitizenRef2): Boolean = {
    val sourceCitizenDef = program2.lookupCitizen(sourceCitizenRef)
    sourceCitizenDef.getAncestorCitizens(program2, includeSelf = false).contains(targetCitizenRef)
  }

  def getCompoundTypeMutability(temputs0: Temputs, memberTypes2: List[Reference2])
  : Mutability = {
    val memberReferendMutabilities = Templar.getMutabilities(temputs0, memberTypes2.map(_.referend))
    val membersOwnerships = memberTypes2.map(_.ownership)
    val memberOwnershipsAndReferendMutabilities = membersOwnerships.zip(memberReferendMutabilities)
    val anyMembersOwnMutables =
      memberOwnershipsAndReferendMutabilities.exists({
        case (Own, Mutable) => true
        case (_, _) => false
      })
    if (anyMembersOwnMutables) Mutable else Immutable
  }

  // Makes a struct to back a closure
  def makeClosureUnderstruct(
      env: LocalEnvironment,
      temputs0: Temputs,
      functionName: String,
      memberNamesAndTypes2: List[(String, Reference2)]):
  (Temputs, StructRef2, Mutability) = {
    val closuredVarsStructMembers =
      memberNamesAndTypes2.map({
        case (name, reference) => StructMember2(name, AddressMemberType2(reference))
      });

    val closureStructFullName = "__Closure:" + functionName

    val mutability =
      StructTemplar.getCompoundTypeMutability(temputs0, memberNamesAndTypes2.map(_._2))

    val temputs2 =
      temputs0.declareStruct(
        closureStructFullName, List(), mutability);

    val closureStructDefinition =
      StructDefinition2(
        closureStructFullName, mutability, List(), List(), closuredVarsStructMembers);
    val temputs3 = temputs2.add(closureStructDefinition)

    val closuredVarsStructRef = closureStructDefinition.getRef;

    (temputs3, closuredVarsStructRef, mutability)
  }

  // Makes a struct to back a pack or tuple
  def makeUnderstruct(temputs0: Temputs, memberTypes2: List[Reference2], prefix: String): (Temputs, StructRef2, Mutability) = {
    temputs0.packTypes.get(memberTypes2) match {
      case Some(structRef2) => (temputs0, structRef2, temputs0.lookupStruct(structRef2).mutability)
      case None => {
        val packMutability = getCompoundTypeMutability(temputs0, memberTypes2)
        val members = memberTypes2.zipWithIndex.map({
          case (pointerType, index) => StructMember2(index.toString, ReferenceMemberType2(pointerType))
        })
        val coercedTemplateArgs =
          TypeTemplar.coerceTemplateArgs(
            temputs0,
            memberTypes2.zipWithIndex.map({
              case (_, index) => TemplateParameter1(prefix + "TemplateParam" + index, ReferenceTemplataType1)
            }),
            memberTypes2.map(ReferenceTemplata));

        val newStructDef =
          StructDefinition2(
            prefix, packMutability, coercedTemplateArgs, List(), members);
        if (memberTypes2.isEmpty && packMutability != Immutable)
          println("wat")

        val temputs2 = temputs0.declareStruct(prefix, coercedTemplateArgs, packMutability);
        val temputs3 = temputs2.add(newStructDef)
        val temputs4 = temputs3.declarePack(memberTypes2, newStructDef.getRef());
        (temputs4, newStructDef.getRef(), packMutability)
      }
    }
  }
}
