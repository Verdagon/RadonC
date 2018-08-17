package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.Function1

import scala.collection.immutable.{Map, Set}

case class CovariantFamily(
    root: Prototype2,
    covariantParamIndices: List[Int],
    overrides: List[Prototype2])

trait Queriable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T];

  def allOf[T](classs: Class[T]): List[T] = {
    all({
      case x if classs.isInstance(x) => classs.cast(x)
    })
  }

  def only[T](func: PartialFunction[Queriable2, T]): T = {
    val list = all(func)
    if (list.size > 1) {
      throw new RuntimeException("More than one!");
    } else if (list.isEmpty) {
      throw new RuntimeException("Not found!");
    }
    list.head
  }

  def onlyOf[T](classs: Class[T]): T = {
    val list =
      all({
        case x if classs.isInstance(x) => classs.cast(x)
      })
    if (list.size > 1) {
      throw new RuntimeException("More than one!");
    } else if (list.isEmpty) {
      throw new RuntimeException("Not found!");
    }
    list.head
  }
}

case class FunctionFamily(
    rootBanner: FunctionBanner2,
    memberSignaturesByVirtualRoots: Map[List[CitizenRef2], Signature2])

trait Program2 {
  def getAllInterfaces: Set[InterfaceDefinition2]
  def getAllStructs: Set[StructDefinition2]
  def getAllFunctions: Set[Function2]
  def getAllCitizens: Set[CitizenDefinition2] = getAllInterfaces ++ getAllStructs
  def functionFamiliesByRootBanner: Map[FunctionBanner2, FunctionFamily];

  def lookupStruct(structRef: StructRef2): StructDefinition2;
  def lookupInterface(interfaceRef: InterfaceRef2): InterfaceDefinition2;
  def lookupCitizen(citizenRef: CitizenRef2): CitizenDefinition2;
  def lookupFunction(signature2: Signature2): Option[Function2];

  def getAllNonExternFunctions: Set[Function2] = {
    getAllFunctions.filter(!_.header.isExtern)
  }
}

case class CompleteProgram2(
    interfaces: List[InterfaceDefinition2],
    structs: List[StructDefinition2],
    functions: Set[Function2],
    functionFamiliesByRootBanner: Map[FunctionBanner2, FunctionFamily]) extends Queriable2 with Program2 {
  override def getAllInterfaces = interfaces.toSet
  override def getAllStructs = structs.toSet
  override def getAllFunctions = functions

  override def lookupStruct(structRef: StructRef2): StructDefinition2 = {
    structs.find(_.getRef == structRef).get
  }

  override def lookupInterface(interfaceRef: InterfaceRef2): InterfaceDefinition2 = {
    interfaces.find(_.getRef == interfaceRef).get
  }

  override def lookupCitizen(citizenRef2: CitizenRef2): CitizenDefinition2 = {
    citizenRef2 match {
      case s@ StructRef2(_, _) => lookupStruct(s)
      case i : InterfaceRef2 => lookupInterface(i)
    }
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    interfaces.flatMap(_.all(func)) ++
        structs.flatMap(_.all(func)) ++
        functions.toList.flatMap(_.all(func))
  }

  def lookupFunction(humanName: String): Function2 = {
    getAllFunctions.find(_.header.humanName == humanName).get
  }

  override def lookupFunction(signature2: Signature2): Option[Function2] = {
    functions.find(_.header.toSignature == signature2).headOption
  }
}

trait Expression2 extends Queriable2 {
  def resultRegister: IRegister2
  def referend: Referend2
}
trait ReferenceExpression2 extends Expression2 {
  override def resultRegister: ReferenceRegister2
  override def referend = resultRegister.reference.referend
}
trait AddressExpression2 extends Expression2 {
  override def resultRegister: AddressRegister2
  override def referend = resultRegister.reference.referend
}

trait IRegister2 extends Queriable2 {
  def expectReference(): ReferenceRegister2 = {
    this match {
      case r @ ReferenceRegister2(_) => r
      case a @ AddressRegister2(_) => throw new RuntimeException("Expected a reference as a result, but got an address!")
    }
  }
  def expectAddress(): AddressRegister2 = {
    this match {
      case a @ AddressRegister2(_) => a
      case r @ ReferenceRegister2(_) => throw new RuntimeException("Expected an address as a result, but got a reference!")
    }
  }
  def referend: Referend2
}
case class AddressRegister2(reference: Reference2) extends IRegister2 {
  override def referend = reference.referend
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
case class ReferenceRegister2(reference: Reference2) extends IRegister2 {
  override def referend = reference.referend
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}

//trait PointerExpression2 extends Expression2 {
//  override def resultType: Reference2
//}

trait Virtuality2 extends Queriable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T];
}
case class Virtual2() extends Virtuality2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class Override2() extends Virtuality2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

case class Parameter2(name: String, virtuality: Option[Virtuality2], tyype: Reference2) extends Queriable2 {
  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ virtuality.toList.flatMap(_.all(func)) ++ tyype.all(func)
  }
}

trait IPotentialBanner2{
  def humanName: String;
  def params: List[Parameter2];
  def paramTypes: List[Reference2] = params.map(_.tyype)
}

case class PotentialBannerFromTemplate(
    humanName: String,
    params: List[Parameter2],
    terry: TemplataFunctionTerry) extends IPotentialBanner2

case class PotentialBannerFromExisting(banner: FunctionBanner2) extends IPotentialBanner2 {
  override def humanName: String = banner.humanName
  override def params: List[Parameter2] = banner.params
}

// A "signature" is just the things required for overload resolution, IOW function name and arg types.

// An autograph could be a super signature; a signature plus attributes like virtual and mutable.
// If we ever need it, a "schema" could be something.

// A FunctionBanner2 is everything in a FunctionHeader2 minus the return type.
// These are only made by the FunctionTemplar, to signal that it's currently being
// evaluated or it's already been evaluated.
// It's easy to see all possible function banners, but not easy to see all possible
// function headers, because functions don't have to specify their return types and
// it takes a complete templar evaluate to deduce a function's return type.

case class Signature2(
    humanName: String,
    templateArgs: List[CoercedTemplateArg2],
    paramTypes: List[Reference2]) {
}

case class FunctionBanner2(
    originFunction: Option[Function1],
    humanName: String,
    templateArgs: List[CoercedTemplateArg2],
    params: List[Parameter2]) extends Queriable2  {
  def toSignature: Signature2 = Signature2(humanName, templateArgs, paramTypes)
  def paramTypes: List[Reference2] = params.map(_.tyype)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ params.flatMap(_.all(func))
  }

  def unapply(arg: FunctionBanner2):
  Option[(String, List[CoercedTemplateArg2], List[Parameter2])] =
    Some(humanName, templateArgs, params)
}

case class FunctionHeader2(
    humanName: String,
    isAbstract: Boolean, // For optimization later
    isExtern: Boolean, // For optimization later
    templateArgs: List[CoercedTemplateArg2],
    params: List[Parameter2],
    returnType: Reference2,
    originFunction: Option[Function1]) extends Queriable2 {

  // Make sure there's no duplicate names
  assert(params.map(_.name).toSet.size == params.size);

  def toBanner: FunctionBanner2 = FunctionBanner2(originFunction, humanName, templateArgs, params)
  def toPrototype: Prototype2 = Prototype2(humanName, templateArgs, FunctionT2(params.map(_.tyype), returnType))
  def toSignature: Signature2 = toPrototype.toSignature
  def functionType = toPrototype.functionType

  def paramTypes: List[Reference2] = params.map(_.tyype)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ params.flatMap(_.all(func)) ++ returnType.all(func)
  }

  def unapply(arg: FunctionHeader2): Option[(String, List[CoercedTemplateArg2], List[Parameter2], Reference2)] =
    Some(humanName, templateArgs, params, returnType)
}

case class Prototype2(
    humanName: String,
    templateArgs: List[CoercedTemplateArg2],
    functionType: FunctionT2) extends Queriable2 {
  def toSignature: Signature2 = Signature2(humanName, templateArgs, functionType.paramTypes)

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ functionType.all(func)
  }
}

case class Function2(
    header: FunctionHeader2,
    body: ReferenceExpression2) extends Queriable2 {
  def getFunctionType: FunctionT2 = {
    FunctionT2(header.params.map(_.tyype), header.returnType)
  }

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ header.all(func) ++ body.all(func)
  }
}
