package net.verdagon.radonc.scout

import net.verdagon.radonc.templar.Virtuality2

case class Program0(
    typeDefinitions: List[TypeDefinition0],
    impls: List[Impl1],
    functions: List[Function0])

trait Expression0

sealed trait TopLevelThing0
case class TopLevelFunction(function: Function0) extends TopLevelThing0
case class TopLevelStruct(struct: Struct0) extends TopLevelThing0
case class TopLevelInterface(function: Interface0) extends TopLevelThing0
case class TopLevelImplements(impl: Impl1) extends TopLevelThing0

trait TypeDefinition0 {
  def name: String;
}

sealed trait Mutability {}
case object Mutable extends Mutability
case object Immutable extends Mutability

case class Struct0(
    name: String,
    mutability: Mutability,
    templateParams: List[TemplateParameter1],
    members: List[StructMember1]) extends TypeDefinition0

case class Interface0(
    name: String,
    mutability: Mutability,
    templateParams: List[TemplateParameter1],
    members: List[Function0]) extends TypeDefinition0

case class Lambda0(function: Function0) extends Expression0

case class Function0(
    name: Option[String],
    isExtern: Boolean,
    isAbstract: Boolean,
    templateParams: List[TemplateParameter1],
    params: List[Parameter0],
    ret: Option[Type1],
    body: Option[Block0]) extends Expression1

case class Parameter0(
    virtuality: Option[Virtuality2],
    pattern: Pattern1)



trait Expression1

case class Program1(
    typeDefinitions: List[TypeDefinition1],
    impls: List[Impl1],
    implementedFunctions: List[Function1])

trait TypeDefinition1 {
  def name: String;
}

case class StructMember1(name: String, tyype: Type1)
case class Struct1(
    struct1Id: Int,
    name: String,
    mutability: Mutability,
    templateParams: List[TemplateParameter1],
    members: List[StructMember1]) extends TypeDefinition1

case class Impl1(structName: String, templateParamNames: List[TemplateParameter1], interface: Type1)

case class Interface1(
    interface1Id: Int,
    name: String,
    mutability: Mutability,
    templateParams: List[TemplateParameter1],
    members: List[NTVFunction1]) extends TypeDefinition1

case class Parameter1(
    virtuality: Option[Virtuality2],
    patternId: Int,
    capturePattern: CaptureP1)

case class SimpleParameter1(
    origin: Option[Parameter1],
    name: String,
    mutable: Boolean,
    virtuality: Option[Virtuality2],
    tyype: Type1)

// Underlying class for all Function1 types
case class Function1(
    name: String,
    isExtern: Boolean,
    isAbstract: Boolean,
    closuredNames: Set[String],
    templateParams: List[TemplateParameter1],
    params: List[Parameter1],
    ret: Option[Type1],
    body: Option[Block1]) extends Expression1

case class BFunction1(
  origin: Function1,
  name: String,
  body: Block1)

// Named, Templatable, Virtual
// (used in, for example, interfaces)
case class NTVFunction1(
  origin: Function1,
  name: String,
  templateParams: List[TemplateParameter1],
  params: List[SimpleParameter1]) {

  def unapply(function: NTVFunction1): Option[(String, List[TemplateParameter1], List[SimpleParameter1])] =
    Some(name, templateParams, params)
}

sealed trait ITemplataType1 {}
case object ReferenceTemplataType1 extends ITemplataType1
case object ReferendTemplataType1 extends ITemplataType1
case object TypeTemplateTemplataType1 extends ITemplataType1
case object IntTemplataType1 extends ITemplataType1
case object StringTemplataType1 extends ITemplataType1
case object BoolTemplataType1 extends ITemplataType1

case class TemplateParameter1(name: String, tyype: ITemplataType1)
