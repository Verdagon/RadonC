package net.verdagon.radonc.scout

sealed trait Type1 { }

case class PackT1(elements: List[Type1]) extends Type1
case class Nullable1(inner: Type1) extends Type1

case class TypeName1(name: String) extends Type1 { }

case class TemplateCall1(templatedTypeName: String, templateArgs: List[Type1]) extends Type1
