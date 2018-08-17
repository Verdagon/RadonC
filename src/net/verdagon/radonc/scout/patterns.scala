package net.verdagon.radonc.scout

import net.verdagon.radonc.templar.Virtuality2

trait Pattern1

case class DiscardP1() extends Pattern1
case class CaptureP1(name: String, mutable: Boolean, innerPattern: Option[Pattern1]) extends Pattern1
case class TypeOfP1(tyype: Type1) extends Pattern1
//case class TemplateSpecifiedTypeOfP1(templateName: String, templateArgs: List[Type1]) extends Pattern1

case class TupleP1(elements: List[Pattern1]) extends Pattern1
case class PackP1(elements: List[Pattern1]) extends Pattern1
case class DestructureP1(name: String, members: List[Pattern1]) extends Pattern1
case class CallableP1(parameters: List[Pattern1], returnType: Type1) extends Pattern1
case class PackRestP1(pattern: Pattern1) extends Pattern1
case class TemplatePackRestP1(templateParamName: String) extends Pattern1
