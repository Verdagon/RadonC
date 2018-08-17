package net.verdagon.radonc.templar

import net.verdagon.radonc.scout._

import scala.collection.immutable._;

sealed trait ITemplataType2 {}
case object ReferenceTemplataType2 extends ITemplataType2
case object ReferendTemplataType2 extends ITemplataType2
case object TypeTemplateTemplataType2 extends ITemplataType2
case object IntTemplataType2 extends ITemplataType2
case object StringTemplataType2 extends ITemplataType2
case object BoolTemplataType2 extends ITemplataType2

sealed trait ITemplata extends Queriable2 {
  def order: Int;
}
case class ReferenceTemplata(reference: Reference2) extends ITemplata {
  override def order: Int = 1;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ reference.all(func)
  }
}
case class ReferendTemplata(referend: Referend2) extends ITemplata {
  override def order: Int = 2;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func) ++ referend.all(func)
  }
}
case class StructTemplateTemplata(struct: Struct1) extends ITemplata {
  override def order: Int = 3;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class ArrayTemplateTemplata(mutable: Boolean) extends ITemplata {
  override def order: Int = 8;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class InterfaceTemplateTemplata(interface: Interface1) extends ITemplata {
  override def order: Int = 4;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

trait IValueTemplata extends ITemplata
case class BooleanTemplata(value: Boolean) extends IValueTemplata {
  override def order: Int = 5;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class IntegerTemplata(value: Integer) extends IValueTemplata {
  override def order: Int = 6;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}
case class NoneTemplata() extends IValueTemplata {
  override def order: Int = 7;

  def all[T](func: PartialFunction[Queriable2, T]): List[T] = {
    List(this).collect(func)
  }
}

//trait IEvaluatedTemplata
//case class TemplataValue(value: IValueTemplata) extends IEvaluatedTemplata
//case class TemplataReferenceExpression(expr: ReferenceExpression2) extends IEvaluatedTemplata
//case class TemplataAddressExpression(expr: AddressExpression2) extends IEvaluatedTemplata
//// This is a value, the result of the code saying 'add' when there are like 6 add functions
//case class TemplataGlobalFunctionGroup(
//    name: String,
//    alreadySpecifiedTemplateArgs: List[ITemplata]) extends IEvaluatedTemplata
//case class TemplataLightTemplateLambdaFunction(
//    understruct: StructRef2,
//    terry: TemplataFunctionTerry) extends IEvaluatedTemplata

// Template Curry = Terry
// A terry is a reference to a template that might or might not have some args already specified.
case class TemplataFunctionTerry(
    outerEnv: Option[LocalEnvironment],
    function1: Function1,
    alreadySpecifiedExplicitTemplateArgs: List[ITemplata])


// dont have another temputs for templates, just pass them around like regular in the
// environment and via templar.evaluate's return values

// typeMembers is a Value2 because...
// fn callTheThing(callable: T) {
//   callable();
// }
// that can be either owned or borrowed.
// so, T will have to be either a MyStruct or a &MyStruct
// which means typemembers have to be Value2, not ConcreteType2.

trait Environment {
  def lookupType(name: String): ITemplata;
  def globalEnv: GlobalEnvironment;
}

trait IVariable2 {
  def reference: Reference2
}
case class LocalVariable2(reference: Reference2) extends IVariable2
case class ClosureVariable2(
    closuredVarsStructType: StructRef2,
    reference: Reference2) extends IVariable2

case class GlobalEnvironment(
    // Non-templated functions without return types.
    // If we know the return type, it will also be in functionHeaders.
    // This is used to know what exists, and can be put into function families,
    // so we don't have to stamp something to fill its spot.
    // Between this and functionTemplates, we can do overload resolution.
    ordinaryBanners: Map[String, Set[FunctionBanner2]],
    // If you want to know the return type of a function, check the temputs.

    functionTemplates: Map[String, List[Function1]],

    typeMembers: Map[String, ITemplata],

    valueMembers: Map[String, IValueTemplata],

    // These are Impl1 instead of an Impl2 because theyre templated
    impls: Map[String, List[Impl1]]) extends Environment {

  assert(ordinaryBanners.size == ordinaryBanners.toSet.size)

  def spawnLocalEnv(): LocalEnvironment = {
    LocalEnvironment(this, Map(), Map())
  }


  def getTerryLocalEnv(terry: TemplataFunctionTerry): TerryEnvironment = {
    terry.outerEnv match {
      case None => TerryEnvironment(spawnLocalEnv())
      case Some(terryLocalEnv) => TerryEnvironment(terryLocalEnv)
    }
  }

  override def globalEnv: GlobalEnvironment = this;

  def addImpl(impl1: Impl1): GlobalEnvironment = {
    impls.get(impl1.structName) match {
      case None => {
        GlobalEnvironment(
          ordinaryBanners,
          functionTemplates,
          typeMembers,
          valueMembers,
          impls + (impl1.structName -> List(impl1)))
      }
      case Some(existingList) => {
        GlobalEnvironment(
          ordinaryBanners,
          functionTemplates,
          typeMembers,
          valueMembers,
          impls + (impl1.structName -> (impl1 :: existingList)))
      }
    }
  }

  def addType(name: String, newTemplata: ITemplata): GlobalEnvironment = {
    typeMembers.get(name) match {
      case None => {
        GlobalEnvironment(
          ordinaryBanners,
          functionTemplates,
          typeMembers + (name -> newTemplata),
          valueMembers,
          impls)
      }
      case Some(existingTemplata) => {
        // would it make sense to have multiple under the same name?
        // would that be like... templated structs or something?
        throw new RuntimeException("wat " + name + " " + newTemplata + " old " + existingTemplata)
      }
    }
  }

  // the name parameter is used instead of banner.name for aliasing/overloading, like print and __printInt.
  def addFunctionBanner(name: String, banner: FunctionBanner2): GlobalEnvironment = {
    val oldOrdinaryBannersWithThisName = ordinaryBanners.getOrElse(name, Set());
    if (oldOrdinaryBannersWithThisName.contains(banner)) {
      throw new RuntimeException("Already added banner for " + banner)
    }
    val newOrdinaryBannersWithThisName = oldOrdinaryBannersWithThisName + banner;

    GlobalEnvironment(
      ordinaryBanners + (name -> newOrdinaryBannersWithThisName),
      functionTemplates,
      typeMembers,
      valueMembers,
      impls)
  }

  def addFunctionTemplate(function1: Function1): GlobalEnvironment = {
    assert(function1.templateParams.nonEmpty);
    GlobalEnvironment(
      ordinaryBanners,
      functionTemplates + (function1.name -> (function1 :: functionTemplates.getOrElse(function1.name, List()))),
      typeMembers,
      valueMembers,
      impls)
  }

  // when we come across an un-templated
  override def lookupType(name: String): ITemplata = {
    typeMembers.get(name) match {
      case None => {
        println(this);
        typeMembers.foreach(tm => println("Type: " + tm))
        throw new RuntimeException("Couldn't find " + name + ", see above for env")
      }
      case Some(templata) => templata
    }
  }
}

// A marker class that says yes, this came from the terry, we're using the right env.
case class TerryEnvironment(localEnv: LocalEnvironment) {
  def addVariables(newVariables: Map[String, IVariable2]): TerryEnvironment = {
    TerryEnvironment(localEnv.addVariables(newVariables))
  }
  def addType(name: String, newTemplata: ITemplata): TerryEnvironment = {
    TerryEnvironment(localEnv.addType(name, newTemplata))
  }
}

case class LocalEnvironment(
    globalEnv: GlobalEnvironment,
    variables: Map[String, IVariable2],
    typeMembers: Map[String, ITemplata]) extends Environment {

  def addVariables(newVariables: Map[String, IVariable2]): LocalEnvironment = {
    LocalEnvironment(
      globalEnv,
      variables ++ newVariables,
      typeMembers)
  }

  def addType(name: String, newTemplata: ITemplata): LocalEnvironment = {
    LocalEnvironment(
      globalEnv,
      variables,
      typeMembers + (name -> newTemplata))
  }

  override def lookupType(name: String): ITemplata = {
    typeMembers.get(name) match {
      case Some(templata) => templata
      case None => {
        globalEnv.typeMembers.get(name) match {
          case Some(templata) => templata
          case None => {
            println(this);
            typeMembers.foreach(tm => println("Type: " + tm))
            throw new RuntimeException("Couldn't find " + name + ", see above for env")
          }
        }
      }
    }
  }
}
