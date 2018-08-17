package net.verdagon.radonc

import net.verdagon.radonc.carpenter.Carpenter
import net.verdagon.radonc.hammer._
import net.verdagon.radonc.scout.{TemplateParameter1, _}
import net.verdagon.radonc.templar._
import net.verdagon.radonc.sculptor._

import scala.collection.immutable.TreeMap
import scala.collection.mutable.ListBuffer

object CommonEnv {

  val builtInExternFunctions = Map[String, List[FunctionHeader2]](
//    ("__printInt" -> List(FunctionHeader2("__printInt", List(Parameter2("left", None, Reference2(Owning, Int2()))), Reference2(Owning, Void2())))),
//    ("__negateInt" -> List(FunctionHeader2("__negateInt", List(Parameter2("left", None, Reference2(Owning, Int2()))), Reference2(Owning, Int2())))),
//    ("__negateFloat" -> List(FunctionHeader2("__negateFloat", List(Parameter2("left", None, Reference2(Owning, Float2()))), Reference2(Owning, Float2())))),
//    ("__addIntInt" -> List(FunctionHeader2("__addIntInt", List(Parameter2("left", None, Reference2(Owning, Int2())), Parameter2("left", None, Reference2(Owning, Int2()))), Reference2(Owning, Int2())))),
//    ("__castFloatInt" -> List(FunctionHeader2("__castFloatInt", List(Parameter2("left", None, Reference2(Owning, Float2()))), Reference2(Owning, Int2())))),
//    ("__castIntInt" -> List(FunctionHeader2("__castIntInt", List(Parameter2("left", None, Reference2(Owning, Float2()))), Reference2(Owning, Int2())))),
//    ("__addStrStr" -> List(FunctionHeader2("__addStrStr", List(Parameter2("left", None, Reference2(Owning, Str2())), Parameter2("left", None, Reference2(Owning, Str2()))), Reference2(Owning, Str2())))),
//    ("__lessThanFloat" -> List(FunctionHeader2("__lessThanFloat", List(Parameter2("left", None, Reference2(Owning, Float2())), Parameter2("left", None, Reference2(Owning, Float2()))), Reference2(Owning, Bool2())))))
  )
  val builtInExternFunctionsNames = Set("__MutArray", "__ImmArray") ++ builtInExternFunctions.keySet;

  val builtInValues = Map[String, IValueTemplata](
    ("__true", BooleanTemplata(true)),
    ("__false", BooleanTemplata(false)),
    ("__none", NoneTemplata())
  );
  val builtInValuesNames = builtInValues.keySet;

  val commonExternFunctions = builtInExternFunctions ++ Map[String, List[FunctionHeader2]](
    ("print" -> List(
      FunctionHeader2("__printInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2()))), Reference2(Share, Void2()), None))),
    ("+" -> List(
      FunctionHeader2("__addIntInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2())), Parameter2("right", None, Reference2(Share, Int2()))), Reference2(Share, Int2()), None),
      FunctionHeader2("__addStrStr", false, true, List(), List(Parameter2("left", None, Reference2(Share, Str2())), Parameter2("right", None, Reference2(Share, Str2()))), Reference2(Share, Str2()), None))),
    ("Float" -> List(
      FunctionHeader2("__castIntFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2()))), Reference2(Share, Float2()), None),
      FunctionHeader2("__castFloatFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2()))), Reference2(Share, Float2()), None))),
    ("Int" -> List(
      FunctionHeader2("__castIntInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2()))), Reference2(Share, Int2()), None),
      FunctionHeader2("__castFloatInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2()))), Reference2(Share, Int2()), None))),
    ("-" -> List(
      FunctionHeader2("__negateInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2()))), Reference2(Share, Int2()), None),
      FunctionHeader2("__negateFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2()))), Reference2(Share, Float2()), None),
      FunctionHeader2("__subtractIntInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2())), Parameter2("right", None, Reference2(Share, Int2()))), Reference2(Share, Int2()), None),
      FunctionHeader2("__subtractFloatInt", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2())), Parameter2("right", None, Reference2(Share, Int2()))), Reference2(Share, Float2()), None),
      FunctionHeader2("__subtractIntFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Int2())), Parameter2("right", None, Reference2(Share, Float2()))), Reference2(Share, Float2()), None),
      FunctionHeader2("__subtractFloatFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2())), Parameter2("right", None, Reference2(Share, Float2()))), Reference2(Share, Float2()), None))),
    ("<" -> List(
      FunctionHeader2("__lessThanFloat", false, true, List(), List(Parameter2("left", None, Reference2(Share, Float2())), Parameter2("right", None, Reference2(Share, Float2()))), Reference2(Share, Bool2()), None))))
  val commonExternFunctionsNames = commonExternFunctions.keySet;

  val commonValues = builtInValues ++ Map[String, IValueTemplata](
    ("true", BooleanTemplata(true)),
    ("false", BooleanTemplata(false)),
    ("none", NoneTemplata())
  );
  val commonValuesNames = commonValues.keySet;

  val builtInTypes = Map[String, ITemplata](
    ("__int" -> ReferendTemplata(Int2())),
    ("__str" -> ReferendTemplata(Str2())),
    ("__float" -> ReferendTemplata(Float2())),
    ("__Nothing" -> ReferendTemplata(Nothing2())),
    ("__MutArray" -> ArrayTemplateTemplata(false)),
    ("__ImmArray" -> ArrayTemplateTemplata(true)),
  );
  val builtInTypesNames = builtInTypes.keySet;

  val commonTypes = builtInTypes ++ Map[String, ITemplata](
    ("Int" -> ReferendTemplata(Int2())),
    ("Bool" -> ReferendTemplata(Bool2())),
    ("Str" -> ReferendTemplata(Str2())),
    ("Float" -> ReferendTemplata(Float2())),
    ("None" -> ReferendTemplata(Nothing2())),
    ("Void" -> ReferendTemplata(Void2()))
  );
  val commonTypesNames = commonTypes.keySet;

  def runParser(codeWithComments: String): Option[Program0] = {
    val regex = "//[^\\r\\n]*".r
    val code = regex.replaceAllIn(codeWithComments, "")

    VParser.parse(VParser.program, code.toCharArray()) match {
      case VParser.NoSuccess(msg, input) => {
        println("No! " + msg)
        None
      }
      case VParser.Success(program0, rest) => {
        assert(rest.atEnd)
        assert(rest.offset == code.length)
        Some(program0)
      }
    }
  }

  def runNamifier(program0: Program0): Program1 = {
    val Program1(originalTypeDefinitions, originalImpls, originalImplementedFunctions) =
      Scout.namifyProgram(
        commonExternFunctionsNames ++ commonValuesNames ++ commonTypesNames, program0)
    val params =
      List(
        Parameter1(None, 1, CaptureP1("size", false, Some(TypeOfP1(TypeName1("Int"))))),
        Parameter1(None, 1, CaptureP1("generator", false, Some(TypeOfP1(TypeName1("F"))))))
    val mutableArrayFunction =
      Function1(
        "__MutArray",
        false,
        false,
        Set(),
        List(
          TemplateParameter1("F", ReferenceTemplataType1)),
        params,
        None,
        Some(
          Block1(
            List(
              ConstructArray1(
                Lookup1("size"),
                Lookup1("generator"),
                Mutable)))))
    val immutableArrayFunction =
      Function1(
        "__ImmArray",
        false,
        false,
        Set(),
        List(
          TemplateParameter1("F", ReferenceTemplataType1)),
        params,
        None,
        Some(
          Block1(
            List(
              ConstructArray1(
                Lookup1("size"),
                Lookup1("generator"),
                Immutable)))))
    val program1 =
      Program1(
        originalTypeDefinitions,
        originalImpls,
        mutableArrayFunction :: immutableArrayFunction :: originalImplementedFunctions)
    program1
  }

  def runNamifier(code: String): Option[Program1] = {
    runParser(code) match {
      case None => None
      case Some(program0) => {
        Some(runNamifier(program0))
      }
    }
  }

  def runTemplar(program1: Program1): CompleteProgram2 = {
    Templar.evaluate(program1, commonTypes, commonExternFunctions, commonValues)
  }

  def runTemplar(code: String): Option[CompleteProgram2] = {
    runNamifier(code) match {
      case None => None
      case Some(program1) => Some(runTemplar(program1))
    }
  }

  def runCarpenter(code: String): Option[Hinputs] = {
    runTemplar(code) match {
      case None => None
      case Some(program2) => {
        Some(Carpenter.translate(program2))
      }
    }
  }

  def runHammer(code: String): Option[Program3] = {
    runTemplar(code) match {
      case None => None
      case Some(program2) => {
        val hinputs = Carpenter.translate(program2)
        Some(Hammer.translate(hinputs))
      }
    }
  }

  def runSculptor(code: String): Option[String] = {
    runHammer(code) match {
      case None => None
      case Some(program3) => Some(Sculptor.translate(program3))
    }
  }

  def main(argsArray: Array[String]): Unit = {

    def parseOptions(input: List[String]) : (Map[Symbol, Boolean], Map[Symbol, String], List[String]) = {
      input match {
        case Nil => (Map[Symbol, Boolean](), Map[Symbol, String](), List())
        case "-t" :: tail =>
          val (tailBools, tailStrings, tailArgs) = parseOptions(tail)
          (tailBools ++ Map('templar -> true), tailStrings, tailArgs)
        case "-m" :: tail =>
          val (tailBools, tailStrings, tailArgs) = parseOptions(tail)
          (tailBools ++ Map('hammer -> true), tailStrings, tailArgs)
        case "-i" :: tail =>
          val (tailBools, tailStrings, tailArgs) = parseOptions(tail)
          (tailBools ++ Map('incomplete -> true), tailStrings, tailArgs)
        case "-o" :: outputFilename :: tail =>
          val (tailBools, tailStrings, tailArgs) = parseOptions(tail)
          (tailBools, tailStrings ++ Map('outputFilename -> outputFilename), tailArgs)
        case inputFilename :: tail =>
          val (tailBools, tailStrings, tailArgs) = parseOptions(tail)
          (tailBools, tailStrings, tailArgs :+ inputFilename)
      }
    }
    val (boolOptions, stringOptions, args) = parseOptions(argsArray.toList)

    val templar = boolOptions.get('templar).getOrElse(false)
    val hammer = boolOptions.get('hammer).getOrElse(false)
    val outputFilename = stringOptions.get('outputFilename)

    val code: String =
        if (args.length > 0) {
          args.head
        } else {
          var lines: List[String] = Nil;
          for (ln <- io.Source.stdin.getLines) {
            lines = ln :: lines
          };
          lines.reverse.mkString("\n")
        };

    val output =
        if (templar)
          runTemplar(code).toString
        else if (hammer)
          runHammer(code).toString
        else
          runSculptor(code).getOrElse("")

    outputFilename match {
      case None => println(output)
      case Some(outputFilename) => {
        import java.io._
        val pw = new PrintWriter(new File(outputFilename))
        pw.write(output)
        pw.close
      }
    }
  }
}
