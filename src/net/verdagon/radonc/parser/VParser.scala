package net.verdagon.radonc

import scala.io.Source._
import scala.util.parsing.combinator._
import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar.{Override2, Virtual2, Virtuality2};

object VParser extends RegexParsers {
  override def skipWhitespace = false
  override val whiteSpace = "[ \t\r\f]+".r

  private def exprIdentifier: Parser[String] = {
    """[^\s\.\$\&\,\:\(\)\;\[\]\{\}]+""".r
  }

  private def typeIdentifier: Parser[String] = {
    """[^\s\.\!\?\#\$\&\,\:\;\(\)\[\]\{\}]+""".r
  }
//  def exprIdentifier: Parser[String] = {
//    """[^\d\s\$\&\,\:\(\)\;\[\]\{\}][^\s\$\&\,\:\(\)\;\[\]\{\}]*""".r
//  }
//
//  def typeIdentifier: Parser[String] = {
//    """[^\d\s\.\!\?\#\$\&\,\:\;\(\)\[\]\{\}][^\s\.\!\?\#\$\&\,\:\;\(\)\[\]\{\}]*""".r
//  }

  //  def literal: Parser[Literal] = {
  //    """"[^"]*"""".r ^^ { str =>
  //      Literal(str.substring(0, str.length - 0))
  //    }
  //  }

  private def white: Parser[Unit] = { "\\s+".r ^^^ Unit }
  private def optWhite: Parser[Unit] = { opt(white) ^^^ Unit }

  private def destructurePattern: Parser[Pattern1] = {
    typeIdentifier ~ elementsPattern ^^ { case s ~ p => DestructureP1(s, p) }
  }
  
  private def packRest: Parser[PackRestP1] = {
    "..." ~> optWhite ~> typePattern ^^ PackRestP1
  }
  
  private def templatePackRest: Parser[TemplatePackRestP1] = {
    "#" ~> optWhite ~> "..." ~> optWhite ~> typeIdentifier ^^ TemplatePackRestP1
  }

  private def typeOfWithTemplateArgSinglePattern: Parser[TypeOfP1] = {
    (typeIdentifier <~ optWhite <~ ":" <~ optWhite) ~ tyype ^^ {
      case templateType ~ templateArgType => {
        TypeOfP1(TemplateCall1(templateType, List(templateArgType)))
      }
    }
  }

  private def typeOfWithTemplateArgMultiplePattern: Parser[TypeOfP1] = {
    (typeIdentifier <~ optWhite <~ ":" <~ optWhite) ~ ("(" ~> optWhite ~> repsep(tyype, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")") ^^ {
      case templateType ~ templateArgTypes => {
        TypeOfP1(TemplateCall1(templateType, templateArgTypes))
      }
    }
  }

  private def typeOfWithTemplateArgPattern: Parser[TypeOfP1] = {
    typeOfWithTemplateArgSinglePattern | typeOfWithTemplateArgMultiplePattern
  }

  private def typeOfPattern: Parser[TypeOfP1] = {
    typeIdentifier ^^ {
      case typeName => TypeOfP1(TypeName1(typeName))
    }
  }
  
  private def typePackPattern: Parser[List[Pattern1]] = {
    "(" ~> optWhite ~> repsep(optWhite ~> typePattern, optWhite ~> ",") <~ optWhite <~ ")"
  }

  private def templatedType: Parser[TemplateCall1] = {
    (typeIdentifier <~ optWhite <~ ":" <~ optWhite) ~ tyype ^^ {
      case templateTypeName ~ templateArgType => {
        TemplateCall1(templateTypeName, List(templateArgType))
      }
    }
  }
  
  private def tyype: Parser[Type1] = {
//    ("#" ~> typeIdentifier ^^ TemplateParamName1) |
    ("?" ~> tyype ^^ Nullable1) |
    templatedType |
    (typeIdentifier ^^ TypeName1)
  }
  
  private def callablePattern: Parser[Pattern1] = {
    typePackPattern ~ tyype ^^ {
      case params ~ returnType => CallableP1(params, returnType)
    }
  }

  private def typePattern: Parser[Pattern1] = {
    tuplePattern |
    packRest |
    templatePackRest |
    callablePattern |
    destructurePattern |
    typeOfWithTemplateArgPattern |
    typeOfPattern
  }

  private def capturePattern: Parser[Pattern1] = {
    opt(optWhite ~> "mut" <~ optWhite) ~ exprIdentifier ~ opt(optWhite ~> ":" ~> optWhite ~> typePattern) ^^ {
      case maybeMutable ~ ident ~ p => CaptureP1(ident, maybeMutable.nonEmpty, p)
    }
  }

  private def discardPattern: Parser[Pattern1] = {
    ":" ~> typePattern
  }

  private def underscoreDiscardPattern: Parser[Pattern1] = {
    "_" ~> opt(":" ~> typePattern) ^^ {
      case Some(tyype) => tyype
      case None => DiscardP1()
    }
  }

  // Can have sum([a: Int, b:Int]) and even sum(things:[a: Int, b: Int])
  // In other words, tuples can appear in both the element part and
  // the type part.
  private def tuplePattern: Parser[Pattern1] = {
    "[" ~> optWhite ~> repsep(optWhite ~> elementPattern, optWhite ~> ",") <~ optWhite <~ "]" ^^ TupleP1
  }

  private def packPattern: Parser[Pattern1] = {
    "(" ~> optWhite ~> repsep(optWhite ~> elementPattern, optWhite ~> ",") <~ optWhite <~ ")" ^^ {
      case elements => {
        elements match {
          case List(onlyElement) => onlyElement
          case _ => PackP1(elements)
        }
      }
    }
  }

  // In (a: Int, b: Char) the elements are "a: Int" and "b: Char", also
  // in [a: Int, b: Char].
  private def elementPattern: Parser[Pattern1] = {
    underscoreDiscardPattern |
    discardPattern |
    capturePattern |
    packPattern |
    tuplePattern
  }

  def elementsPattern: Parser[List[Pattern1]] = {
    "(" ~> optWhite ~> repsep(optWhite ~> elementPattern, optWhite ~> ",") <~ optWhite <~ ")"
  }

  private def virtuality: Parser[Virtuality2] = {
    ("virtual" ^^^ Virtual2()) |
    ("override" ^^^ Override2())
  }

  private def interfacePrototypeParam: Parser[Parameter0] = {
    opt(virtuality <~ optWhite) ~ opt(optWhite ~> "$") ~ (optWhite ~> exprIdentifier) ~ (optWhite ~> ":" ~> optWhite ~> tyype) ^^ {
      case optVirtuality ~ maybeMutable ~ ident ~ tyype =>
        Parameter0(optVirtuality, CaptureP1(ident, maybeMutable.nonEmpty, Some(TypeOfP1(tyype))))
    }
  }

  private def lookup: Parser[Lookup0] = {
    exprIdentifier ^^ Lookup0
  }

  private def templateSpecifiedLookupMultiple: Parser[Expression0] = {
    (exprIdentifier <~ optWhite <~ ":" <~ optWhite) ~ ("(" ~> optWhite ~> repsep(tyype, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")") ^^ {
      case name ~ templateArgs => TemplateSpecifiedLookup0(name, templateArgs)
    }
  }

  private def templateSpecifiedLookupSingle: Parser[Expression0] = {
    (exprIdentifier <~ optWhite <~ ":" <~ optWhite) ~ tyype ^^ {
      case name ~ tyype => TemplateSpecifiedLookup0(name, List(tyype))
    }
  }

  private def templateSpecifiedLookup: Parser[Expression0] = {
    templateSpecifiedLookupMultiple | templateSpecifiedLookupSingle
  }

  private def let: Parser[Let0] = {
    ("let" ~> white ~> elementPattern <~ white <~ "=" <~ white) ~ expression ^^ {
      case pattern ~ expr => Let0(pattern, expr)
    }
  }

  private def lend: Parser[Expression0] = {
    "&" ~> optWhite ~> expression ^^ Lend0
  }

  private def mutable: Parser[Expression0] = {
    "$" ~> optWhite ~> expression ^^ Mutable0
  }

  private def mutate: Parser[Expression0] = {
    ("mut" ~> white ~> exprIdentifier <~ white <~ "=" <~ white) ~ expression ^^ {
      case name ~ expr => Mutate0(Lookup0(name), expr)
    }
  }

  private def ifPart: Parser[(Lambda0, Lambda0)] = {
    ("if" ~> optWhite ~> paramLessLambda <~ optWhite) ~ paramLessLambda ^^ {
      case condLambda ~ thenLambda => (condLambda, thenLambda)
    }
  }

  private def ifLadder: Parser[Expression0] = {
    (ifPart <~ optWhite) ~
    repsep(optWhite ~> "else" ~> optWhite ~> ifPart <~ optWhite, optWhite) ~
    opt(optWhite ~> "else" ~> optWhite ~> paramLessLambda) ^^ {
      case rootIf ~ ifElses ~ maybeElseLambda => {
        val finalElse: Lambda0 =
          maybeElseLambda match {
            case None => Lambda0(Function0(None, false, false, List(), List(), None, Some(Block0(List(Pack0(List()))))))
            case Some(lam) => { lam }
          }
        val rootElseLambda =
          ifElses.foldRight(finalElse)({
            case ((conditionLambda, thenLambda), elseLambda) => {
              Lambda0(Function0(None, false, false, List(), List(), None, Some(Block0(List(If0(conditionLambda, thenLambda, elseLambda))))))
            }
          })
        val (rootConditionLambda, rootThenLambda) = rootIf
        If0(rootConditionLambda, rootThenLambda, rootElseLambda)
      }
    }
  }

  private def integer: Parser[Expression0] = {
    raw"^-?\d+".r ^^ {
      case thingStr => IntLiteral1(thingStr.toInt)
    }
  }

  private def float: Parser[Expression0] = {
    raw"^-?\d+\.\d*".r ^^ {
      case thingStr => FloatLiteral1(thingStr.toFloat)
    }
  }

  private def string: Parser[Expression0] = {
    "\"" ~> "[^\"]*".r <~ "\"" ^^ {
      case thingStr => StrLiteral1(thingStr)
    }
  }

  private def expressionElementLevel1: Parser[Expression0] = {
    string | float | integer | ifLadder | let | mutate | lend | mutable | lambda | packExpr | tupleExpr | templateSpecifiedLookup | lookup
  }

  private def dotWithName: Parser[Expression0] = {
    expressionElementLevel1 ~ rep1(optWhite ~> "." ~> optWhite ~> exprIdentifier) ^^ {
      case expr ~ members => {
        members.foldLeft(expr)((e: Expression0, m: String) => Dot0(e, m))
      }
    }
  }

  private def dotWithParen: Parser[Expression0] = {
    expressionElementLevel1 ~ rep1(optWhite ~> "." ~> optWhite ~> packExpr) ^^ {
      case expr ~ members => {
        members.foldLeft(expr)((e: Expression0, pack: Pack0) => DotCall0(e, pack))
      }
    }
  }

  private def expressionElementLevel2: Parser[Expression0] = {
    dotWithParen | dotWithName | expressionElementLevel1
  }

  def expression: Parser[Expression0] = {
    (expressionElementLevel2 <~ optWhite) ~ repsep(expressionElementLevel2, optWhite) ^^ {
      case a ~ b => {
        if (b.isEmpty) a else Scramble0(a :: b)
      }
    }
  }

  private def block: Parser[Block0] = {
    repsep(expression, optWhite ~> ";" <~ optWhite) ~ opt(";") ^^ {
      case Nil ~ _ => Block0(List(Pack0(List())))
      case List(a) ~ None => Block0(List(a))
      case a ~ None => Block0(a)
      case a ~ Some(_) => Block0(a ++ List(scout.Pack0(List())))
    }
  }

  private def tupleExpr: Parser[Expression0] = {
    "[" ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ "]" ^^ {
      (a:List[Expression0]) => {
        scout.Sequence0(a)
      }
    }
  }

  private def packExpr: Parser[Pack0] = {
    "(" ~> repsep(expression, optWhite ~> "," <~ optWhite) <~ ")" ^^ {
      a => {
        scout.Pack0(a)
      }
    }
  }

  private def paramLambda: Parser[Lambda0] = {
    ("{" ~> patternPrototypeParams) ~ opt(tyype) ~ (optWhite ~> block <~ optWhite <~ "}") ^^ {
      case patternParams ~ maybeReturn ~ maybeBody => Lambda0(Function0(None, false, false, List(), patternParams, maybeReturn, Some(maybeBody)))
    }
  }

  private def paramLessLambda: Parser[Lambda0] = {
    "{" ~> optWhite ~> block <~ optWhite <~ "}" ^^ {
      case b => Lambda0(Function0(None, false, false, List(), List(), None, Some(b)))
    }
  }

  private def lambda: Parser[Lambda0] = {
    (paramLambda | paramLessLambda)
  }



  private def templateParamsMultiple: Parser[List[TemplateParameter1]] = {
    ":" ~> optWhite ~> "(" ~> repsep(exprIdentifier, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")" ^^ {
      case names => names.map(name => TemplateParameter1(name, ReferenceTemplataType1))
    }
  }

  private def templateParamsSingle: Parser[List[TemplateParameter1]] = {
    ":" ~> optWhite ~> exprIdentifier ^^ {
      case name => List(TemplateParameter1(name, ReferenceTemplataType1))
    }
  }

  private def templateParams: Parser[List[TemplateParameter1]] = {
    templateParamsMultiple | templateParamsSingle
  }

  private def patternPrototypeParam: Parser[Parameter0] = {
    opt(virtuality) ~ (optWhite ~> elementPattern) ^^ {
      case maybeVirtuality ~ p => {
        Parameter0(maybeVirtuality, p)
      }
    }
  }

  private def patternPrototypeParams: Parser[List[Parameter0]] = {
    "(" ~> optWhite ~> repsep(optWhite ~> patternPrototypeParam, optWhite ~> ",") <~ optWhite <~ ")"
  }

//  def patternPrototype: Parser[PatternPrototype0] = {
//    ("fn" ~> optWhite ~> exprIdentifier) ~ opt(optWhite ~> templateParams <~ optWhite) ~ (optWhite ~> patternPrototypeParams <~ optWhite) ~ (optWhite ~> opt(tyype) <~ optWhite) ^^ {
//      case name ~ templateParamNames ~ paramPatterns ~ maybeReturnType => {
//        PatternPrototype0(
//          name,
//          PatternSignature0(
//            templateParamNames.getOrElse(List()),
//            paramPatterns,
//            maybeReturnType))
//      }
//    }
//  }

  def body: Parser[Option[Block0]] = {
    optWhite ~> "{" ~> optWhite ~> block <~ optWhite <~ "}" <~ optWhite ^^ {
      case body => Some(body)
    }
  }

  def noBody: Parser[Option[Block0]] = {
    (optWhite ~> ";" <~ optWhite) ^^^ None
  }

  def maybeBody = body | noBody

  def topLevelFunction: Parser[Function0] = {
        (optWhite ~> opt("abstract") <~ optWhite) ~
        (optWhite ~> opt("extern") <~ optWhite) ~
        ("fn" ~> optWhite ~> exprIdentifier) ~
        opt(optWhite ~> templateParams <~ optWhite) ~
        (optWhite ~> patternPrototypeParams <~ optWhite) ~
        (optWhite ~> opt(tyype) <~ optWhite) ~
        (optWhite ~> maybeBody <~ optWhite) ^^ {
      case maybeAbstract ~ maybeExtern ~ name ~ maybeTemplateParamNames ~ patternParams ~ maybeReturnType ~ maybeBody => {
        Function0(Some(name), maybeExtern.nonEmpty, maybeAbstract.nonEmpty, maybeTemplateParamNames.getOrElse(List()), patternParams, maybeReturnType, maybeBody)
      }
    }
  }

  private def structMember: Parser[StructMember1] = {
    (exprIdentifier <~ optWhite <~ ":" <~ optWhite) ~ (tyype <~ optWhite <~ ";") ^^ {
      case name ~ tyype => StructMember1(name, tyype)
    }
  }

  private def structTemplateParamsMultiple: Parser[List[String]] = {
    ":" ~> optWhite ~> "(" ~> repsep(exprIdentifier, optWhite ~> "," <~ optWhite) <~ optWhite <~ ")" ^^ {
      case names => names
    }
  }

  private def interface: Parser[Interface0] = {
    ("interface " ~> optWhite ~> exprIdentifier <~ optWhite) ~
        (opt("imm") <~ optWhite) ~
        (opt(templateParams) <~ optWhite <~ "{" <~ optWhite) ~
        repsep(topLevelFunction, optWhite) <~ (optWhite <~ "}") ^^ {
      case name ~ imm ~ templateParams ~ members => {
        val mutability = if (imm == Some("imm")) Immutable else Mutable
        Interface0(name, mutability, templateParams.getOrElse(List()), members)
      }
    }
  }

  private def struct: Parser[Struct0] = {
    ("struct " ~> optWhite ~> exprIdentifier <~ optWhite) ~ (opt("imm") <~ optWhite) ~ (opt(templateParams) <~ optWhite <~ "{" <~ optWhite) ~ repsep(structMember, optWhite) <~ (optWhite <~ "}") ^^ {
      case name ~ imm ~ templateParams ~ members => {
        val mutability = if (imm == Some("imm")) Immutable else Mutable
        Struct0(name, mutability, templateParams.getOrElse(List()), members)
      }
    }
  }

  private def impl: Parser[Impl1] = {
    (typeIdentifier <~ optWhite) ~ (opt(templateParams) <~ optWhite <~ "implements" <~ optWhite) ~ (tyype <~ ";") ^^ {
      case structName ~ structTemplateParams ~ interfaceType => {
        Impl1(structName, structTemplateParams.getOrElse(List()), interfaceType)
      }
    }
  }

  private def topLevelThing: Parser[TopLevelThing0] = {
    struct ^^ TopLevelStruct |
    topLevelFunction ^^ TopLevelFunction |
    interface ^^ TopLevelInterface |
    impl ^^ TopLevelImplements
  }
  
  def program: Parser[Program0] = {
    optWhite ~> repsep(topLevelThing, optWhite) <~ optWhite ^^ {
      case tlts => {
        val typeDefs: List[TypeDefinition0] =
          tlts.collect({
            case TopLevelStruct(s) => s
            case TopLevelInterface(i) => i
          });
        val impls: List[Impl1] =
          tlts.collect({ case TopLevelImplements(i) => i });
        val functions: List[Function0] =
          tlts.collect({ case TopLevelFunction(f) => f });
        Program0(typeDefs, impls, functions)
      }
    }
  }
}

