package net.verdagon.radonc.scout

import net.verdagon.radonc.templar._
import net.verdagon.radonc.scout

import scala.util.matching.Regex
import scala.collection.immutable.Range

object Scout {
  case class StackFrame(parent: Option[StackFrame], locals: Set[String]) {
    def ++(newNames: Set[String]): StackFrame = {
      StackFrame(parent, locals ++ newNames)
    }
    def allNames: Set[String] = {
      locals ++ parent.map(_.allNames).getOrElse(Set[String]())
    }
    def getJumps(name: String): Option[Int] = {
      if (locals.contains(name)) {
        Some(0)
      } else {
        parent match {
          case None => None
          case Some(parent) => {
            parent.getJumps(name) match {
              case None => None
              case Some(num) => Some(num + 1)
            }
          }
        }
      }
    }
  }

  def namifyProgram(globalNames0: Set[String], program: Program0): Program1 = {
    val Program0(typeDefs0, impls1, functions0) = program;
    val globalNames1 = globalNames0 + "_";
    val typeMemberNames =
      typeDefs0
          .map({
            case Struct0(name, _, _, members) => List(name) ++ members.map(_.name)
            case Interface0(name, _, _, members) => List(name) ++ members.map(_.name.get)
          })
          .foldLeft(List[String]())(_ ++ _);
    val names = globalNames1 ++ functions0.map(_.name.get).toSet ++ typeMemberNames;
    val typeDefs1 = namifyTypeDefs(1, typeDefs0);
    Program1(
      typeDefs1,
      impls1,
      functions0.map(namifyTopLevelFunction(names, _)))
  }

  private def namifyTypeDefs(typeNumber: Int, typeDefs0: List[TypeDefinition0]): List[TypeDefinition1] = {
    typeDefs0 match {
      case Nil => List()
      case headTypeDef0 :: tailTypeDefs0 => {
        val headTypeDef1 =
          headTypeDef0 match {
            case Struct0(name, mutability, templateParamNames, members) =>
              Struct1(typeNumber, name, mutability, templateParamNames, members)
            case Interface0(name, mutability, templateParamNames, members) => {
              Interface1(typeNumber, name, mutability, templateParamNames, members.map(namifyInterfaceMember))
            }
          }
        val tailTypeDefs1 =
          namifyTypeDefs(typeNumber + 1, tailTypeDefs0)
        headTypeDef1 :: tailTypeDefs1
      }
    }
  }

  private def namifyInterfaceMember(function0: Function0): NTVFunction1 = {
    val Function0(
        maybeName,
        isExtern,
        isAbstract,
        templateParamNames,
        params0,
        maybeReturnType,
        maybeBody) = function0;

    if (maybeName.isEmpty) {
      throw new RuntimeException("no");
    }
    if (isExtern) {
      throw new RuntimeException("no");
    }
    if (maybeBody.nonEmpty) {
      throw new RuntimeException("no");
    }
    if (isAbstract) {
      throw new RuntimeException("a bit weird");
    }
    val (_, params1, _) = namifyParameters(0, params0)
    val simpleParams1 = Simplify.simplifyParams(params1)
    val function1 = Function1(maybeName.get, isExtern, isAbstract, Set(), templateParamNames, params1, maybeReturnType, None);
    NTVFunction1(function1, maybeName.get, templateParamNames, simpleParams1)
  }

  // returns seq num, new parameters, exported template names from typeless params, and capture names
  private def namifyParameters(seqNum0: Int, params: List[Parameter0]):
      (Int, List[Parameter1], List[String]) = {
    params match {
      case Nil => (seqNum0, List(), List())
      case first0 :: rest0 => {
        val (seqNum1, first1, firstTemplateParamNames) = namifyParameter(seqNum0, first0);
        val (seqNum2, rest1, restTemplateParamNames) = namifyParameters(seqNum1, rest0);
        (seqNum2, first1 :: rest1, firstTemplateParamNames ++ restTemplateParamNames)
      }
    }
  }

  // returns seq num, new parameter, exported template names from typeless params, and capture names
  private def namifyParameter(seqNum0: Int, param0: Parameter0):
      (Int, Parameter1, List[String]) = {
    val Parameter0(virtuality, pattern1) = param0;
    pattern1 match {
      case DiscardP1() => {
        val newTemplateParamName = "__T" + seqNum0;
        val param = Parameter1(virtuality, seqNum0 + 1, CaptureP1("__P" + seqNum0, false, Some(TypeOfP1(TypeName1(newTemplateParamName)))))
        (seqNum0 + 2, param, List(newTemplateParamName))
      }
      case CaptureP1(name, mutable, None) => {
        val newTemplateParamName = "__T" + seqNum0;
        val param = Parameter1(virtuality, seqNum0 + 1, CaptureP1(name, mutable, Some(TypeOfP1(TypeName1(newTemplateParamName)))))
        (seqNum0 + 2, param, List(newTemplateParamName))
      }
      case CaptureP1(name, mutable, Some(innerPattern1)) => {
        val param = Parameter1(virtuality, seqNum0, CaptureP1(name, mutable, Some(innerPattern1)))
        (seqNum0 + 1, param, List())
      }
      case pattern1 => {
        val param = Parameter1(virtuality, seqNum0 + 1, CaptureP1("__P" + seqNum0, false, Some(pattern1)))
        (seqNum0 + 2, param, List())
      }
    }
//    val Parameter0(maybeName, mutable, maybeType) = param0;
//    val (seqNum1, name) =
//        maybeName match {
//          case Some(name) => (seqNum0, name)
//          case None => {
//            (seqNum0 + 1, "__P" + seqNum0)
//          }
//        };
//    val (seqNum3: Int, paramType1: ParameterType1) =
//        maybeType match {
//          case Some(ParamPattern0(pattern1)) => {
//            (seqNum1, ParamPattern1(pattern1))
//          }
//          case Some(ParamTemplate0(templateParamName)) => {
//            (seqNum1, List(), ParamTemplate1(templateParamName))
//          }
//          case None => {
//            (seqNum1 + 1, List(), "__T" + seqNum1)
//          }
//        };
//    (seqNum3, Parameter1(name, mutable, paramType1))
  }

//  // returns seq num, captured names, and new pattern
//  def namifyPattern(seqNum0: Int, names: Set[String], pattern0: Pattern0):
//      (Int, List[String], Pattern1) = {
//    pattern0 match {
//      case Discard0() => {
//        (seqNum0, List(), Discard1())
//      }
//      case Capture0(name, mutable, None) => {
//        (seqNum0, List(), Capture1(name, mutable, None))
//      }
//      case Capture0(name, mutable, Some(inner0)) => {
//        val (seqNum1, innerCapturedNames, inner1) =
//            namifyPattern(seqNum0, names, inner0);
//        (seqNum0, List(name), Capture1(name, mutable, inner1))
//      }
//      case _ => {
////        val newName = "__P" + seqNum0
////        val seqNum1 = seqNum0 + 1
////        val (seqNum2, capturedNames, pattern1) = namifyPattern(seqNum1, names, pattern0)
////        (seqNum2, List(newName) ++ capturedNames, Capture1(newName, false, pattern1))
//        throw new RuntimeException("wat")
//      }
//    }
//  }

  private def namifyTopLevelFunction(
      globalNames: Set[String],
      function0: Function0):
  Function1 = {
    val Function0(Some(name), isExtern, isAbstract, explicitTemplateParams0, paramList0, ret, maybeBody0) = function0;
    val seqNum0 = 1;
    val (seqNum1, explicitParams1, exportedTemplateParamNames) =
      namifyParameters(seqNum0, paramList0);
    assert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val captureNames =
      explicitParams1.map(_.capturePattern).flatMap(PatternTemplar.getPatternCaptureNames)

    val myStackFrame = StackFrame(None, explicitTemplateParams0.map(_.name).toSet ++ captureNames.toSet)
    val (maybeBody1, closuredNames) =
      maybeBody0 match {
        case Some(body0) => {
          val (seqNum2, _, body1, closuredNames, magicNum0) =
            namifyBlock(name, seqNum1, myStackFrame, globalNames, body0, 0)
          assert(magicNum0 == 0)
          (Some(body1), closuredNames)
        }
        case None => (None, Set[String]())
      };


    val templateParamNames = explicitTemplateParams0 ++ exportedTemplateParamNames.map(name => TemplateParameter1(name, ReferenceTemplataType1));

    Function1(name, isExtern, isAbstract, closuredNames, templateParamNames, explicitParams1, ret, maybeBody1)
  }

  private def namifyLambda(
      tlfName: String,
      seqNum0: Int,
      parentStackFrame: Option[StackFrame],
      globalNames: Set[String],
      lambdaFunction0: Function0):
      (Int, Function1) = {
    val Function0(_, false, false, explicitTemplateParams0, paramList0, ret, Some(body0)) = lambdaFunction0;
    val funcId = seqNum0
    val seqNum1 = seqNum0 + 1
    val funcName = tlfName + ":lam" + funcId

    val (seqNum2, explicitParams1, exportedTemplateParamNames) =
      namifyParameters(seqNum1, paramList0);
    assert(exportedTemplateParamNames.size == exportedTemplateParamNames.toSet.size)

    val captureNames =
      explicitParams1.map(_.capturePattern).flatMap(PatternTemplar.getPatternCaptureNames)

    val myStackFrame = StackFrame(parentStackFrame, explicitTemplateParams0.map(_.name).toSet ++ captureNames.toSet)
    val (seqNum3, _, body1, closuredNames, magicNum0) =
      namifyBlock(tlfName, seqNum2, myStackFrame, globalNames, body0, 0)

    if ((magicNum0 != 0) && (explicitParams1.size != 0)) {
      throw new RuntimeException("Cant have a lambda with _ and params");
    }

    val magicParamNames = Range(0, magicNum0).map("__P" + _)
    val magicParamTemplateParamNames = Range(0, magicNum0).map("__T" + _)
    val magicParamPatternId = Range(0, magicNum0);
    val magicParams =
      magicParamNames.zip(magicParamTemplateParamNames).zip(magicParamPatternId)
          .map({
            case ((paramName, templateParamName), patternId) => {
              Parameter1(None, patternId, CaptureP1(paramName, false, Some(TypeOfP1(TypeName1(templateParamName)))))
            }
          })
          .toList;
    val seqNum5 = seqNum3 + magicNum0;

    val templateParamNames = explicitTemplateParams0 ++ exportedTemplateParamNames.map(name => TemplateParameter1(name, ReferenceTemplataType1)) ++ magicParamTemplateParamNames.map(name => TemplateParameter1(name, ReferenceTemplataType1));

    // Every lambda has a closure as its first arg, even if its empty
    val closureStructName = "__Closure:" + funcName
    val closureParam1 = Parameter1(None, seqNum5, CaptureP1("__Closure", false, Some(TypeOfP1(TypeName1(closureStructName)))))
    val seqNum6 = seqNum5 + 1

    val totalParams = closureParam1 :: explicitParams1 ++ magicParams;

    (seqNum6, Function1(funcName, false, false, closuredNames, templateParamNames, totalParams, ret, Some(body1)))
  }
  
  private def namifyElements(tlfName: String, seqNum0: Int, stackFrame0: StackFrame, globalNames: Set[String], exprs: List[Expression0], magicNum0: Int):
      (Int, List[Expression1], Set[String], Int) = {
    exprs match {
      case Nil => (seqNum0, Nil, Set(), magicNum0)
      case first0 :: rest0 => {
        val (seqNum1, namesFromInsideFirst, first1, closuredVarsFromFirst, magicNum1) =
            namifyExpression(tlfName, seqNum0, stackFrame0, globalNames, first0, magicNum0);
        val stackFrame1 = stackFrame0 ++ namesFromInsideFirst;
        val (seqNum2, rest1, closuredVarsFromRest, magicNum2) =
          namifyElements(tlfName, seqNum1, stackFrame1, globalNames, rest0, magicNum1);
        (seqNum2, first1 :: rest1, closuredVarsFromFirst ++ closuredVarsFromRest, magicNum2)
      }
    }
  }
  
  private def findLargestMember(names: Set[String], name: String): (Option[Expression1], Int) = {
    if (name.length == 0) {
      return (None, 0);
    }
    if (names.contains(name)) {
      return (Some(Lookup1(name)), name.length);
    }
    
    // verdict:
    // x - 3 should be parsed as x minus 3
    // x- 3  should be parsed as x minus 3
    // x -3  should be parsed as x(-3)
    // x-3   should be parsed as x minus 3, unless "x-3" is a valid name in scope.
    // The x-3 case is tricky. It'll find the x and then just see the -3 as a -3,
    // accidentally parsing it as x(-3) which we don't want.
    // We could just parse it as a -3 and re-split it above (unless it's the first one)
    // but instead lets just parse it the - separately, and if we see a - then a number
    // at the front of the list above, then we'll combine em.
    
    // Note the lack of - in this regex
//    val integer = raw"^\d+".r;
//    integer.findFirstIn(name) match {
//      case Some(intStr) => {
//        (Some(IntLiteral1(intStr.toInt)), intStr.length)
//      }
//      case _ => {
//        val number = raw"^\d+(?:\.\d+)?".r;
//        number.findFirstIn(name) match {
//          case Some(numStr) => {
//            (Some(FloatLiteral1(numStr.toFloat)), numStr.length)
//          }
//          case _ => {
            findLargestMember(names, name.substring(0, name.length - 1))
//          }
//        }
//      }
//    }
  }
  
  private def splitNameInner(names: Set[String], name: String): List[Expression1] = {
    if (name.length == 0) {
      List()
    } else {
      findLargestMember(names, name) match {
        case (None, _) => {
          println(names)
          throw new RuntimeException("Couldn't find '" + name + "'");
        }
        case (Some(largestMember), length) => {
          List(largestMember) ++ splitNameInner(names, name.substring(length))
        }
      }
    }
  }
  
  private def splitName(names: Set[String], name: String): List[Expression1] = {
    val numbered =
        splitNameInner(names, name) match {
          case Lookup1("-") :: IntLiteral1(num) :: rest => IntLiteral1(-num) :: rest
          case split => split
        };
    val unmuted =
        numbered
            .map(_ match {
              case Lookup1("mut") => List(Lookup1("&"), Lookup1("$"))
              case other => List(other)
            })
            .reduce(_ ++ _)
     unmuted
  }
  
  private def flattenPack(elements: List[Expression1]): List[Expression1] = {
    elements.map((expr: Expression1) => {
      expr match {
        case PackE1(elementElements) => flattenPack(elementElements)
        case _ => List(expr)
      }
    }).foldLeft(List[Expression1]())(_ ++ _)
  }

  private def flattenScramble(elements: List[Expression1]): List[Expression1] = {
    elements.map((expr: Expression1) => {
      expr match {
        case Scramble1(elementElements) => flattenScramble(elementElements)
        case _ => List(expr)
      }
    }).foldLeft(List[Expression1]())(_ ++ _)
  }

  private def namifyBlock(tlfName: String, seqNum0: Int, stackFrame: StackFrame, globalNames: Set[String], block0: Block0, magicNum0: Int):
      (Int, Set[String], Block1, Set[String], Int) = {
    val (seqNum1, elements1, closuredNames, magicNum1) =
      namifyElements(tlfName, seqNum0, stackFrame, globalNames, block0.elements, magicNum0)
    (seqNum1, Set[String](), Block1(elements1), closuredNames, magicNum1)
  }

  private def namifyLambdaExpression(tlfName: String, seqNum0: Int, stackFrame: StackFrame, globalNames: Set[String], magicNum0: Int, lam: Lambda0):
  (Int, Set[String], Function1, Set[String], Int) = {
    val (seqNum1, func1) = namifyLambda(tlfName, seqNum0, Some(stackFrame), globalNames, lam.function)
    val closuredNames =
      func1.closuredNames
          .map(name => {
            val isClosure =
              stackFrame.getJumps(name) match {
                case None => false
                case Some(0) => false
                case Some(_) => true
              };
            if (isClosure) Set(name) else Set()
          })
          .foldLeft(Set[String]())(_ ++ _);

    (seqNum1, Set[String](), func1, closuredNames, magicNum0)
  }

  // Returns the new seq num, any exported names, the new expression, and the number of _ found
  private def namifyExpression(tlfName: String, seqNum0: Int, stackFrame: StackFrame, globalNames: Set[String], expr: Expression0, magicNum0: Int):
      (Int, Set[String], Expression1, Set[String], Int) = {
    expr match {
      case lam @ Lambda0(_) => {
        namifyLambdaExpression(tlfName, seqNum0, stackFrame, globalNames, magicNum0, lam)
      }
      case b @ Block0(_) => {
        namifyBlock(tlfName, seqNum0, stackFrame, globalNames, b, magicNum0)
      }
      case RepeaterBlock0(inner0) => {
        // Things declared inside a repeater dont come outside
        val (seqNum1, _, inner1, closuredNames, magicNum1) = namifyExpression(tlfName, seqNum0, stackFrame, globalNames, inner0, magicNum0)
        (seqNum1, Set[String](), RepeaterBlock1(inner1), closuredNames, magicNum1)
      }
      case RepeaterBlockIterator0(inner0) => {
        val (seqNum1, _, inner1, closuredNames, magicNum1) = namifyExpression(tlfName, seqNum0, stackFrame, globalNames, inner0, magicNum0)
        (seqNum1, Set[String](), RepeaterBlockIterator1(inner1), closuredNames, magicNum1)
      }
      case Pack0(elements) => {
        val (seqNum1, elements1, closuredNames, magicNum1) = namifyElements(tlfName, seqNum0, stackFrame, globalNames, elements, magicNum0)
        (seqNum1, Set[String](), PackE1(flattenPack(elements1)), closuredNames, magicNum1)
      }
      case RepeaterPack0(inner0) => {
        val (seqNum1, _, inner1, closuredNames, magicNum1) = namifyExpression(tlfName, seqNum0, stackFrame, globalNames, inner0, magicNum0)
        (seqNum1, Set[String](), RepeaterPack1(inner1), closuredNames, magicNum1)
      }
      case RepeaterPackIterator0(inner0) => {
        val (seqNum1, _, inner1, closuredNames, magicNum1) = namifyExpression(tlfName, seqNum0, stackFrame, globalNames, inner0, magicNum0)
        (seqNum1, Set[String](), RepeaterPackIterator1(inner1), closuredNames, magicNum1)
      }
      case IntLiteral1(value) => (seqNum0, Set(), IntLiteral1(value), Set(), magicNum0)
      case BoolLiteral1(value) => (seqNum0, Set(), BoolLiteral1(value), Set(), magicNum0)
      case StrLiteral1(value) => (seqNum0, Set(), StrLiteral1(value), Set(), magicNum0)
      case FloatLiteral1(value) => (seqNum0, Set[String](), FloatLiteral1(value), Set(), magicNum0)
      case Lookup0(magickedName) => {
        val (magicNum2, demagickedName) =
          magickedName match {
            case "_" => (magicNum0 + 1, "__P" + magicNum0)
            case _ => (magicNum0, magickedName)
          };

          val isClosure =
              stackFrame.getJumps(demagickedName) match {
                case None => false
                case Some(0) => false
                case Some(_) => true
              };
          val closuredNames = if (isClosure) Set(demagickedName) else Set[String]()

          (seqNum0, Set(), Lookup1(demagickedName), closuredNames, magicNum2)
      }
      case TemplateSpecifiedLookup0(templateName, templateArgs) => {
        (seqNum0, Set(), TemplateSpecifiedLookup1(templateName, templateArgs), Set(), magicNum0)
      }
      case Scramble0(elements) => {
        val (seqNum1, elements1, closuredNames, magicNum1) = namifyElements(tlfName, seqNum0, stackFrame, globalNames, elements, magicNum0)
        (seqNum1, Set[String](), Scramble1(flattenScramble(elements1)), closuredNames, magicNum1)
      }
      case Sequence0(elements0) => {
        val (seqNum1, elements1, closuredNames, magicNum1) = namifyElements(tlfName, seqNum0, stackFrame, globalNames, elements0, magicNum0)
        (seqNum1, Set[String](), scout.SequenceE1(elements1), closuredNames, magicNum1)
      }
      case If0(condition, thenBody, elseBody) => {
        // Ignoring exported names
        val (seqNum1, _, condFunc1, closuredNamesFromCondition, magicNum1) =
          namifyLambdaExpression(tlfName, seqNum0, stackFrame, globalNames, magicNum0, condition)
        // Ignoring exported names
        val (seqNum2, _, thenFunc1, closuredNamesFromThen, magicNum2) =
          namifyLambdaExpression(tlfName, seqNum1, stackFrame, globalNames, magicNum1, thenBody)
        // Ignoring exported names
        val (seqNum3, _, elseFunc1, closuredNamesFromElse, magicNum3) =
          namifyLambdaExpression(tlfName, seqNum2, stackFrame, globalNames, magicNum2, elseBody)
        val closuredNames = closuredNamesFromCondition ++ closuredNamesFromThen ++ closuredNamesFromElse;
        (seqNum3, Set[String](), If1(condFunc1, thenFunc1, elseFunc1), closuredNames, magicNum3)
      }
      case Let0(pattern1, expr0) => {
        val (seqNum1, exportedNames1, expr1, closuredNames, magicNum1) =
          namifyExpression(tlfName, seqNum0, stackFrame, globalNames, expr0, magicNum0);
        val namesFromPattern = PatternTemplar.getPatternCaptureNames(pattern1);
        val patternId = seqNum1
        val seqNum2 = seqNum1 + 1
        (seqNum2, exportedNames1 ++ namesFromPattern, Let1(patternId, pattern1, expr1), closuredNames, magicNum1)
      }
      case Mutate0(destinationExpr0, sourceExpr0) => {
        val (seqNum1, exportedNames1, sourceExpr1, closuredNames1, magicNum1) =
          namifyExpression(tlfName, seqNum0, stackFrame, globalNames, sourceExpr0, magicNum0);
        val (seqNum2, exportedNames2, destinationExpr1, closuredNames2, magicNum2) =
          namifyExpression(tlfName, seqNum1, stackFrame, globalNames, destinationExpr0, magicNum1);
        (seqNum2, exportedNames1 ++ exportedNames2, Mutate1(destinationExpr1, sourceExpr1), closuredNames1 ++ closuredNames2, magicNum1)
      }
      case Dot0(expr0, name) => {
        val (seqNum1, exportedNames1, expr1, closuredNames, magicNum1) =
          namifyExpression(tlfName, seqNum0, stackFrame, globalNames, expr0, magicNum0);
        (seqNum1, exportedNames1, Dot1(expr1, name), closuredNames, magicNum1)
      }
      case DotCall0(containerExpr0, indexExpr0) => {
        val (seqNum1, exportedNames1, containerExpr1, closuredNames1, magicNum1) =
          namifyExpression(tlfName, seqNum0, stackFrame, globalNames, containerExpr0, magicNum0);
        val (seqNum2, exportedNames2, indexExpr1, closuredNames2, magicNum2) =
          namifyExpression(tlfName, seqNum1, stackFrame, globalNames, indexExpr0, magicNum1);
        (seqNum2, exportedNames1 ++ exportedNames2, DotCall1(containerExpr1, indexExpr1), closuredNames1 ++ closuredNames2, magicNum2)
      }
    }
  }
}
