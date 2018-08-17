package net.verdagon.radonc.templar;

import net.verdagon.radonc.scout._
import net.verdagon.radonc.templar

import scala.collection.immutable.{List, Nil}

sealed trait IHoistee
case class InitHoistee(expr2: ReferenceExpression2) extends IHoistee
case class DestroyHoistee(expr2: ReferenceExpression2) extends IHoistee
case class InitAndDestroyHoistee(
    initExpr2: ReferenceExpression2,
    destroyExpr2: ReferenceExpression2) extends IHoistee

object ExpressionTemplar {
  // Returns a list that does not start with a callable.
  // It will either be followed by nothing, or an infix callable.
  // "!!false or !!true" will become "false or !!true" 
  private def unscramblePrefixCallsAtBeginning(
      env: LocalEnvironment,
      temputs0: Temputs,
      elements: List[Expression2]):
  (Temputs, List[Expression2]) = {
    val callabilitiesAndElements =
      Callabilities.getCallabilities(
        env, elements.map(_.resultRegister.referend))
          .zip(elements);
    callabilitiesAndElements match {
      case Nil => (temputs0, List())
      case soloTemplata :: Nil => (temputs0, elements)
      case (NotCallable(), _) :: _ => (temputs0, elements)
      case (Callable(_), callableExpr) :: (_, argsExpr) :: rest => {
        val callableReferenceExpr =
          coerceToReferenceExpression(callableExpr)
        val (temputs1, expr2) =
          CallTemplar.evaluatePrefixCall(env, temputs0, callableExpr, argsExpr);
        val reassembledElements = expr2 :: elements.tail.tail
        unscramblePrefixCallsAtBeginning(env, temputs1, reassembledElements)
      }
      case _ => throw new RuntimeException("wat " + callabilitiesAndElements)
    }
  }

  private def unscrambleInfixCalls(
      env: LocalEnvironment, temputs0: Temputs, templatas0: List[Expression2]):
  (Temputs, List[Expression2]) = {
    // templatas0 can be anything, any scramble of templatas

    val (temputs1, elements1) =
        unscramblePrefixCallsAtBeginning(env, temputs0, templatas0);
    // templatas1 has something at the beginning that is not prefix callable
    // if anything is after that, it better be an infix callable.

    val callabilities =
      Callabilities.getCallabilities(
        env, elements1.map(_.resultRegister.referend))
    val callabilitiesAndTemplatas = callabilities.zip(elements1);
    callabilitiesAndTemplatas match {
      case Nil => (temputs1, List())
      case soloThing :: Nil => (temputs1, elements1)
      case (NotCallable(), leftArgTemplata) :: (Callable(true), callableTemplata) :: _ :: _ => {
        val elements2 = elements1.tail.tail
        // elements2 is everything after the non-callable at position 0 and the infix callable at position 1.

        val (temputs2, elements3) =
            unscramblePrefixCallsAtBeginning(env, temputs1, elements2);
        // elements3 is everything after the non-callable at position 0 and the infix callable at position 1,
        // and with the guarantee that the first thing in there is a non-callable.
        // In other words, we're now clear to use it as an argument.

        val (rightArgTemplata :: elements4) = elements3;
        // elements4 doesn't contain the left arg, the callable, or the right arg; It's
        // everything after that.

        val (temputs3, expr2) =
            CallTemplar.evaluateInfixCall(
              env,
              temputs2,
              coerceToReferenceExpression(callableTemplata),
              coerceToReferenceExpression(leftArgTemplata),
              coerceToReferenceExpression(rightArgTemplata));
        // expr2 is the resulting expression of calling the callable with the left and right args.

        val elements5 = expr2 :: elements4;
        // elements5 is the reassembled whole list of templatas, after the call has happened.

        // Fun fact: the first element in templatas5 may very well be a callable. For example:
        // myList foldStartingWith "" +
        // templatas5 would be the result of (myList foldStartingWith "")
        // which is a callable, which takes in as the next argument a callable.

        // Now, repeat the process.
        unscrambleInfixCalls(env, temputs3, elements5)
      }
      case _ => throw new RuntimeException("wat " + callabilitiesAndTemplatas)
    }
  }

  private def evaluateScramble(env: LocalEnvironment, temputs0: Temputs, scramble: Scramble1):
      (Temputs, ReferenceExpression2, Map[String, IVariable2], List[IHoistee]) = {
    // If we really want to know what things are packs, for precedence reasons,
    // we can just do something like:
    // val elementsArePacks = scramble.elements.map(expr1 => expr1.isInstanceOf[Pack1]);
    // "Why not just check if they'rPackE2s?"
    //   Because we get rid of 1-element Pack2s in the expression templar.

    val (temputs1, scrambleExprs2, elementsExporteds, hoistees) =
        evaluateExpressions(env, temputs0, scramble.elements);
    val (temputs2, resultTemplatas) =
      unscrambleInfixCalls(env, temputs1, scrambleExprs2);
    if (resultTemplatas.length != 1)
      throw new RuntimeException("wat")
    (temputs2, coerceToReferenceExpression(resultTemplatas.head), elementsExporteds, hoistees)
  }

  private def evaluateExpressions(env: LocalEnvironment, temputs0: Temputs, exprs1: List[Expression1]):
  (Temputs, List[Expression2], Map[String, IVariable2], List[IHoistee]) = {
    exprs1 match {
      case Nil => (temputs0, List(), Map(), List())
      case first1 :: rest1 => {
        val (temputs1, first2, firstEnvExportedVariables, firstHoistees) =
          evaluate(env, temputs0, first1);
        val (temputs2, rest2, restEnvExportedVariables, restHoistees) =
          evaluateExpressions(
            env.addVariables(firstEnvExportedVariables), temputs1, rest1);
        (temputs2, first2 :: rest2, firstEnvExportedVariables ++ restEnvExportedVariables, firstHoistees ++ restHoistees)
      }
    }
  }

  private def evaluateList(env: LocalEnvironment, temputs0: Temputs, expr1: List[Expression1]):
      (Temputs, List[Expression2], Map[String, IVariable2], List[IHoistee]) = {
    expr1 match {
      case Nil => (temputs0, List(), Map(), List())
      case first1 :: rest1 => {
        val (temputs1, first2, firstEnvExportedVariables, firstHoistees) =
          evaluate(env, temputs0, first1);
        val (temputs2, rest2, restEnvExportedVariables, restHoistees) = evaluateList(env.addVariables(firstEnvExportedVariables), temputs1, rest1);
        (temputs2, first2 :: rest2, firstEnvExportedVariables ++ restEnvExportedVariables, firstHoistees ++ restHoistees)
      }
    }
  }

  private def evaluateAndCoerceToReferenceExpressions(
      env: LocalEnvironment,
      temputs0: Temputs,
      exprs1: List[Expression1]):
  (Temputs, List[ReferenceExpression2], Map[String, IVariable2], List[IHoistee]) = {
    exprs1 match {
      case Nil => (temputs0, List(), Map(), List())
      case first1 :: rest1 => {
        val (temputs1, first2, firstEnvExportedVariables, firstHoistees) =
          evaluateAndCoerceToReferenceExpression(
            env, temputs0, first1);
        val (temputs2, rest2, restEnvExportedVariables, restHoistees) =
          evaluateAndCoerceToReferenceExpressions(
            env.addVariables(firstEnvExportedVariables), temputs1, rest1);
        (temputs2, first2 :: rest2, firstEnvExportedVariables ++ restEnvExportedVariables, firstHoistees ++ restHoistees)
      }
    }
  }

  private def spreadHoistees(hoistees: List[IHoistee], expr2: ReferenceExpression2) = {
    // Just pretend the expr is an init expr, put it on the stack.
    val (reverseInits, destroys) = spreadHoisteesInner(InitHoistee(expr2) :: hoistees)
    // Head of inits should happen last, so let's reverse that...
    val inits = reverseInits.reverse
    // Head of destroys should happen first, so it's in the right order.
    inits ++ destroys
  }

  // Returns inits (most recent first) and destroys (most recent first)
  // That means the head of the inits should happen last.
  // Head of destroys should happen first.
  private def spreadHoisteesInner(stack: List[IHoistee]):
  (List[ReferenceExpression2], List[ReferenceExpression2]) = {
    stack match {
      case Nil => (List(), List())
      case head :: tail => {
        val (tailInitExprs, tailDestroyExprs) = spreadHoisteesInner(tail)
        head match {
          case InitHoistee(initExpr) => (initExpr :: tailInitExprs, tailDestroyExprs)
          case DestroyHoistee(destroyExpr) => (tailInitExprs, destroyExpr :: tailDestroyExprs)
          case InitAndDestroyHoistee(initExpr, destroyExpr) => (initExpr :: tailInitExprs, destroyExpr :: tailDestroyExprs)
        }
      }
    }
  }

  private def evaluateBlockStatements(env0: LocalEnvironment, temputs0: Temputs, expr1: List[Expression1]):
  (Temputs, List[Expression2], Map[String, IVariable2]) = {
    expr1 match {
      case Nil => (temputs0, List(), Map())
      case first1 :: rest1 => {
        val (temputs1, firstExpr2, firstEnvExportedVariables, firstHoistees) =
          evaluateAndCoerceToReferenceExpression(env0, temputs0, first1);
        // Now, spread its hoistees! Init hoistees before, and destroy hoistees after.
        val firstExprs2 = spreadHoistees(firstHoistees, firstExpr2)

        val env1 = env0.addVariables(firstEnvExportedVariables)

        val (temputs3, restExprs2, restEnvExportedVariables) =
          evaluateBlockStatements(env1, temputs1, rest1)

        (temputs3, firstExprs2 ++ restExprs2, firstEnvExportedVariables ++ restEnvExportedVariables)
      }
    }
  }

  def evaluateBlock(env: LocalEnvironment, temputs0: Temputs, block: Block1):
      (Temputs, Block2, Map[String, IVariable2]) = {
    val Block1(exprs1) = block;
    val (temputs1, exprs2, exporteds) = evaluateBlockStatements(env, temputs0, exprs1);
    if (exprs2.isEmpty)
      throw new RuntimeException("wat")
    (temputs1, Block2(exprs2), exporteds)
  }

  private def evaluateLookup(env: LocalEnvironment, temputs0: Temputs, name: String):
  Expression2 = {
    env.variables.get(name) match {
      case Some(LocalVariable2(reference)) => {
        LocalLookup2(name, reference)
      }
      case Some(ClosureVariable2(closuredVarsStructRef, tyype)) => {
        val ownership = if (temputs0.lookupMutability(closuredVarsStructRef) == Mutable) Borrow else Share
        AddressMemberLookup2(
          name,
          ExpressionTemplar.borrowSoftLoad(temputs0, LocalLookup2("__Closure", Reference2(ownership, closuredVarsStructRef))),
          tyype)
      }
      case None => {
        env.globalEnv.valueMembers.get(name) match {
          case Some(value) => {
            value match {
              case BooleanTemplata(value) => BoolLiteral2(value)
              case IntegerTemplata(value) => IntLiteral2(value)
              case NoneTemplata() => NoneLiteral2()
            }
          }
          case None => {
            val functionsExist =
              env.globalEnv.ordinaryBanners.contains(name) ||
              env.globalEnv.functionTemplates.contains(name);
            if (functionsExist) {
              Templar.newGlobalFunctionGroupExpression(name, List())
            } else {
              println("value members: " + env.globalEnv.valueMembers)
              throw new RuntimeException("Couldn't find anything named \"" + name + "\" in env:\n" + env);
            }
          }
        }
      }
      case _ => throw new RuntimeException("wat")
    }
  }

  private def evaluateTemplateSpecifiedLookup(
      env: LocalEnvironment,
      name: String,
      templateArgs: List[ITemplata]):
  ReferenceExpression2 = {
    env.variables.get(name) match {
      case Some(LocalVariable2(tyype)) =>
        throw new RuntimeException("Can't specify template parameters on a variable!");
      case Some(ClosureVariable2(closuredVarsStructRef, tyype)) =>
        throw new RuntimeException("Can't specify template parameters on a variable!");
      case None =>
      case _ => throw new RuntimeException("wat")
    }
    assert(env.globalEnv.functionTemplates.contains(name));
    Templar.newGlobalFunctionGroupExpression(name, templateArgs)
  }

  private def makeClosureStructConstructExpression(
      env: LocalEnvironment, temputs0: Temputs, closureStructRef: StructRef2):
  (Temputs, ReferenceExpression2) = {
    val closureStructDef = temputs0.lookupStruct(closureStructRef);
    // Note, this is where the unordered closuredNames set becomes ordered.
    val lookupExpressions2 =
      closureStructDef.members.map(_.name).map(varName => {
        ExpressionTemplar.evaluateLookup(env, temputs0, varName)
      });
    val ownership = if (closureStructDef.mutability == Mutable) Own else Share
    val resultPointerType = Reference2(ownership, closureStructRef)
    val constructExpr2 = Construct2(closureStructRef, resultPointerType, lookupExpressions2)
    (temputs0, constructExpr2)
  }

  private def evaluateAndCoerceToReferenceExpression(
      env: LocalEnvironment,
      temputs0: Temputs,
      expr1: Expression1):
  (Temputs, ReferenceExpression2, Map[String, IVariable2], List[IHoistee]) = {
    val (temputs1, expr2, exporteds, hoistees) = evaluate(env, temputs0, expr1)
    expr2 match {
      case r : ReferenceExpression2 => (temputs1, r, exporteds, hoistees)
      case a : AddressExpression2 => (temputs1, coerceToReferenceExpression(a), exporteds, hoistees)
    }
  }

  def coerceToReferenceExpression(expr2: Expression2): ReferenceExpression2 = {
    expr2 match {
      case r : ReferenceExpression2 => r
      case a : AddressExpression2 => SoftLoad2(a, a.resultRegister.reference.ownership)
    }
  }

  private def evaluateExpectedAddressExpression(
      env: LocalEnvironment,
      temputs0: Temputs,
      expr1: Expression1):
  (Temputs, AddressExpression2, Map[String, IVariable2], List[IHoistee]) = {
    val (temputs1, expr2, exporteds, hoistees) = evaluate(env, temputs0, expr1)
    expr2 match {
      case a : AddressExpression2 => (temputs1, a, exporteds, hoistees)
      case r : ReferenceExpression2 => throw new RuntimeException("Expected reference expression!")
    }
  }

  // returns:
  // - temputs
  // - resulting templata
  // - exported things (from let)
  // - hoistees; expressions to hoist (like initializing blocks)
  private def evaluate(env: LocalEnvironment, temputs0: Temputs, expr1: Expression1):
      (Temputs, Expression2, Map[String, IVariable2], List[IHoistee]) = {

    expr1 match {
      case IntLiteral1(i) => (temputs0, IntLiteral2(i), Map(), List())
      case StrLiteral1(s) => (temputs0, StrLiteral2(s), Map(), List())
      case FloatLiteral1(f) => (temputs0, FloatLiteral2(f), Map(), List())
      case scramble:Scramble1 => evaluateScramble(env, temputs0, scramble)
      case Lookup1(name) => {
        (temputs0, evaluateLookup(env, temputs0, name), Map(), List())
      }
      case TemplateSpecifiedLookup1(name, templateArgs1) => {
        val (temputs1, typeTemplataTemplateArgs) =
          TypeTemplar.evaluateTypes(env, temputs0, templateArgs1);
        (temputs1, evaluateTemplateSpecifiedLookup(env, name, typeTemplataTemplateArgs), Map(), List())
      }
      case Dot1(innerExpr1, memberName) => {
        val (temputs1, unborrowedContainerExpr2, exportedVariables, hoistees) =
          evaluate(env, temputs0, innerExpr1);

        // Auto-borrow for doing dots.
        val containerExpr2 = maybeBorrowSoftLoad(temputs1, unborrowedContainerExpr2)

        containerExpr2.resultRegister.reference.referend match {
          case structRef @ StructRef2(_, _) => {
            temputs1.lookupStruct(structRef) match {
              case structDef : StructDefinition2 => {
                val memberType = structDef.getMember(memberName).tyype.expectReferenceMember().reference;
                val dot2 = ReferenceMemberLookup2(memberName, containerExpr2, memberType)
                (temputs1, dot2, exportedVariables, hoistees)
              }
            }
          }
          case TupleT2(_, structRef) => {
            temputs1.lookupStruct(structRef) match {
              case structDef : StructDefinition2 => {
                val memberType = structDef.getMember(memberName).tyype.expectReferenceMember().reference;
                val dot2 = ReferenceMemberLookup2(memberName, containerExpr2, memberType)
                (temputs1, dot2, exportedVariables, hoistees)
              }
            }
          }
          case at @ ArraySequenceT2(_, arrayType) => {
            val exprTemplata =
              if (memberName.forall(Character.isDigit)) {
                ArrayLookup2(
                  TemplarReinterpret2(containerExpr2, containerExpr2.resultRegister.reference.copy(referend = arrayType)),
                  arrayType,
                  IntLiteral2(memberName.toInt))
              } else {
                throw new RuntimeException("Sequence has no member named " + memberName)
              }
            (temputs1, exprTemplata, exportedVariables, hoistees)
          }
          case at @ ArrayT2(_, _) => {
            val exprTemplata =
              if (memberName.forall(Character.isDigit)) {
                ArrayLookup2(
                  containerExpr2,
                  at,
                  IntLiteral2(memberName.toInt))
              } else {
                throw new RuntimeException("Sequence has no member named " + memberName)
              }
            (temputs1, exprTemplata, exportedVariables, hoistees)
          }
        }
      }
      case function1 @ Function1(_, _, _, _, _, _, _, _) => {
        evaluateLambda(env, temputs0, function1)
      }
      case PackE1(elements1) => {
        val (temputs1, elementsExprs2, exporteds, elementsHoistees) =
          evaluateAndCoerceToReferenceExpressions(env, temputs0, elements1);

        // Put them all into a pack because im not quite ready yet to have packs
        // that have templatas... we'd have to make some sort of... TemplataPack thing...
        // Note from later than that: yeah we definitely need a TemplataPack btw
        val (temputs3, resultExpr2) =
          PackTemplar.evaluate(env, temputs1, elementsExprs2)
        (temputs3, resultExpr2, Map(), elementsHoistees)
      }
      case SequenceE1(elements1) => {
        val (temputs1, exprs2, exporteds, elementsHoistees) =
          evaluateAndCoerceToReferenceExpressions(env, temputs0, elements1);

        // would we need a sequence templata? probably right?
        val (temputs3, expr2) = SequenceTemplar.evaluate(env, temputs1, exprs2)
        (temputs3, expr2, Map(), elementsHoistees)
      }
      case b @ Block1(exprs1) => {
        val (temputs1, block2, exporteds) = evaluateBlock(env, temputs0, b);
        (temputs1, block2, exporteds, List())
      }
      case Construct1(type1, argExprs1) => {
        val (temputs1, argExprs2, exporteds, argsHoistees) =
          evaluateList(env, temputs0, argExprs1);

        val (temputs3, valueType2) = TypeTemplar.evaluateType(env, temputs1, type1)
        val constructExpr2 =
          valueType2 match {
            case ReferendTemplata(structRef2 @ StructRef2(_, _)) => {
              val structDef2 = temputs3.lookupStruct(structRef2)
              val ownership = if (structDef2.mutability == Mutable) Own else Share
              val resultPointerType = Reference2(ownership, structRef2)
              Construct2(structRef2, resultPointerType, argExprs2)
            }
            case _ => throw new RuntimeException("wat")
          }
        (temputs3, constructExpr2, exporteds, argsHoistees)
      }
      case ConstructArray1(sizeExpr1, generatorExpr1, arrayMutability) => {
        val (temputs1, sizeExpr2, exportedsA, argsHoisteesA) =
          evaluate(env, temputs0, sizeExpr1);

        val (temputs3, generatorExpr2, exportedsB, argsHoisteesB) =
          evaluateAndCoerceToReferenceExpression(env, temputs1, generatorExpr1);

        // Now, pretend we're calling it with an integer.
        // That zero should be replaced by an index in later stages.
        val (temputs4, callExpr2) =
          CallTemplar.evaluatePrefixCall(
            env,
            temputs3,
            generatorExpr2,
            IntLiteral2(0))
//
//        val (temputs5, memberType2) =
//          TypeTemplar.evaluateAndReferencifyType(env, temputs4, type1, Own)

        val memberType2 = callExpr2.resultRegister.reference
        if (arrayMutability == Immutable &&
            Templar.getMutability(temputs4, memberType2.referend) == Mutable) {
          throw new RuntimeException("Can't have an immutable array of mutable elements!")
        }
        val arrayType = ArrayT2(memberType2, arrayMutability)

        val sizeRefExpr2 = coerceToReferenceExpression(sizeExpr2)
        assert(sizeRefExpr2.resultRegister.expectReference().reference == Reference2(Share, Int2()))

        val constructExpr2 =
          ConstructArray2(
            arrayType,
            sizeRefExpr2,
            callExpr2)
        (temputs4, constructExpr2, exportedsA ++ exportedsB, argsHoisteesA ++ argsHoisteesB)

//        val (temputs3, valueType2) = TypeTemplar.evaluateType(env, temputs2, type1)
//        val constructExpr2 =
//          valueType2 match {
//            case ReferendTemplata(structRef2 @ StructRef2(_, _)) => {
//              val structDef2 = temputs3.lookupStruct(structRef2)
//              val ownership = if (structDef2.mutability == Mutable) Own else Share
//              val resultPointerType = Reference2(ownership, structRef2)
//              Construct2(structRef2, resultPointerType, argExprs2)
//            }
//            case _ => throw new RuntimeException("wat")
//          }
//        (temputs3, TemplataReferenceExpression(constructExpr2), exporteds, argsHoistees)
      }
      case Mutate1(destinationExpr1, sourceExpr1) => {
        val (temputs1, sourceExpr2, sourceExporteds, sourceHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs0, sourceExpr1)
        val (temputs3, destinationExpr2, destinationExporteds, destinationHoistees) =
          evaluateExpectedAddressExpression(env, temputs1, destinationExpr1)
        val mutate2 = Mutate2(destinationExpr2, sourceExpr2);
        (temputs3, mutate2, sourceExporteds ++ destinationExporteds, sourceHoistees ++ destinationHoistees)
      }
      case Let1(patternId, pattern, sourceExpr1) => {
        val (temputs1, sourceExpr2, sourceExprExportedVariables, sourceHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs0, sourceExpr1)

        val tempName = "__patterninput_" + patternId
        val stack = Let2(tempName, false, sourceExpr2)
        val lookup = LocalLookup2(tempName, sourceExpr2.resultRegister.reference)
        val patternInputExpr = SoftLoad2(lookup, lookup.reference.ownership)
        val (temputs4, lets2) =
          PatternTemplar.nonCheckingTranslate(env, temputs1, patternId, pattern, patternInputExpr)

        val variableExports = lets2.map(let2 => (let2.name -> LocalVariable2(let2.expr.resultRegister.reference))).toMap

        val resultExprBlock2 = Block2(stack :: lets2)

        (temputs4, resultExprBlock2, variableExports, sourceHoistees)
      }
      case If1(condition1, thenBody1, elseBody1) => {
        // In all of these, the hoistees contain the lambdas used for the conditions and
        // case bodies. They'll be hoisted to the closest spot in the nearest Block2.
        // Ignoring the exporteds
        val (temputs1, conditionExpr2, _, conditionHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs0, condition1);
        val (temputs2, conditionCallExpr2) =
          CallTemplar.evaluatePrefixCall(
            env, temputs1, conditionExpr2, PackTemplar.newPackExpression)

        // Ignoring the exporteds
        val (temputs3, thenBodyExpr2, _, thenHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs2, thenBody1);
        // This will fail if it's not a callable, and it better be a callable.
        val (temputs4, thenCallExpr2) =
          CallTemplar.evaluatePrefixCall(
            env, temputs3, thenBodyExpr2, PackTemplar.newPackExpression)

        // Ignoring the exporteds
        val (temputs5, elseBodyExpr2, _, elseHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs4, elseBody1);
        // This will fail if it's not a callable, and it better be a callable.
        val (temputs6, elseCallExpr2) =
          CallTemplar.evaluatePrefixCall(
            env, temputs5, elseBodyExpr2, PackTemplar.newPackExpression)

        val ifExpr2 = If2(conditionCallExpr2, thenCallExpr2, elseCallExpr2)
        (temputs6, ifExpr2, Map(), elseHoistees ++ thenHoistees ++ conditionHoistees)
      }
      case Panic1() => {
        (temputs0, Panic2(), Map(), List())
      }
      case DotCall1(containerExpr1, indexExpr1) => {
        val (temputs1, containerExpr2, containerExprExportedVariables, containerExprHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs0, containerExpr1);

        val (temputs3, indexExpr2, indexExprExportedVariables, indexExprHoistees) =
          evaluateAndCoerceToReferenceExpression(env, temputs1, indexExpr1);

        val exprTemplata =
          containerExpr2.resultRegister.reference.referend match {
            case at @ ArrayT2(_, _) => {
              ArrayLookup2(containerExpr2, at, indexExpr2)
            }
            case ArraySequenceT2(_, arrayType) => {
              ArrayLookup2(
                TemplarReinterpret2(containerExpr2, containerExpr2.resultRegister.reference.copy(referend = arrayType)),
                arrayType,
                indexExpr2)
            }
            // later on, a map type could go here
          }
        (temputs3, exprTemplata, containerExprExportedVariables ++ indexExprExportedVariables, containerExprHoistees ++ indexExprHoistees)
      }
      case _ => {
        println(expr1)
        throw new RuntimeException(expr1.toString)
      }
    }
  }

//  def flattenAndBlockify(treeExpr2: Expression2): Block2 = {
//    val containedExprs2 = flattenAndBlockifyInner(treeExpr2)
//    Block2(containedExprs2)
//  }
//
//  def flattenAndBlockifyInner(expr2: Expression2): List[Expression2] = {
//    expr2 match {
//      case Block2(exprs2) => exprs2.flatMap(flattenAndBlockifyInner)
//      case otherExpr2 => flattenAndBlockifyInner(otherExpr2)
//    }
//  }

  // returns:
  // - temputs
  // - resulting templata
  // - exported things (from let)
  // - hoistees; expressions to hoist (like initializing blocks)
  def evaluateLambda(env: LocalEnvironment, temputs0: Temputs, function1: Function1):
  (Temputs, ReferenceExpression2, Map[String, IVariable2], List[IHoistee]) = {
//    if (function1.closuredNames.isEmpty) {
//      val (temputs1, closureStructRef2) =
//        FunctionTemplar.evaluateClosureStruct(env, temputs0, function1);
//      if (function1.templateParams.isEmpty) {
//        val (temputs2, header) =
//          FunctionTemplar.evaluateOrdinaryClosureFunctionFromNonCallForPrototype(
//            env, temputs1, closureStructRef2, function1)
//
//        val resultType = OrdinaryClosure2(closureStructRef2, header.toPrototype)
//        ExpressionTemplar.evaluateClosure()
//
//        val resultTemplata = templar.TemplataReferenceExpression(resultExpr)
//        (temputs2, resultingTemplata, Map(), List())
//      } else {
//        val resultingTemplata =
//          TemplataLightTemplateLambdaFunction(
//            closureStructRef2,
//            TemplataFunctionTerry(Some(env), function1, List()))
//        (temputs0, resultingTemplata, Map(), List())
//      }
//    } else {
      evaluateClosure(env, temputs0, function1)
//    }
  }

  // Given a function1, this will give a closure (an OrdinaryClosure2 or a TemplatedClosure2)
  // returns:
  // - temputs
  // - resulting templata
  // - exported things (from let)
  // - hoistees; expressions to hoist (like initializing blocks)
  def evaluateClosure(env: LocalEnvironment, temputs0: Temputs, function1: Function1):
  (Temputs, ReferenceExpression2, Map[String, IVariable2], List[IHoistee]) = {
    val (temputs1, closureStructRef2) =
      FunctionTemplar.evaluateClosureStruct(env, temputs0, function1);
    val (temputs3, closureType2) =
      if (function1.templateParams.isEmpty) {
        // Note, this will re-evaluate the closure struct, that's fine...
        val (temputs2, header) =
          FunctionTemplar.evaluateOrdinaryClosureFunctionForPrototype(
            env, temputs1, closureStructRef2, function1)
        (temputs2, OrdinaryClosure2(closureStructRef2, header.toPrototype))
      } else {
        // When we made the function1, we inserted a first argument, the "__Closure:(function name)"
        // now is when we put that into the environment.
        val function1Env = env.addType(closureStructRef2.humanName, ReferendTemplata(closureStructRef2))
        (temputs1, TemplatedClosure2(closureStructRef2, TemplataFunctionTerry(Some(function1Env), function1, List())))
      };
    val (temputs4, constructExpr2) =
      makeClosureStructConstructExpression(env, temputs3, closureStructRef2)
    val ownership = if (temputs4.lookupMutability(closureStructRef2) == Mutable) Borrow else Share
    val resultExpr2 = TemplarReinterpret2(constructExpr2, Reference2(ownership, closureType2))
    val letExpr2 = Let2("__var." + function1.name, false, resultExpr2)

    val lookupExpr2 = LocalLookup2("__var." + function1.name, resultExpr2.resultReference)
    val borrowExpr = ExpressionTemplar.borrowSoftLoad(temputs4, lookupExpr2)
    (temputs4, borrowExpr, Map(), List(InitHoistee(letExpr2)))
  }

  def getBorrowOwnership(temputs: Temputs, referend: Referend2): Ownership = {
    referend match {
      case Int2() => Share
      case Bool2() => Share
      case Str2() => Share
      case ft @ FunctionT2(_, _) => Raw
      case TupleT2(_, understruct2) => {
        if (temputs.lookupMutability(understruct2) == Mutable) Borrow else Share
      }
      case ArraySequenceT2(_, ArrayT2(_, mutability)) => {
        if (mutability == Mutable) Borrow else Share
      }
      case ArrayT2(_, mutability) => {
        if (mutability == Mutable) Borrow else Share
      }
      case TemplatedClosure2(structRef, _) => {
        if (temputs.lookupMutability(structRef) == Mutable) Borrow else Share
      }
      case OrdinaryClosure2(structRef, _) => {
        if (temputs.lookupMutability(structRef) == Mutable) Borrow else Share
      }
      case sr2 @ StructRef2(_, _) => {
        if (temputs.lookupMutability(sr2) == Mutable) Borrow else Share
      }
      case ir2 @ InterfaceRef2(_, _) => {
        if (temputs.lookupMutability(ir2) == Mutable) Borrow else Share
      }
    }
  }

  def borrowAlias(temputs: Temputs, expr2: ReferenceExpression2): ReferenceExpression2 = {
    Alias2(expr2, getBorrowOwnership(temputs, expr2.resultRegister.reference.referend))
  }

  def borrowSoftLoad(temputs0: Temputs, expr2: AddressExpression2): ReferenceExpression2 = {
    SoftLoad2(expr2, getBorrowOwnership(temputs0, expr2.resultRegister.reference.referend))
  }

  def maybeBorrowSoftLoad(temputs0: Temputs, expr2: Expression2): ReferenceExpression2 = {
    expr2 match {
      case e : ReferenceExpression2 => e
      case e : AddressExpression2 => borrowSoftLoad(temputs0, e)
    }
  }

  def borrowSoftLoad(temputs0: Temputs, exprs2: List[AddressExpression2]):
  List[ReferenceExpression2] = {
    exprs2.map(expr2 => {
      SoftLoad2(expr2, getBorrowOwnership(temputs0, expr2.resultRegister.reference.referend))
    })
  }

  def borrow(temputs0: Temputs, expr2: Expression2): ReferenceExpression2 = {
    expr2 match {
      case re : ReferenceExpression2 => borrowAlias(temputs0, re)
      case ae : AddressExpression2 => borrowSoftLoad(temputs0, ae)
    }
  }

  def getMoveOwnership(temputs: Temputs, referend: Referend2): Ownership = {
    referend match {
      case Int2() => Share
      case Bool2() => Share
      case Str2() => Share
      case ft @ FunctionT2(_, _) => Raw
      case sr2 @ StructRef2(_, _) => {
        if (temputs.lookupMutability(sr2) == Mutable) Own else Share
      }
      case ir2 @ InterfaceRef2(_, _) => {
        if (temputs.lookupMutability(ir2) == Mutable) Own else Share
      }
    }
  }

  def moveAlias(temputs: Temputs, expr2: ReferenceExpression2): ReferenceExpression2 = {
    Alias2(expr2, getMoveOwnership(temputs, expr2.resultRegister.reference.referend))
  }

  def moveSoftLoad(temputs0: Temputs, expr2: AddressExpression2): ReferenceExpression2 = {
    SoftLoad2(expr2, getMoveOwnership(temputs0, expr2.resultRegister.reference.referend))
  }

  def move(temputs0: Temputs, expr2: Expression2): ReferenceExpression2 = {
    expr2 match {
      case re : ReferenceExpression2 => moveAlias(temputs0, re)
      case ae : AddressExpression2 => moveSoftLoad(temputs0, ae)
    }
  }

}
