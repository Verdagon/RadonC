package net.verdagon.radonc.templar

import net.verdagon.radonc._
import net.verdagon.radonc.scout._

import scala.collection.immutable.List

object InferTemplateTemplar {
  def inferTemplatas(
      env: TerryEnvironment,
      temputs: Temputs,
      functionTemplateParams: List[TemplateParameter1],
      explicitTemplateArgTemplatas: List[CoercedTemplateArg2],
      argTemplatas: List[ITemplata],
      paramTypes1: List[Type1]):
  Option[Map[String, CoercedTemplateArg2]] = {
    if (argTemplatas.size != paramTypes1.size) {
      throw new RuntimeException("wat " + argTemplatas + "\n" + paramTypes1); // is it ever fine for these two to be different?
    }

    (argTemplatas, paramTypes1) match {
      case (Nil, Nil) => Some(Map())
      case (headArgTemplata :: tailArgTemplatas, headParamType1 :: tailParamTypes1) => {
        val maybeHeadInferred =
          inferTemplataAgainstType1(
            env,
            temputs,
            functionTemplateParams,
            explicitTemplateArgTemplatas,
            headArgTemplata,
            headParamType1)
        maybeHeadInferred match {
          case Some(headInferred) => {
            val maybeTailInferred =
              inferTemplatas(
                env,
                temputs,
                functionTemplateParams,
                explicitTemplateArgTemplatas,
                tailArgTemplatas,
                tailParamTypes1)
            maybeTailInferred match {
              case Some(tailInferred) => Some(headInferred ++ tailInferred)
              case None => None
            }
          }
          case None => None
        }
      }
    }
  }

  // Returns None if there wasn't a match
  private def inferTemplataAgainstType1(
      env: TerryEnvironment,
      temputs0: Temputs,
      functionTemplateParams: List[TemplateParameter1],
      explicitTemplateArgTemplatas: List[CoercedTemplateArg2],
      argTemplata: ITemplata,
      paramType1: Type1):
  Option[Map[String, CoercedTemplateArg2]] = {
    (argTemplata, paramType1) match {
      case (uncoercedTemplateArg2 @ ReferenceTemplata(_), tn1 @ TypeName1(name)) => {
        // So, if we had a "fn foo:(T: reference)(x: T)" and called "foo(3)" then
        // we're currently trying to match:
        //   the 3, which is the argTemplata, which is a Reference(Int2())
        // with:
        //   the "T", which is the paramType1, which is a TypeName1("T")
        // However, that 3 could be an IntTemplata, or god knows what.
        // And the T wants it to be a reference.
        // So first, we need to coerce the argument to the parameter.

        functionTemplateParams.find(_.name == name) match {
          case Some(templateParameter1) => {
            val coercedTemplateArg2 =
              TypeTemplar.coerceTemplateArg(
                temputs0,
                templateParameter1,
                uncoercedTemplateArg2)
            Some(Map(name -> coercedTemplateArg2))
          }
          case None => {
            // Evaluate the type just to be sure it exists.
            TypeTemplar.evaluateType(env.localEnv, temputs0, tn1)
            Some(Map())
          }
        }
      }
      case (
          ReferenceTemplata(r2 @ Reference2(ownership, InterfaceRef2(interfaceTemplateName, argInterfaceTemplateArgs))),
          TemplateCall1(paramTemplateName, paramTemplateArgs)) => {
        inferTemplateArgAgainstTemplateCall(env, temputs0, functionTemplateParams, explicitTemplateArgTemplatas, r2, paramTemplateName, paramTemplateArgs)
      }
      case (
          ReferenceTemplata(r2 @ Reference2(ownership, StructRef2(structTemplateName, argStructTemplateArgs))),
          TemplateCall1(paramTemplateName, paramTemplateArgs)) => {
        inferTemplateArgAgainstTemplateCall(env, temputs0, functionTemplateParams, explicitTemplateArgTemplatas, r2, paramTemplateName, paramTemplateArgs)
      }
    }
  }

  // This is if, for example, we're matching a MySome:Int to a MyOption:T.
  private def inferTemplateArgAgainstTemplateCall(
      env: TerryEnvironment,
      temputs: Temputs,
      functionTemplateParams: List[TemplateParameter1],
      explicitTemplateArgTemplatas: List[CoercedTemplateArg2],
      argReference2: Reference2,
      paramTemplateName: String,
      paramTemplateArgTypes1: List[Type1]):
  Option[Map[String, CoercedTemplateArg2]] = {
    assert(argReference2.ownership == Own || argReference2.ownership == Share); // borrows not yet implemented
    argReference2 match {
      case Reference2(argOwnership, argCitizenRef2: CitizenRef2) => {
        // Our mission is to match the given argCitizenRef argument
        // to the paramTemplateName parameter.

        // Look for all ancestors of the citizen, including self.
        // Hopefully one of them matches the param template name we're looking for.
        val ancestors =
        temputs.lookupCitizen(argCitizenRef2)
            .getAncestorCitizens(temputs, includeSelf = true);

        // This filter is just an optimization, to drastically cut down the number
        // of types we need to check against.
        val matchingAncestors = ancestors.filter(_.humanName == paramTemplateName)
        if (matchingAncestors.isEmpty) {
          return None
        }
        if (matchingAncestors.size > 1) {
          return None
        }
        // Missing here is the code that chooses between several matching ancestors.
        // TODO: write that code.
        val matchingAncestor = matchingAncestors.head;

        // From here on, we're trying to match matchingAncestor against the param.
        val matchingAncestorReference2 = Reference2(argOwnership, matchingAncestor);

        // Note, it's totally possible argReference2 == matchingAncestorReference2.
        // That would happen if we were trying to match a MyOption:Int against a
        // MyOption:T.

        // Now, let's say we're matching a MySome:Int against a MyOption:T.
        // We need to somehow infer that T = Int.
        // This call is to infer that.
        inferTemplatas(
          env,
          temputs,
          functionTemplateParams,
          explicitTemplateArgTemplatas,
          argTemplatas = matchingAncestor.templateArgs.map(_.templata),
          paramTypes1 = paramTemplateArgTypes1)
      }
    }
  }
}