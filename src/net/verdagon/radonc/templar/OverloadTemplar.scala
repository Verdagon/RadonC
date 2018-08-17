package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.NTVFunction1
import net.verdagon.radonc.templar.TypeTemplar.TypeDistance
import net.verdagon.radonc.scout.Function1

import scala.collection.immutable.List

object OverloadTemplar {

  def scoutFunctionForPrototype(
      env: LocalEnvironment,
      temputs0: Temputs,
      humanName: String,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[Reference2],
      exact: Boolean):
  (Temputs, FunctionHeader2) = {
    val (temputs1, maybePotentialBanner) =
      scoutPotentialFunction(env, temputs0, humanName, alreadySpecifiedTemplateArgs, argTypes2, exact)
    maybePotentialBanner match {
      case None => throw new RuntimeException("Couldn't find a " + humanName + "(" + argTypes2.map(":" + _).mkString(", ") + ")")
      case Some(potentialBanner) => {
        stampPotentialFunctionForPrototype(env, temputs1, potentialBanner, argTypes2)
      }
    }
  }

  private def getCandidateBanners(env: LocalEnvironment, temputs0: Temputs, humanName: String, alreadySpecifiedTemplateArgs: List[ITemplata], argTypes2: List[Reference2], exact: Boolean):
  (Temputs, List[IPotentialBanner2]) = {
    val paramMatches =
      if (exact) {
        (source: Reference2, destination: Reference2) => source == destination
      } else {
        (source: Reference2, destination: Reference2) => TypeTemplar.isTypeConvertible(temputs0, source, destination)
      }
    val paramsMatch =
      (sourceParams: List[Reference2], destinationParams: List[Reference2]) => {
        sourceParams.zip(destinationParams).forall({
          case (sourceParam, destinationParam) => paramMatches(sourceParam, destinationParam)
        })
      }

    val ordinaryBanners =
      env.globalEnv.ordinaryBanners.getOrElse(humanName, List())
          .filter(ordinaryBanner => paramsMatch(argTypes2, ordinaryBanner.paramTypes))
          .map(PotentialBannerFromExisting)

    ordinaryBanners.foreach(banner => {
      assert(temputs0.exactDeclaredSignatureExists(banner.humanName, banner.banner.templateArgs, banner.paramTypes), "banner: " + banner.banner + " declared sigs: " + temputs0.declaredSignatures)
    })

    val (temputs3, templatedBanners) =
      env.globalEnv.functionTemplates.getOrElse(humanName, List())
          .foldLeft((temputs0, Set[PotentialBannerFromTemplate]()))({
            case ((temputs1, previousPotentials), function1) => {
              // TODO: Someday, put type bounds checks here. Until then, we'll have to
              // avoid them with clever overloaded function names...
              val (temputs2, maybeBannerAndTerry) =
                FunctionTemplar.evaluateTemplatedLightBannerAndTerryFromCall(
                  env, temputs1, TemplataFunctionTerry(None, function1, alreadySpecifiedTemplateArgs), argTypes2)
              maybeBannerAndTerry match {
                case Some((banner, terry)) => {
                  val potentialBanner =
                    PotentialBannerFromTemplate(humanName, banner.params, terry)
                  if (paramsMatch(argTypes2, banner.paramTypes)) {
                    (temputs2, previousPotentials + potentialBanner)
                  } else {
                    (temputs2, previousPotentials)
                  }
                }
                case None => {
                  (temputs2, previousPotentials)
                }
              }
            }
          })
    (temputs3, ordinaryBanners.toList ++ templatedBanners.toList)
  }

  // Checks to see if there's a function that *could*
  // exist that takes in these parameter types, and returns what the signature *would* look like.
  // Only considers when arguments match exactly.
  def scoutPotentialFunction(
      env: LocalEnvironment,
      temputs0: Temputs,
      humanName: String,
      alreadySpecifiedTemplateArgs: List[ITemplata],
      argTypes2: List[Reference2],
      exact: Boolean):
  (Temputs, Option[IPotentialBanner2]) = {
    val (temputs3, candidateBanners) = getCandidateBanners(env, temputs0, humanName, alreadySpecifiedTemplateArgs, argTypes2, exact);
    val bestBanner =
      if (candidateBanners.isEmpty) {
        None
      } else if (candidateBanners.size == 1) {
        Some(candidateBanners.head)
      } else {
        Some(narrowDownOverloads(temputs0, candidateBanners, argTypes2))
      }
    (temputs3, bestBanner)
  }

  private def narrowDownOverloads(
      program2: Program2,
      banners: List[IPotentialBanner2],
      argTypes: List[Reference2]) = {

    val bannersAndScores: List[(IPotentialBanner2, List[TypeDistance])] =
      banners.map(banner => {
        val scores =
          banner.paramTypes.zip(argTypes).map({
            case (paramType, argType) => {
              TypeTemplar.getTypeDistance(program2, argType, paramType) match {
                case None => throw new RuntimeException("wat")
                case Some(distance) => distance
              }
            }
          });
        (banner, scores)
      });

    val bestScore =
      bannersAndScores.map(_._2).reduce((aScore, bScore) => {
        if (aScore == bScore) {
          // Doesn't matter, just return one
          aScore
        } else {
          val aIsBetter =
            aScore.zip(bScore).forall({
              case (aScorePart, bScorePart) => aScorePart.lessThanOrEqualTo(bScorePart)
            })
          if (aIsBetter) aScore else bScore
        }
      })

    val bannersWithBestScore =
      bannersAndScores.filter({ case (banner, score) => score == bestScore })

    val bannerWithBestScore =
      if (bannersWithBestScore.isEmpty) {
        throw new RuntimeException("wat")
      } else if (bannersWithBestScore.size > 1) {
        throw new RuntimeException("Can't resolve between " + bannersWithBestScore)
      } else {
        bannersWithBestScore.head._1
      };

    bannerWithBestScore
  }

  // The "for temputs" thing is important, it means we don't care what the result is, we just
  // want to make sure it gets into the outputs.
  def stampPotentialFunctionForBanner(
      env: LocalEnvironment,
      temputs0: Temputs,
      potentialBanner: IPotentialBanner2):
  (Temputs, FunctionBanner2) = {
    potentialBanner match {
      case PotentialBannerFromExisting(banner) => {
        assert(temputs0.exactDeclaredSignatureExists(banner.humanName, banner.templateArgs, banner.paramTypes))
        // Do nothing. It already exists in the temputs, so our watch is ended.
        (temputs0, banner)
      }
      case PotentialBannerFromTemplate(humanName, params2, terry) => {
        FunctionTemplar.evaluateTemplatedLightFunctionFromCallForBanner(
          env, temputs0, terry, params2.map(_.tyype));
      }
    }
  }

  // The "for temputs" thing is important, it means we don't care what the result is, we just
  // want to make sure it gets into the outputs.
  private def stampPotentialFunctionForPrototype(
      env: LocalEnvironment,
      temputs0: Temputs,
      potentialBanner: IPotentialBanner2,
      argTypes2: List[Reference2]):
  (Temputs, FunctionHeader2) = {
    potentialBanner match {
      case PotentialBannerFromExisting(banner) => {
        temputs0.functions.find(_.header.toBanner == banner) match {
          case Some(existingFunction) => {
            // Then it was already stamped/evaluated. This is the case if it came from
            // a light lambda. We have to do this because the env.functions1ByOrdinarySignature
            // will fail down there, because lambdas aren't included there...
            (temputs0, existingFunction.header)
          }
          case None => {
            // Then the best banner came from an ordinary banner. Let's speed up its evaluating to now.
            val maybeOriginFunction = banner.originFunction;
            maybeOriginFunction match {
              case None => {
                throw new RuntimeException("?") // what do we do when we want to stamp something with no origin function?
              }
              case Some(originFunction) => {
                originFunction.body match {
                  case None => {
                    throw new RuntimeException("?") // what do we do when we want to stamp an abstract function?
//                        val (temputs1, header) =
//                          FunctionTemplar.evaluateOrdinaryLightAbstractFunctionForHeader(env, temputs0, originFunction)
//                        (temputs1, header.toPrototype)
                  }
                  case Some(block) => {
                    FunctionTemplar.evaluateOrdinaryLightFunctionForPrototype(env, temputs0, originFunction)
                  }
                }
              }
            }
          }
        }
      }
      case PotentialBannerFromTemplate(humanName, params2, terry) => {
        FunctionTemplar.evaluateTemplatedLightFunctionFromCallForPrototype(
          env, temputs0, terry, argTypes2)
      }
    }
  }
}
