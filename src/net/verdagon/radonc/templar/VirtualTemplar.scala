package net.verdagon.radonc.templar

import net.verdagon.radonc.templar.TypeTemplar.TypeDistance

import scala.collection.immutable.List

object VirtualTemplar {
  // Called the spark header because it sparks this huge explosion in stamping
  def ensureFamiliesExistsAndStampRelatives(
      env: LocalEnvironment, temputs0: Temputs, sparkHeader: FunctionHeader2):
  Temputs = {
    assert(sparkHeader.params.count(_.virtuality.nonEmpty) <= 1)

    val sparkHeaderVirtualParamTypes =
      sparkHeader.params.collect({
        case Parameter2(_, Some(_), Reference2(_, tyype: CitizenRef2)) => tyype
      });

    if (sparkHeaderVirtualParamTypes.isEmpty) {
      // It's a non-virtual, so just throw it into the temputs. Non-virtual functions
      // dont need a family made for them.
      val temputs1 = temputs0.declareFunctionSignature(sparkHeader.toSignature)
      return temputs1
    }

    // We need to create some families. These are their roots' potential functions.
    val (temputs1, familiesRootsPotentialFunctions) =
      findFamilyRoots(env, temputs0, sparkHeader)

    if (sparkHeaderVirtualParamTypes.nonEmpty && familiesRootsPotentialFunctions.isEmpty) {
      throw new RuntimeException("Can't find what this overrides");
    }

    // Now we find all the function families this could be part of (or rather, the roots
    // of those function families) and stamp them.
    // Once we stamp the root, we then stamp its descendants.
    // Note, this is VERY reentrant. Any time we stamp a function we're asking for it to be
    // declared and then added. Declaring a virtual function brings it back into this function.
    val temputs4 =
      familiesRootsPotentialFunctions.foldLeft(temputs1)({
        case (temputs2, familyRootPotentialBanner) => {
          val (temputs3, familyRootBanner) =
            OverloadTemplar.stampPotentialFunctionForBanner(
              env, temputs2, familyRootPotentialBanner)
          val needsToBeCreated = !temputs3.functionFamiliesByRootBanner.contains(familyRootBanner)
          val temputs4 =
            if (needsToBeCreated) {
              temputs3.addFunctionFamily(familyRootBanner)
            } else {
              temputs3
            }
          val temputs5 = temputs4.addFunctionToFamily(familyRootBanner, sparkHeaderVirtualParamTypes, sparkHeader.toSignature)
          val temputs6 =
            if (needsToBeCreated) {
              stampDescendants(env, temputs5, familyRootBanner)
            } else {
              temputs5
            }
          temputs6
        }
      })

    temputs4
  }

  private def stampDescendants(env: LocalEnvironment, temputs0: Temputs, rootBanner: FunctionBanner2):
  Temputs = {
    val overrideParamCombinations = getAllOverrideParamCombinations(temputs0, rootBanner);
    overrideParamCombinations.foldLeft(temputs0)({
      case (temputs1, (virtualParamTypes, overrideParamTypesWithTheseVirtualParamTypes)) => {
        val (temputs2, maybePotentialBanner) =
          OverloadTemplar.scoutPotentialFunction(env, temputs1, rootBanner.humanName, List(), overrideParamTypesWithTheseVirtualParamTypes, exact = false)
        maybePotentialBanner match {
          case None => {
            // i wonder if we should loosen it, so it doesnt always look for exact matches?
            throw new RuntimeException("couldnt find match for " + rootBanner.humanName + " for params " + overrideParamTypesWithTheseVirtualParamTypes)
          }
          case Some(potentialBanner) => {
            val (temputs3, matchingBanner) = OverloadTemplar.stampPotentialFunctionForBanner(env, temputs2, potentialBanner)
            temputs3.addFunctionToFamily(rootBanner, virtualParamTypes, matchingBanner.toSignature)
          }
        }
      }
    })
  }


  def getAllOverrideParamCombinations(program: Program2, banner: FunctionBanner2):
  Map[List[CitizenRef2], List[Reference2]] = {
    val virtualParamCombinations =
      getAllVirtualParamDescendantCombinations(program, banner)
    virtualParamCombinations.map(virtualParamCombination => {
      (virtualParamCombination -> replaceVirtualParams(banner.params, virtualParamCombination))
    }).toMap
  }

  def replaceVirtualParams(oldParams: List[Parameter2], newcomerCitizens: List[CitizenRef2]):
  List[Reference2] = {
    (oldParams, newcomerCitizens) match {
      case (Nil, Nil) => Nil
      case (Parameter2(_, _, headOldParamType) :: tailOldParams, Nil) => {
        headOldParamType :: replaceVirtualParams(tailOldParams, Nil)
      }
      case (Parameter2(_, None, headOldParamType) :: tailOldParams, _) => {
        headOldParamType :: replaceVirtualParams(tailOldParams, newcomerCitizens)
      }
      case (Parameter2(name, Some(_), Reference2(ownership, _)) :: tailOldParams, headNewcomerCitizen :: tailNewcomerCitizens) => {
        Reference2(ownership, headNewcomerCitizen) :: replaceVirtualParams(tailOldParams, tailNewcomerCitizens)
      }
    }
  }

  def getAllVirtualParamDescendantCombinations(
      program: Program2,
      banner: FunctionBanner2):
  List[List[CitizenRef2]] = {
    val virtualCitizens = banner.params.filter(_.virtuality.nonEmpty)
    getAllVirtualParamDescendantCombinations(program, List(), virtualCitizens)
  }

  private def getAllVirtualParamDescendantCombinations(
      program: Program2,
      overrideParamsSoFar: List[CitizenRef2],
      remainingRootParams: List[Parameter2]):
  List[List[CitizenRef2]] = {
    remainingRootParams match {
      case Nil => List(overrideParamsSoFar)
      case Parameter2(_, None, _) :: tailRemainingRootParams => {
        getAllVirtualParamDescendantCombinations(program, overrideParamsSoFar, tailRemainingRootParams)
      }
      case Parameter2(_, Some(_), Reference2(_, headRemainingRootParamCitizenRef : CitizenRef2)) :: tailRemainingRootParams => {
        val descendantCitizens =
          program.lookupCitizen(headRemainingRootParamCitizenRef).getDescendants(program, includeSelf = true)
        descendantCitizens.toList.flatMap(descendantCitizen => {
          val newOverrideParamsSoFar = overrideParamsSoFar :+ descendantCitizen
          getAllVirtualParamDescendantCombinations(program, newOverrideParamsSoFar, tailRemainingRootParams)
        })
      }
    }
  }


  private def findNeededSignatures(env: LocalEnvironment, temputs0: Temputs, citizenRef2: StructRef2):
  List[Signature2] = {
    temputs0.functionFamiliesByRootBanner.flatMap({
      case (familyRootBanner, _) => {
        val convertedParamTypes =
          familyRootBanner.params.map({
            case Parameter2(_, Some(_), Reference2(ownership, rootCitizenType: CitizenRef2))
              if (StructTemplar.isCitizenConvertible(temputs0, citizenRef2, rootCitizenType)) => {
              Reference2(ownership, citizenRef2)
            }
            case Parameter2(_, _, paramType2) => paramType2
          });
        if (familyRootBanner.paramTypes != convertedParamTypes) {
          List(Signature2(familyRootBanner.humanName, familyRootBanner.templateArgs, convertedParamTypes))
        } else {
          List()
        }
      }
    }).toList
  }

  def findAndStampIntoFamilies(env: LocalEnvironment, temputs0: Temputs, citizenRef2: StructRef2): Temputs = {
    val neededSignatures = findNeededSignatures(env, temputs0, citizenRef2);

    val temputs4 = neededSignatures.foldLeft(temputs0)({
      case (temputs1, neededSignature) => {
        val (temputs2, maybePotentialBanner) =
          OverloadTemplar.scoutPotentialFunction(
            env, temputs1, neededSignature.humanName, List(), neededSignature.paramTypes, exact = false)
        assert(maybePotentialBanner.nonEmpty)
        val potentialBanner = maybePotentialBanner.get;
        val (temputs3, banner) = OverloadTemplar.stampPotentialFunctionForBanner(env, temputs2, potentialBanner)
        temputs3
      }
    });

    temputs4
  }

  private def findFamilyRoots(
      env: LocalEnvironment,
      temputs0: Temputs,
      sparkHeader: FunctionHeader2):
  (Temputs, Set[IPotentialBanner2]) = {
    val virtualRootParams =
      sparkHeader.params.zipWithIndex.collect({
        case (Parameter2(_, Some(_), Reference2(ownership, concreteValue)), paramIndex) => {
          val citizenRef = concreteValue.asInstanceOf[CitizenRef2];
          (paramIndex, citizenRef)
        }
      })
    findFamilyRoots(env, temputs0, sparkHeader, virtualRootParams)
  }

  private def findFamilyRoots(
      env: LocalEnvironment,
      temputs0: Temputs,
      header: FunctionHeader2,
      virtualParams: List[(Int, CitizenRef2)]):
  (Temputs, Set[IPotentialBanner2]) = {
    virtualParams match {
      case Nil => (temputs0, Set())
      case (paramIndex, citizenRef) :: tail => {
        val (temputs1, headParamNewFamiliesRootsPotentialFunctions) =
          findFamilyRoots(env, temputs0, header, paramIndex, citizenRef);
        val (temputs2, tailParamsNewFamiliesRootsPotentialFunctions) =
          findFamilyRoots(env, temputs1, header, tail)
        (temputs2, headParamNewFamiliesRootsPotentialFunctions ++ tailParamsNewFamiliesRootsPotentialFunctions)
      }
    }
  }

  private def findFamilyRoots(
      env: LocalEnvironment,
      temputs0: Temputs,
      header: FunctionHeader2,
      paramIndex: Int,
      citizenRef2: CitizenRef2):
  (Temputs, Set[IPotentialBanner2]) = {
    val ancestorInterfaces =
      temputs0.lookupCitizen(citizenRef2).getAncestorCitizens(temputs0, includeSelf = true)
    val (temputs3, rootPotentialBanners) =
      ancestorInterfaces
        .foldLeft((temputs0, Set[IPotentialBanner2]()))({
          case ((temputs1, previousRootPotentialBanners), ancestorInterface) => {
            val oldParams = header.params.map(_.tyype)
            val Reference2(ownership, _) = oldParams(paramIndex)
            val newParam = Reference2(ownership, ancestorInterface)
            val newParams = oldParams.updated(paramIndex, newParam)

            val (temputs2, maybeRootPotentialBannerUnchecked) =
              OverloadTemplar.scoutPotentialFunction(env, temputs1, header.humanName, List(), newParams, exact = true);
            val maybeRootPotentialBanner =
              maybeRootPotentialBannerUnchecked.filter(rootPotentialBannerUnchecked => {
                rootPotentialBannerUnchecked.params(paramIndex).virtuality match {
                  case Some(Virtual2()) => true
                  case Some(Override2()) => false
                  case None => false
                }
              })
            (temputs2, previousRootPotentialBanners ++ maybeRootPotentialBanner.toList)
          }
        });
    (temputs3, rootPotentialBanners)
  }

//
//  private def isFamilyRoot(
//      env: LocalEnvironment,
//      temputs0: Temputs,
//      memberHeader: FunctionHeader2,
//      paramIndex: Int,
//      potentialRootPotentialBanner: IPotentialBanner2):
//  Boolean = {
//    val maybeVirtuality = potentialRootPotentialBanner.params(paramIndex).virtuality
//    val virtuality =
//      maybeVirtuality match {
//        case None => throw new RuntimeException("wat") // shouldnt this be virtual or abstract or override or something? throw an error?
//        case Some(v) => v
//      };
//    virtuality match {
//      case Override2() => return false
//      case Abstract2() | Virtual2() => // continue
//    }
//    // We know it's an ancestor that's marked abstract/virtual.
//    true
//  }
}
