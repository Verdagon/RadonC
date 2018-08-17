package net.verdagon.radonc.carpenter

import net.verdagon.radonc.templar._

import scala.collection.immutable.Map

// If family A has a super family B, that means it's completely compatible with family B's
// signature and can be merged into it. No need for two separate vtable enries.
object SuperFamilyCarpenter {
  def getSuperFamilyMap(program2: CompleteProgram2):
  Map[FunctionBanner2, FunctionBanner2] = {
    val superFamilyRootBannersByRootBanner =
      program2.functionFamiliesByRootBanner.map({
        case (_, family) => (family.rootBanner -> getSuperFamily(program2, family))
      })
    // If A has super family B which has super family C, we might get a map
    // like (A -> B, B -> C) when we really want (A -> C, B -> C).
    // So, we make a new map, from root banner to transitive super banner.
    superFamilyRootBannersByRootBanner.mapValues(
      nonTransitiveSuperFamilyRootBanner =>
        getTransitiveSuperFamilyRootBanner(
          nonTransitiveSuperFamilyRootBanner, superFamilyRootBannersByRootBanner))
  }

  private def getTransitiveSuperFamilyRootBanner(
      thisFamilyRootBanner: FunctionBanner2,
      superFamilyRootBannersByRootBanner: Map[FunctionBanner2, FunctionBanner2]): FunctionBanner2 = {
    superFamilyRootBannersByRootBanner(thisFamilyRootBanner) match {
      case superFamilyRootBanner if superFamilyRootBanner == thisFamilyRootBanner => superFamilyRootBanner
      case superFamilyRootBanner => getTransitiveSuperFamilyRootBanner(superFamilyRootBanner, superFamilyRootBannersByRootBanner)
    }
  }

  private def getSuperFamily(
      program2: CompleteProgram2,
      thisFamily: FunctionFamily): FunctionBanner2 = {
    // We make sure to iterate over *all* of these, because we want to check that if we remove it,
    // its members match perfectly with any family whose roots match.
    val supersetFamilies =
      program2.functionFamiliesByRootBanner.flatMap({
        case (_, thatFamily) => {
          if (thisFamily == thatFamily) {
            List()
          } else if (thisFamily.rootBanner.humanName != thisFamily.rootBanner.humanName) {
            List()
          } else {
            // Not comparing against self, and root banners have the same name.
            if (bannerIsConvertible(program2, thisFamily.rootBanner, thatFamily.rootBanner)) {
              // Before we return true, lets doublecheck that all the members of thisFamily are
              // also inside thatFamily
              thisFamily.memberSignaturesByVirtualRoots.foreach({
                case (virtualRoots, signature) => {
                  if (thatFamily.memberSignaturesByVirtualRoots.get(virtualRoots) != Some(signature)) {
                    println("signature:" + signature)
                    println(thatFamily.memberSignaturesByVirtualRoots.get(virtualRoots))
                    throw new RuntimeException("wat")
                  }
                }
              })
              List(thatFamily.rootBanner)
            } else {
              List()
            }
          }
        }
      })
    // Just pick one, doesn't really matter.
    val maybeSuperFamily = supersetFamilies.headOption;
    // If there is none, then this family is its own super family
    maybeSuperFamily.getOrElse(thisFamily.rootBanner)
  }

  private def bannerIsConvertible(
      program2: Program2,
      sourceBanner: FunctionBanner2,
      targetBanner: FunctionBanner2): Boolean = {
    if (sourceBanner.humanName != targetBanner.humanName) {
      return false
    }
    sourceBanner.params.zip(targetBanner.params).forall({
      // Both virtualities none
      case (Parameter2(_, None, thisPointer), Parameter2(_, None, thatPointer)) => thisPointer == thatPointer
      // Virtualities mismatch
      case (Parameter2(_, None, _), Parameter2(_, Some(_), _)) => false
      case (Parameter2(_, Some(_), _), Parameter2(_, None, _)) => false
      // Both are virtual
      case (
          Parameter2(_, Some(Virtual2()), Reference2(thisOwnership, thisCitizen : CitizenRef2)),
          Parameter2(_, Some(Virtual2()), Reference2(thatOwnership, thatCitizen : CitizenRef2))) => {
        if (thisOwnership == thatOwnership) {
          StructTemplar.isCitizenConvertible(program2, thisCitizen, thatCitizen)
        } else {
          false
        }
      }
    })
  }
}
