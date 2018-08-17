package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.{Immutable, Mutability, Mutable}

object PackTemplar {
  val emptyPackType: PackT2 = PackT2(List(), StructTemplar.emptyPackStructRef)
  val emptyPackReference: Reference2 = Reference2(Share, emptyPackType)

  val newPackExpression: PackE2 = PackE2(List(), Reference2(Share, PackTemplar.emptyPackType), PackTemplar.emptyPackType)

  def evaluate(env: LocalEnvironment, temputs0: Temputs, exprs2: List[ReferenceExpression2]):
  (Temputs, ReferenceExpression2) = {

    // Flatten any member packs
    val flattenedExprs2 =
      exprs2.flatMap(expr2 => {
        expr2.resultRegister.reference.referend match {
          case PackT2(innerTypes, underlyingStructRef) => {
            val underlyingStructDef = temputs0.structDefsByRef.get(underlyingStructRef).get;
            underlyingStructDef.members.map(member => {

            })
            // We'll have to store the incoming struct into a temporary variable,
            // then make a bunch of extract expressions
            throw new RuntimeException("impl me")
          }
          case other => {
            List(expr2)
          }
        }
      })

    // Simplify 1-element packs
    val (temputs2, finalExpr2) =
      flattenedExprs2 match {
        case List(onlyExpr2) => (temputs0, onlyExpr2)
        case _ => {
          val types2 =
            flattenedExprs2.map(
              expr2 => expr2.resultRegister.expectReference().reference)
          val (temputs1, packType2, mutability) = makePackType(temputs0, types2)
          val ownership = if (mutability == Mutable) Own else Share
          val expression = PackE2(flattenedExprs2, Reference2(ownership, packType2), packType2)
          (temputs1, expression)
        }
      };

    (temputs2, finalExpr2)
  }

  def makePackType(temputs0: Temputs, types2: List[Reference2]): (Temputs, PackT2, Mutability) = {
    val (temputs1, structRef, mutability) = StructTemplar.makeUnderstruct(temputs0, types2, "__Pack")

    if (types2.isEmpty)
      assert(temputs1.lookupStruct(structRef).mutability == Immutable)

    (temputs1, PackT2(types2, structRef), mutability)
  }
}
