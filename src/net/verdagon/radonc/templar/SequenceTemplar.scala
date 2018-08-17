package net.verdagon.radonc.templar

import net.verdagon.radonc.scout.{Immutable, Mutability, Mutable}

object SequenceTemplar {
  def evaluate(env: LocalEnvironment, temputs0: Temputs, exprs2: List[ReferenceExpression2]):
  (Temputs, Expression2) = {

    val types2 = exprs2.map(_.resultRegister.expectReference().reference)
    if (types2.toSet.size == 1) {
      val memberType = types2.toSet.head
      // Theyre all the same type, so make it an array.
      val arraySequenceType = makeArraySequenceType(temputs0, types2.size, memberType)
      val ownership = if (arraySequenceType.array.mutability == Mutable) Own else Share
      val finalExpr = ArraySequenceE2(exprs2, Reference2(ownership, arraySequenceType), arraySequenceType)
      (temputs0, finalExpr)
    } else {
      val (temputs1, tupleType2, mutability) = makeTupleType(temputs0, types2)
      val ownership = if (mutability == Mutable) Own else Share
      val finalExpr = TupleE2(exprs2, Reference2(ownership, tupleType2), tupleType2)
      (temputs1, finalExpr)
    }
  }

  def makeArraySequenceType(temputs0: Temputs, size: Int, type2: Reference2):
  ArraySequenceT2 = {
    val tupleMutability =
      StructTemplar.getCompoundTypeMutability(temputs0, List(type2))
    ArraySequenceT2(size, ArrayT2(type2, tupleMutability))
  }

  def makeTupleType(temputs0: Temputs, types2: List[Reference2]):
  (Temputs, TupleT2, Mutability) = {
    val (temputs1, structRef, mutability) =
      StructTemplar.makeUnderstruct(temputs0, types2, "__Tup")

    if (types2.isEmpty)
      assert(temputs1.lookupStruct(structRef).mutability == Immutable)
    // Make sure it's in there
    temputs1.lookupMutability(structRef)

    (temputs1, TupleT2(types2, structRef), mutability)
  }
}
