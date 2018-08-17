package net.verdagon.radonc.vivem

import net.verdagon.radonc.templar.Share

object VivemExterns {
  def addIntInt(memory: AdapterForExterns, args: Vector[ReferenceV]): ReturnV = {
    assert(args.size == 2)
    val aReferend = memory.dereference(args(0))
    val bReferend = memory.dereference(args(1))
    (aReferend, bReferend) match {
      case (IntV(aValue), IntV(bValue)) => {
        memory.addAllocationForReturn(Share, IntV(aValue + bValue))
      }
    }
  }

  def printInt(memory: AdapterForExterns, args: Vector[ReferenceV]): ReturnV = {
    assert(args.size == 1)
    memory.dereference(args(0)) match {
      case IntV(value) => memory.stdout(value.toString)
    }
    memory.addAllocationForReturn(Share, VoidV())
  }
}
