package net.verdagon.radonc

import net.verdagon.radonc.carpenter.Carpenter
import net.verdagon.radonc.hammer.{Hammer, Hinputs, Program3}
import net.verdagon.radonc.scout.Program1
import net.verdagon.radonc.templar.CompleteProgram2
import net.verdagon.radonc.vivem._

class Compilation(code: String) {
  var scoutputCache: Option[Program1] = None
  var temputsCache: Option[CompleteProgram2] = None
  var hinputsCache: Option[Hinputs] = None
  var hamutsCache: Option[Program3] = None

  def getScoutput(): Program1 = {
    scoutputCache match {
      case Some(scoutput) => scoutput
      case None => {
        val scoutput = CommonEnv.runNamifier(code)
        assert(scoutput != None) // runNamifier returns a None if it failed
        scoutputCache = scoutput
        scoutput.get
      }
    }
  }

  def getTemputs(): CompleteProgram2 = {
    temputsCache match {
      case Some(temputs) => temputs
      case None => {
        val temputs = CommonEnv.runTemplar(getScoutput())
        temputsCache = Some(temputs)
        temputs
      }
    }
  }

  def getHinputs(): Hinputs = {
    hinputsCache match {
      case Some(hinputs) => hinputs
      case None => {
        val hinputs = Carpenter.translate(getTemputs())
        hinputsCache = Some(hinputs)
        hinputs
      }
    }
  }

  def getHamuts(): Program3 = {
    hamutsCache match {
      case Some(hamuts) => hamuts
      case None => {
        val hamuts = Hammer.translate(getHinputs())
        hamutsCache = Some(hamuts)
        hamuts
      }
    }
  }

  def evalForReferend(heap: Heap, args: Vector[ReferenceV]): ReferendV = {
    Vivem.executeWithHeap(getHamuts(), heap, args).value
  }
  def evalForReferend(args: Vector[PrimitiveReferendV]): ReferendV = {
    Vivem.executeWithPrimitiveArgs(getHamuts(), args).value
  }
  def evalForStdout(args: Vector[PrimitiveReferendV]): String = {
    Vivem.executeWithPrimitiveArgs(getHamuts(), args).stdout
  }
}