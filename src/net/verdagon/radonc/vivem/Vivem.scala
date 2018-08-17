package net.verdagon.radonc.vivem
import net.verdagon.radonc.hammer._
import net.verdagon.radonc.templar._
import net.verdagon.radonc.vivem.ExpressionVivem.{Continue, Return}

case class Output(stdout: String, value: ReferendV)

object Vivem {
  def executeWithPrimitiveArgs(
      program3: Program3,
      externalArgumentReferends: Vector[PrimitiveReferendV]): Output = {
    val heap = new Heap()
    val argReferences =
      externalArgumentReferends.map(argReferend => {
        heap.add(Share, argReferend);
      });
    innerExecute(program3, argReferences, heap)
  }

  def executeWithHeap(
      program3: Program3,
      inputHeap: Heap,
      inputArgumentReferences: Vector[ReferenceV]):
  Output = {
    assert(inputHeap.countUnreachableAllocations(inputArgumentReferences) == 0)
    innerExecute(program3, inputArgumentReferences, inputHeap)
  }

  def innerExecute(
      program3: Program3,
      argumentReferences: Vector[ReferenceV],
      heap: Heap): Output = {
    val main = program3.main

    print("Making stack frame")

    val stdoutput = new StringBuilder()
    val stdout = ((str: String) => {
      print(str)
      stdoutput.append(str)
    }): (String => Unit)

    val memory = new StackFrame(heap, argumentReferences, stdout)
    println()

    val returnRef = executeFunction(program3, memory, main, 0)

    print("Ending program")
    val resultReferend = heap.dereference(returnRef.reference)
    heap.decrementReferenceRefCount(returnRef.reference)
    println()
    if (!heap.isEmpty) {
      heap.printAll();
      throw new RuntimeException("Memory leaks! See above for heap.")
    }
    println()
    Output(stdoutput.mkString, resultReferend)
  }

  def doomRegisters(function3: Function3) = {
    var lastUsingRegisterIdsByUsedRegisterId = Map[String, String]()
    function3.nodes.foreach(node3 => {
      // We :: in the node3.registerId because every register accesses itself
      // and so can be the last user of itself
      (node3.registerId :: node3.accessedRegisterIds).foreach(accessedRegisterId => {
        lastUsingRegisterIdsByUsedRegisterId =
            lastUsingRegisterIdsByUsedRegisterId.updated(accessedRegisterId, node3.registerId)
      })
    })
    val dyingRegisterIdsByLastUsingRegisterId =
      lastUsingRegisterIdsByUsedRegisterId.toList.groupBy(_._2).mapValues(_.map(_._1))
    dyingRegisterIdsByLastUsingRegisterId
  }

  def executeFunction(
      program3: Program3,
      memory: StackFrame,
      function3: Function3,
      callDepth: Int
  ): ReturnV = {
    val dyingRegisterIdsByLastUsingRegisterId = doomRegisters(function3)
    val registersById = function3.nodes.groupBy(_.registerId).mapValues(_.head)
    assert(registersById.size == function3.nodes.size)

//    println("About to execute:")
//    function3.nodes.foreach(println)
//    println("/Function")
    var nextLine = 0
    var maybeReturnReference: Option[ReturnV] = None

    while (maybeReturnReference.isEmpty && nextLine < function3.nodes.size) {
      val node = function3.nodes(nextLine)
      print("  " * callDepth + node)
      nextLine = nextLine + 1

      val registerId = node.registerId
      val nodeExecutionResult =
        ExpressionVivem.executeNode(program3, memory, callDepth, node)
      nodeExecutionResult match {
        case Continue() =>
        case Return(ref) => maybeReturnReference = Some(ref)
      }

      dyingRegisterIdsByLastUsingRegisterId.get(registerId) match {
        case None =>
        case Some(dyingRegisterIds) => {
          println()
          print("  " * callDepth)
          dyingRegisterIds.foreach(dyingRegisterId => {
            print("~r" + dyingRegisterId)
            registersById(dyingRegisterId) match {
              case _ : NonProducingNode3 =>
              case refNode : ReferenceNode3 => {
                memory.clearReferenceRegister(dyingRegisterId, refNode.resultRegister.reference)
              }
              case addrNode : AddressNode3 => {
                memory.clearAddressRegister(dyingRegisterId, addrNode.resultRegister.reference)
              }
            }
            print(" ")
          })
        }
      };

      println()
    }

    print("  " * callDepth + "Returning")
    memory.dispose()

    println()

    val Some(returnReference) = maybeReturnReference
    returnReference
  }

  def getExternFunction(ref: FunctionRef3): (AdapterForExterns, Vector[ReferenceV]) => ReturnV = {
    ref.prototype match {
      case Prototype3(_, "__addIntInt", _, List(Reference3(Share,Int3()), Reference3(Share,Int3())), Reference3(Share,Int3())) =>
        VivemExterns.addIntInt
      case Prototype3(_,"__printInt",_,List(Reference3(Share,Int3())),Reference3(Share,Void3())) =>
        VivemExterns.printInt
    }
  }
}
