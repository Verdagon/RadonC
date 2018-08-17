package net.verdagon.radonc.hammer

import net.verdagon.radonc.templar._

import scala.collection.immutable.HashMap

object FunctionHammer {

  def newId(nodesByLine: Vector[Node3]) = Hammer.newId(nodesByLine)

  def translateFunctions(hinputs: Hinputs, hamuts0: Hamuts, functions2: List[Function2]):
  (Hamuts, List[FunctionRef3]) = {
    functions2.foldLeft((hamuts0, List[FunctionRef3]()))({
      case ((hamuts1, previousFunctions3), function2) => {
        val (hamuts2, function3) = translateFunction(hinputs, hamuts1, function2)
        (hamuts2, function3 :: previousFunctions3)
      }
    })
  }

  private def translateFunction(hinputs: Hinputs, hamuts0: Hamuts, function2: Function2):
  (Hamuts, FunctionRef3) = {
    hamuts0.functionRefs.get(function2.header.toPrototype) match {
      case Some(functionRef3) => (hamuts0, functionRef3)
      case None => {
        val Function2(header @ FunctionHeader2(humanName, isAbstract, isExtern, _, params2, returnType2, _), body) = function2;

        val (hamuts1, prototype3) = translatePrototype(hinputs, hamuts0, header.toPrototype);
        val temporaryFunctionRef3 = FunctionRef3(prototype3);
        val hamuts2 = hamuts1.forwardDeclareFunction(header.toPrototype, temporaryFunctionRef3)

        val nodesByLine0 = Vector[Node3]();
        val localVariableStackAddressNodesByName0 = HashMap[String, Node3]();

        val paramsAndIndexes = params2.zipWithIndex
        val (hamuts5, nodesByLine3, localVariableStackAddressNodesByName3) =
            paramsAndIndexes.foldLeft((hamuts2, nodesByLine0, localVariableStackAddressNodesByName0))({
              case ((hamuts3, nodesByLine1, localVariableStackAddressNodesByName1), (param, index)) => {
                val (hamuts4, paramType3) = TypeHammer.translateReference(hinputs, hamuts3, param.tyype);
                val argNode = Argument3(newId(nodesByLine1), paramType3, index);
                val stackNode = Stackify3(newId(nodesByLine1) + 1, paramType3, argNode.registerId, param.name)
                val nodesByLine2 = nodesByLine1 ++ List(argNode, stackNode);
                (hamuts4, nodesByLine2, localVariableStackAddressNodesByName1 + (param.name -> stackNode))
              }
            });
        val (hamuts6, _, nodesByLine4, resultLine) =
          ExpressionHammer.translate(hinputs, hamuts5, localVariableStackAddressNodesByName3, nodesByLine3, body);

        TypeHammer.checkConversion(
          prototype3.returnType,
          nodesByLine4.find(_.registerId == resultLine).get.resultRegister.expectReferenceRegister().reference)
        val returnNode = Return3(newId(nodesByLine4), prototype3.returnType, resultLine)
        val nodesByLine5 = nodesByLine4 :+ returnNode

        val function3 = Function3(prototype3, isAbstract, isExtern, nodesByLine5);
        val hamuts7 = hamuts6.addFunction(header.toPrototype, function3)

        (hamuts7, temporaryFunctionRef3)
      }
    }
  }

  def translatePrototypes(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototypes2: List[Prototype2]):
  (Hamuts, List[Prototype3]) = {
    prototypes2 match {
      case Nil => (hamuts0, Nil)
      case headPrototype2 :: tailPrototypes2 => {
        val (hamuts1, headPrototype3) = translatePrototype(hinputs, hamuts0, headPrototype2)
        val (hamuts2, tailPrototypes3) = translatePrototypes(hinputs, hamuts1, tailPrototypes2)
        (hamuts2, headPrototype3 :: tailPrototypes3)
      }
    }
  }

  def translatePrototype(
      hinputs: Hinputs, hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, Prototype3) = {
    val Prototype2(humanName, _, FunctionT2(params2, returnType2)) = prototype2;
    val (hamuts1, paramsTypes3) = TypeHammer.translateReferences(hinputs, hamuts0, params2)
    val (hamuts2, returnType3) = TypeHammer.translateReference(hinputs, hamuts1, returnType2)
    val functionNumber = hinputs.functionIds(prototype2.toSignature)
    val globalName = NameTemplar.getIdentifierName(prototype2)
    val prototype3 = Prototype3(functionNumber, humanName, globalName, paramsTypes3, returnType3)
    (hamuts2, prototype3)
  }

  def translateFunctionRef(
      hinputs: Hinputs,
      hamuts0: Hamuts,
      prototype2: Prototype2):
  (Hamuts, FunctionRef3) = {
    val (hamuts1, prototype3) = translatePrototype(hinputs, hamuts0, prototype2);
    val functionRef3 = FunctionRef3(prototype3);
    (hamuts1, functionRef3)
  }
}
