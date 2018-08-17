package net.verdagon.radonc.scout

// Assumes it's given simple parameters (name and type) and returns SimpleParameter0 for them
object Simplify {
  def simplifyParams(params1: List[Parameter1]): List[SimpleParameter1] = {
    params1.map({
      case param1 @ Parameter1(virtuality, patternId, pattern) => {
        pattern match {
          case CaptureP1(name, mutable, Some(TypeOfP1(type1))) => {
            SimpleParameter1(Some(param1), name, mutable, virtuality, type1)
          }
        }
      }
    })
  }
}