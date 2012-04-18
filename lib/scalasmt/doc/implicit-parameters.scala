def maxList[T](elements: List[T])
      (implicit orderer: T => Ordered[T]): T =
  elements match {
    case List() =>
      throw new 
        IllegalArgumentException("empty list!")
    case List(x) => x
    case x :: rest =>
      val maxRest = maxList(rest)  
        // (orderer) is implicit
      if (x > maxRest) x           
        // orderer(x) is implicit
      else maxRest
  }

