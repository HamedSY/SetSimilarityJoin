import scala.collection.mutable.ListBuffer

class PPJoinHandler(threshold: Double) extends SSJAlgorithmHandler(threshold) {
  def isPositionFiltered(
      rPrefix: Array[Int],
      sPrefix: Array[Int],
      rSize: Int,
      sSize: Int,
      requiredOverlap: Int
  ): Boolean = {
    val sTokenPositions: Map[Int, Int] = sPrefix.zipWithIndex.toMap
    for ((rToken, rTokenPos) <- rPrefix.zipWithIndex) {
      sTokenPositions.get(rToken) match {
        case Some(sTokenPos) =>
          val rRemaining = rSize - rTokenPos
          val sRemaining = sSize - sTokenPos
          val maxPossibleOverlap = 1 + math.min(rRemaining - 1, sRemaining - 1)
          maxPossibleOverlap < requiredOverlap
        case None => () // do nothing
      }
    }
    false
  }
}
