import scala.collection.mutable.ListBuffer

class AllPairsHandler (threshold: Double) extends SSJAlgorithmHandler(threshold) {
  def isPositionFiltered(
      rPrefix: Array[Int],
      sPrefix: Array[Int],
      rSize: Int,
      sSize: Int,
      requiredOverlap: Int
  ): Boolean = {
    false
  }
}
