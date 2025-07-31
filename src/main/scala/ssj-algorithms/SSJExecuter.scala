import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class SSJExecuter {
  def executeSelfJoin(
      filename: String,
      algorithmHandler: SSJAlgorithmHandler
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename)

    // Sort the sets by frequency
    val frequencyHandler = new FrequencyHandler()
    val sortedR: Array[Array[Int]] = frequencyHandler.createSortedLists(R)

    algorithmHandler match
      case handler: AllPairsHandler =>
        handler.findSimilarPairs(sortedR)
      case handler: PPJoinHandler =>
        handler.findSimilarPairs(sortedR)
  }

  def executeRSJoin(
      filename1: String,
      filename2: String,
      algorithmHandler: SSJAlgorithmHandler
  ): ArrayBuffer[(Array[Int], Array[Int])] = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename1)
    val S: Array[Array[Int]] = datasetReader.readSetsFromFile(filename2)

    // Sort the sets by frequency
    val frequencyHandler = new FrequencyHandler()
    val (sortedR, sortedS) = frequencyHandler.createSortedLists(R, S)

    algorithmHandler match
      case handler: AllPairsHandler =>
        handler.findSimilarPairs(sortedR, sortedS)
      case handler: PPJoinHandler =>
        handler.findSimilarPairs(sortedR, sortedS)
  }

  def executeRSJoinWithFAR(
      filename1: String,
      filename2: String,
      algorithmHandler: SSJAlgorithmHandler,
      rChunks: Int,
      sChunks: Int
  ): Int = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename1)
    val S: Array[Array[Int]] = datasetReader.readSetsFromFile(filename2)

    // Fragment the dataset
    val farHandler = new FARHandler
    val fragmentedR: ArrayBuffer[Array[Array[Int]]] =
      farHandler.fragmentCollection(R, rChunks)
    val fragmentedS: ArrayBuffer[Array[Array[Int]]] =
      farHandler.fragmentCollection(S, sChunks)

    // Join all the pairs
    var sumSimilars = 0
    for (Rn <- fragmentedR; Sn <- fragmentedS) {
      // Sort the sets by frequency
      val frequencyHandler = new FrequencyHandler()
      val (sortedRn, sortedSn) = frequencyHandler.createSortedLists(Rn, Sn)

      algorithmHandler match
        case handler: AllPairsHandler =>
          sumSimilars +=  handler.findSimilarPairs(sortedRn, sortedSn).length
        case handler: PPJoinHandler =>
          sumSimilars += handler.findSimilarPairs(sortedRn, sortedSn).length
    }

    sumSimilars
  }
}
