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

    algorithmHandler.findSimilarPairs(sortedR)
  }

  def executeSelfJoinWithFAR(
      filename: String,
      algorithmHandler: SSJAlgorithmHandler,
      chunks: Int
  ): Int = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename)

    // Fragment the dataset
    val farHandler = new FARHandler
    val fragmentedR: ArrayBuffer[Array[Array[Int]]] =
      farHandler.fragmentCollection(R, chunks)

    // Join all the pairs
    var sumSimilars = 0
    for ((r1n, r1Index) <- fragmentedR.zipWithIndex; (r2n, r2Index) <- fragmentedR.zipWithIndex) {
      // Sort the sets by frequency
      val frequencyHandler = new FrequencyHandler()
      if (r1Index >= r2Index) {
        if (r1Index == r2Index) { // Self-Join
          val sortedR: Array[Array[Int]] = frequencyHandler.createSortedLists(r1n)
          sumSimilars += algorithmHandler.findSimilarPairs(sortedR).length
        } else { // R-S Join
          val (sortedR1n, sortedR2n) = frequencyHandler.createSortedLists(r1n, r2n)
          sumSimilars += algorithmHandler.findSimilarPairs(sortedR1n, sortedR2n).length
        }
      }
    }

    sumSimilars
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

    algorithmHandler.findSimilarPairs(sortedR, sortedS)
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

      sumSimilars += algorithmHandler.findSimilarPairs(sortedRn, sortedSn).length
    }

    sumSimilars
  }
}
