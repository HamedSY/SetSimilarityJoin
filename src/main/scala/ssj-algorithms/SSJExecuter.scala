import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

class SSJExecuter {
  private def printResults(similarPairs: ArrayBuffer[(Array[Int], Array[Int])]) : Unit = {
    println(s"Number of similar pairs: ${similarPairs.length}")
  }

  def executeSelfJoin(filename: String, algorithmHandler: SSJAlgorithmHandler) : Unit = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename)

    // Sort the sets by frequency
    val frequencyHandler = new FrequencyHandler()
    val sortedR: Array[Array[Int]] = frequencyHandler.createSortedLists(R)

    algorithmHandler match 
      case handler: AllPairsHandler => 
        printResults(handler.findSimilarPairs(sortedR))
      case handler: PPJoinHandler => 
        printResults(handler.findSimilarPairs(sortedR))
  }

  def executeRSJoin(filename1: String, filename2: String, algorithmHandler: SSJAlgorithmHandler) : Unit = {
    // Read the dataset
    val datasetReader = new DatasetReader
    val R: Array[Array[Int]] = datasetReader.readSetsFromFile(filename1)
    val S: Array[Array[Int]] = datasetReader.readSetsFromFile(filename2)

    // Sort the sets by frequency
    val frequencyHandler = new FrequencyHandler()
    val (sortedR, sortedS) = frequencyHandler.createSortedLists(R, S)

    algorithmHandler match 
      case handler: AllPairsHandler => 
        printResults(handler.findSimilarPairs(sortedR, sortedS))
      case handler: PPJoinHandler => 
        printResults(handler.findSimilarPairs(sortedR, sortedS))
  }
}
