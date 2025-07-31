import scala.collection.mutable.ArrayBuffer

private def printResults(
    similarPairs: ArrayBuffer[(Array[Int], Array[Int])]
): Unit = {
  println(s"Number of similar pairs: ${similarPairs.length}")
}

object Main extends App {
  val executer = new SSJExecuter
  // Create a "PPJoinHandler"/"AllPairsHandler" instance
  // Change the threshold if you want
  val algorithmHandler = new AllPairsHandler(threshold = 0.22)
  // Address of the dataset(s)
  // Use "executeRSJoin" if you have two datasets

  // val results = executer.executeRSJoin("data/r.txt", "data/s.txt", algorithmHandler)
  // printResults(results)
  val results = executer.executeRSJoinWithFAR("data/r.txt", "data/s.txt", algorithmHandler, 4, 2)
  println(s"Number of similar pairs: ${results}")
}
