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
  val algorithmHandler = new PPJoinHandler(threshold = 0.5)
  // Address of the dataset(s)
  // Use "executeRSJoin" if you have two datasets

  val norStartTime = System.nanoTime()
  val norResults = executer.executeRSJoin("data/r.txt", "data/s.txt", algorithmHandler)
  printResults(norResults)
  val norEndTime = System.nanoTime()
  val norduration = (norEndTime - norStartTime) / 1e6 
  println(f"Normal took $norduration%.2f ms\n")

  val farStartTime = System.nanoTime()
  val farResults = executer.executeRSJoinWithFAR("data/r.txt", "data/s.txt", algorithmHandler, 30, 30)
  println(s"(FAR)# of similar pairs: ${farResults}")
  val farEndTime = System.nanoTime()
  val farDuration = (farEndTime - farStartTime) / 1e6 
  println(f"Normal took $farDuration%.2f ms\n")
}
