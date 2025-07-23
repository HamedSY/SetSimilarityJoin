object Main extends App {
  val executer = new SSJExecuter
  // Create a "PPJoinHandler"/"AllPairsHandler" instance
  // Change the threshold if you want
  val ssjAlgorithmHandler = new PPJoinHandler(threshold = 0.7)
  // Address of the dataset(s)
  // Use "executeRSJoin" if you have two datasets
  executer.executeSelfJoin("data/bms-pos-5p-sample.txt", ssjAlgorithmHandler)
}
