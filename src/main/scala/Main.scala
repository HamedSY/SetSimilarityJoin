object Main extends App {
  val executer = new SSJExecuter
  // create a "PPJoinHandler"/"AllPairsHandler" instance
  val ssjAlgorithmHandler = new PPJoinHandler(threshold = 0.7)
  // address of the dataset
  executer.executeSelfJoin("data/bms-pos-5p-sample.txt", ssjAlgorithmHandler)
}
