import scala.collection.mutable.ListBuffer

object MoneyChange {
  def calc(n: Int, coins: List[Int]): List[List[Int]] ={
    var changes = new ListBuffer[List[Int]]()
    var result = new ListBuffer[List[Int]]()

    def recur(coinIndex: Int, change: List[Int]): Unit = {

      if(change.sum == n){
        changes.append(change)
      }else if(coinIndex < coins.size){
        for(i <- 0 to (n-change.sum)/coins(coinIndex)){
          recur(coinIndex+1, change:::List.fill(i)(coins(coinIndex)))
        }
      }
    }

    recur(0, List[Int]())

    changes.foreach(f => result += f.toList)
    result.toList
  }
}
