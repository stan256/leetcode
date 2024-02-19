object Graphs extends App {
  // 547. Number of Provinces
  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    var map = isConnected.indices.map(_ -> List.empty[Int]).toMap
    val seen = collection.mutable.ArraySeq.fill(isConnected.length){false}
    var answer = 0

    for (i <- isConnected.indices) {
      var list = map(i)
      for (j <- isConnected(i).indices) {
        if (isConnected(i)(j) == 1 && i != j)
          list = list :+ j
      }
      map = map + (i -> list)
    }


    def dfs(x: Int): Unit = {
      val list = map(x)
      for (l <- list) {
        if (!seen(l)) {
          seen(l) = true
          dfs(l)
        }
      }
    }


    for (x <- isConnected.indices) {
      if (!seen(x)) {
        answer += 1
        seen(x) = true
        dfs(x)
      }
    }

    answer
  }


}
