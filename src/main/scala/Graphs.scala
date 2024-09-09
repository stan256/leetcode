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

  // 200. Number of Islands
  def numIslands(grid: Array[Array[Char]]): Int = {
    val directions = List((-1, 0), (0, -1), (1, 0), (0, 1))
    val seen = Array.fill(grid.length)(Array.fill(grid(0).length)(false))
    var answer = 0

    def check(x: Int, y: Int, isNew: Boolean): Unit = {
      if (x < 0 ||
        y < 0 ||
        x >= grid.length ||
        y >= grid(0).length ||
        seen(x)(y)
      ) return
      seen(x)(y) = true
      if (grid(x)(y) == '1') {
        if (isNew) answer += 1
        for (d <- directions) check(x + d._1, y + d._2, isNew = false)
      }
    }
    for (x <- grid.indices) {
      for (y <- grid(0).indices) {
        check(x, y, isNew = true)
      }
    }
    answer
  }

  val directions = Array((1, 0), (0, 1), (-1, 0), (0, -1))
  def getAmountOfPaths(grid: Array[Array[Char]], robotLocation: Array[Int],  maxSteps: Int): Int = {
    val memo = grid.clone()

    var result = 0

    def search(step: Int, coordinate: (Int, Int)): Unit = {
      if (step > maxSteps ||
        coordinate._1 < 0 ||
        coordinate._2 < 0 ||
        coordinate._1 >= grid.length ||
        coordinate._2 >= grid(0).length ||
        memo(coordinate._1)(coordinate._2) == 'V' ||
        memo(coordinate._1)(coordinate._2) == 'X') return

      if (grid(coordinate._1)(coordinate._2) == 'C') {
        result += 1
      } else {
        memo(coordinate._1)(coordinate._2) = 'V'
        for (xy <- directions) {
          search(step + 1, (coordinate._1 + xy._1, coordinate._2 + xy._2))
        }
        memo(coordinate._1)(coordinate._2) = '0'
      }
    }


    search(0, (robotLocation(0), robotLocation(1)))

    result
  }
}
