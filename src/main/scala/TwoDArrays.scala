object TwoDArrays extends App {

  // 54. Spiral Matrix
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    val lb = collection.mutable.ListBuffer.empty[Int]
    var left = 0
    var right = matrix.head.length - 1
    var top = 0
    var bottom = matrix.length - 1

    def shouldProceed: Boolean = lb.length < matrix.length * matrix.head.length

    while (shouldProceed) {
      for (i <- left to right) lb.addOne(matrix(top)(i))
      for (i <- top + 1 to bottom) lb.addOne(matrix(i)(right))
      if (shouldProceed) for (i <- right - 1 to left by -1) lb.addOne(matrix(bottom)(i))
      if (shouldProceed) for (i <- bottom - 1 until top by -1) lb.addOne(matrix(i)(left))

      top += 1
      right -= 1
      bottom -= 1
      left += 1
    }

    lb.toList
  }

  def candyCrush(board: Array[Array[Int]]): Array[Array[Int]] = {
    def find(): List[(Int, Int)] = {
      val seen = Array.fill(board.length)(Array.fill(board.head.length)(false))
      val deleteList = collection.mutable.ListBuffer.empty[(Int, Int)]
      for (row <- board.zipWithIndex) {
        for (column <- row._1.zipWithIndex) {
          val r = row._2
          val c = column._2

          def dfs(direction: (Int, Int), prevRow: Int, prevCol: Int): List[(Int, Int)] = {
            val newRow = prevRow + direction._1
            val newCol = prevCol + direction._2
            if (newRow < board.length && newCol < board.head.length && board(newRow)(newCol) == board(prevRow)(prevCol)) {
              seen(newRow)(newCol) = true
              List(newRow -> newCol) :++ dfs(direction, newRow, newCol)
            } else List()
          }

          if (!seen(r)(c)) {
            val horizontal = dfs((0, 1), r, c)
            val vertical = dfs((1, 0), r, c)
            if (horizontal.length >= 3) deleteList.addAll(horizontal)
            if (vertical.length >= 3) deleteList.addAll(vertical)
          }

        }
      }
      deleteList.toList
    }

    val tuples = find()
    println(tuples.mkString(", "))
    Array()
  }


  // 289. Game of Life
  def gameOfLife(board: Array[Array[Int]]): Unit = {
    val counters = Array.fill(board.length)(Array.fill(board.head.length)(0))
    for (i <- board.indices) {
      for (j <- board.head.indices) {
        val neighbours = Array(
          (i + 1, j),
          (i - 1, j),
          (i, j + 1),
          (i, j - 1),
          (i - 1, j - 1),
          (i - 1, j + 1),
          (i + 1, j - 1),
          (i + 1, j + 1)
        ).filter((a, b) => a >= 0 && a < board.length && b >= 0 && b < board.head.length)

        var counter = 0
        for (n <- neighbours) {
          if (board(n._1)(n._2) == 1) counter += 1
        }
        counters(i)(j) = counter
      }
    }

    for (i <- board.indices) {
      for (j <- board.head.indices) {
        if (board(i)(j) == 1) {
          val counter = counters(i)(j)
          if (counter < 2) board(i)(j) = 0
          else if (counter < 4) {}
          else if (counter > 3) {board(i)(j) = 0}
        } else {
          if (counters(i)(j) == 3) board(i)(j) = 1
        }
      }
    }
  }

}
