
object Backtracking extends App {

  // 46. Permutations
  def permute(nums: Array[Int]): List[List[Int]] = {
    var answer = List.empty[List[Int]]

    def backtracking(curr: List[Int]): Unit = {
      if (curr.length == nums.length) answer = answer :+ curr
      else
        for (num <- nums) {
          if (!curr.contains(num))
            backtracking(curr.appended(num))
        }
    }

    backtracking(List.empty)
    answer
  }

  // 78. Subsets
  def subsets(nums: Array[Int]): List[List[Int]] = {
    var answer = List.empty[List[Int]]

    def backtracking(curr: List[Int], i: Int): Unit = {
      if (!answer.contains(curr))
        answer = answer :+ curr

      for (x <- nums.drop(i).zipWithIndex) {
        if (!curr.contains(x._1))
          backtracking(curr.appended(x._1), i + x._2)
      }
    }

    backtracking(List.empty, 0)
    answer
  }

  // 77. Combinations
  def combine(n: Int, k: Int): List[List[Int]] = {
    val answer = collection.mutable.ListBuffer.empty[List[Int]]

    def backtracking(curr: List[Int], i: Int): Unit = {
      if (curr.length == k) {
        answer += curr
      } else {
        val need = k - curr.length
        val remain = n - i + 1
        val available = remain - need

        for (num <- (i to i + available).toList) {
          backtracking(num :: curr, num + 1)
        }
      }
    }

    backtracking(List.empty, 1)
    answer.toList
  }

  // 797. All Paths From Source to Target
  def allPathsSourceTarget(graph: Array[Array[Int]]): List[List[Int]] = {
    val answer = collection.mutable.ListBuffer.empty[List[Int]]
    val maxElement = graph.flatten.max

    def backtracking(curr: collection.mutable.ListBuffer[Int]): Unit = {
      if (curr.lastOption.contains(maxElement)) {
        answer += curr.toList
        return
      }

      val paths = graph(curr.last)
      for (path <- paths) {
        if (!curr.contains(path)) {
          val length = curr.length
          backtracking(curr += path)
          curr.remove(length)
        }
      }
    }

    backtracking(collection.mutable.ListBuffer(0))

    answer.map(_.toList).toList
  }
}
