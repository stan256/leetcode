
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

  // 17. Letter Combinations of a Phone Number
  def letterCombinations(digits: String): List[String] = {
    if (digits.isEmpty)
      return List()

    val keys = Map(
      "2" -> "abc".toCharArray,
      "3" -> "def".toCharArray,
      "4" -> "ghi".toCharArray,
      "5" -> "jkl".toCharArray,
      "6" -> "mno".toCharArray,
      "7" -> "pqrs".toCharArray,
      "8" -> "tuv".toCharArray,
      "9" -> "wxyz".toCharArray,
    )

    val list = scala.collection.mutable.ListBuffer[String]()

    def backtracking(s: String, result: String): Unit = {
      if (s.length == 1) keys(s).foreach(r => list += (result + r))
      else keys(s.charAt(0).toString).foreach(ch => backtracking(s.substring(1), result + ch))
    }

    backtracking(digits, "")

    list.toList
  }

  // 39. Combination Sum
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    var answer = List.empty[List[Int]]

    def backtracking(offset: Int, currentSum: Int, path: List[Int]): Unit = {
      for (i <- offset until candidates.length) {
        val v = candidates(i)
        val updatedSum = currentSum + v

        if (updatedSum < target) {
          backtracking(i, updatedSum, v :: path)
        } else if (updatedSum == target) {
          val ints = v :: path
          answer = ints :: answer
        }
      }
    }

    backtracking(0, 0, List())

    answer
  }

  // 39. Combination Sum
  def combinationSum_2(candidates: Array[Int], target: Int): List[List[Int]] = {
    import collection.mutable

    val result = mutable.HashSet.empty[List[Int]]

    def func(list: List[Int], current: Int): Unit = {
      if (current > target) return
      if (current == target) result.addOne(list.sorted)
      else candidates.foreach(c => func(c :: list, current + c))
    }

    func(List(), 0)
    result.toList
  }

  // 52. N-Queens II
  def totalNQueens(n: Int): Int = {
    var answer = 0
    val columns = collection.mutable.HashSet.empty[Int]
    val diagonals = collection.mutable.HashSet.empty[Int]
    val antiDiagonals = collection.mutable.HashSet.empty[Int]

    def backtracking(row: Int): Unit = {
      if (row == n) {
        answer += 1
      } else {
        for (col <- 0 until n) {
          if (!(columns.contains(col) || diagonals.contains(row - col) || antiDiagonals.contains(row + col))) {
            columns.add(col)
            diagonals.add(row - col)
            antiDiagonals.add(row + col)

            backtracking(row + 1)

            columns.remove(col)
            diagonals.remove(row - col)
            antiDiagonals.remove(row + col)
          }
        }
      }
    }

    backtracking(0)

    answer
  }

  // 79. Word Search
  def exist(board: Array[Array[Char]], word: String): Boolean = {

    val set = collection.mutable.HashSet.empty[(Int, Int)]

    def backtracking(i: Int, r: Int, c: Int): Boolean = {
      if (i == word.length)
        true
      else {
        val cc = word.charAt(i)
        val options = Set(
          if (c > 0) Some((r, c - 1)) else None,
          if (c < board(0).length - 1) Some((r, c + 1)) else None,
          if (r > 0) Some((r - 1, c)) else None,
          if (r < board.length - 1) Some((r + 1, c)) else None
        ).collect { case Some(x) => x }
          .diff(set)

        options.exists(rc => {
          set.add(rc);

          val bool = if (board(rc._1)(rc._2) == cc) backtracking(i + 1, rc._1, rc._2)
          else false

          set.remove(rc)
          bool
        })
      }
    }

    board.indices.exists(row => {
      board(row).indices.exists(col => {
        set.add((row, col))

        val bool = if (board(row)(col) == word.charAt(0)) backtracking(1, row, col)
        else false

        set.remove((row, col))

        bool
      })
    })
  }

  // 22. Generate Parentheses
  def generateParenthesis(n: Int): List[String] = {

    val arr = collection.mutable.ListBuffer.empty[String]

    def backtracking(str: String, open: Int, closed: Int): Unit = {
      if (str.length == n * 2) {
        arr += str
      } else {
        if (closed == open) {
          backtracking(str + "(", open + 1, closed)
        } else {
          if (open < n) backtracking(str + "(", open + 1, closed)
          if (closed < n) backtracking(str + ")", open, closed + 1)
        }
      }
    }

    backtracking("", 0, 0)
    arr.toList
  }

  // 967. Numbers With Same Consecutive Differences
  def numsSameConsecDiff(n: Int, k: Int): Array[Int] = {
    var arr = Array.empty[Int]

    def backtracking(i: Int, iteration: Int): Unit = {
      if (iteration == n) {
        arr = arr :+ i
      } else {
        for (j <- 0 to 9) {
          if (iteration == 0) {
            if ((j + k < 10 || j - k >= 0) && j != 0) {
              backtracking(j, iteration + 1)
            }
          } else {
            val diff = Math.abs(i % 10 - j)
            if (diff == k) {
              backtracking(i * 10 + j, iteration + 1)
            }
          }
        }
      }
    }

    backtracking(0, 0)

    arr
  }

  // 216. Combination Sum III
  def combinationSum3(k: Int, n: Int): List[List[Int]] = {
    var arr = Set.empty[List[Int]]

    def backtracking(set: Set[Int], sum: Int): Unit = {
      for (i <- 1 to 9) {
        if (!set.contains(i) && set.size != k) {
          if (set.size == k - 1 && sum + i == n)
            arr = arr + (set + i).toList.sorted
          else
            backtracking(set + i, sum + i)
        }
      }
    }

    backtracking(Set(), 0)

    arr.toList
  }

  // 322. Coin Change
  def coinChange_2(coins: Array[Int], amount: Int): Int = {
    val arr = Array.ofDim[Int](amount + 1)

    for (i <- 1 to amount) {
      arr(i) = coins.map(c => i - c).filter(_ >= 0).map(arr(_)).filter(_ >= 0).minOption.map(_ + 1).getOrElse(-1)
    }

    arr(amount)
  }

  // 322. Coin Change - with max value
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val arr = Array.fill[Int](amount + 1)(Int.MaxValue)
    arr(0) = 0

    for (i <- 1 to amount) {
      for (c <- coins) {
        if (i - c >= 0 && arr(i - c) != Int.MaxValue) arr(i) = Math.min(arr(i), arr(i - c) + 1)
      }
    }

    if (arr(amount) != Int.MaxValue) arr(amount) else -1
  }

  // 46. Permutations
  def permute_2(nums: Array[Int]): List[List[Int]] = {
    import collection.mutable

    val result = mutable.ListBuffer.empty[List[Int]]

    def loop(acc: List[Int], used: Set[Int]): Unit =
      if (acc.length == nums.length) result += acc
      else nums.indices.filterNot(used).foreach(i => loop(nums(i) :: acc, used + i))

    loop(List.empty[Int], Set.empty[Int])
    result.toList
  }


}
