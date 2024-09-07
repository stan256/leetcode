

object DynamicProgramming extends App {
  // 746. Min Cost Climbing Stairs
  def minCostClimbingStairs(cost: Array[Int]): Int = {

    val memo = collection.mutable.HashMap.empty[Int, Int]

    def dp(i: Int): Int =
      i match {
        case 0 | 1 => 0
        case _ if memo.contains(i) => memo(i)
        case _ =>
          memo.put(i, Math.min(dp(i - 1) + cost(i - 1), dp(i - 2) + cost(i - 2)))
          memo(i)
      }

    dp(cost.length)
  }

  // 1395. Count Number of Teams
  def numTeams(ratings: Array[Int]): Int = {
    var teams = 0
    val n = ratings.length
    val increasingCache = Array.ofDim[Array[Int]](n)
    val decreasingCache = Array.ofDim[Array[Int]](n)
    for (i <- 0 until n) {
      increasingCache(i) = Array.ofDim[Int](4)
      decreasingCache(i) = Array.ofDim[Int](4)
    }

    def countIncreasingTeams(position: Int, teamSize: Int): Int = {
      if (position == n) return 0
      if (teamSize == 3) return 1

      if (increasingCache(position)(teamSize) != 0) return increasingCache(position)(teamSize)

      var validTeams = 0
      for (j <- position + 1 until n) {
        if (ratings(j) > ratings(position)) validTeams += countIncreasingTeams(j, teamSize + 1)
      }
      increasingCache(position)(teamSize) = validTeams
      validTeams
    }

    def countDecreasingTeams(position: Int, teamSize: Int): Int = {
      if (position == n) return 0
      if (teamSize == 3) return 1

      if (decreasingCache(position)(teamSize) != 0) return decreasingCache(position)(teamSize)

      var validTeams = 0
      for (j <- position + 1 until n) {
        if (ratings(j) < ratings(position)) validTeams += countDecreasingTeams(j, teamSize + 1)
      }
      decreasingCache(position)(teamSize) = validTeams
      validTeams
    }

    for (i <- 0 until n) {
      teams += countIncreasingTeams(i, 1) + countDecreasingTeams(i, 1)
    }

    teams
  }

  // Top down fibonacci
  def fibonacci(n: Int): Int = {
    var map = Map.empty[Int, Int]

    def dp(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        if (map.contains(n)) map(n)
        else {
          val i = dp(n - 1) + dp(n - 2)
          map += (n -> i)
          i
        }
    }

    dp(n)
  }

  // Down to top fibonacci
  def fibonacci_2(n: Int): Int = {
    val arr = Array.ofDim[Int](n + 1)
    arr(1) = 1

    for (i <- 2 to n) {
      arr(i) = arr(i - 1) + arr(i - 2)
    }

    arr(n)
  }

  // 746. Min Cost Climbing Stairs
  def minCostClimbingStairs_bottomTop(cost: Array[Int]): Int = {
    val arr = Array.ofDim[Int](cost.length + 1)

    for (i <- 2 to cost.length) {
      arr(i) = Math.min(arr(i - 1) + cost(i - 1), arr(i - 2) + cost(i - 2))
    }

    arr(cost.length)
  }

  // 198. House Robber
  def rob(nums: Array[Int]): Int = {
    val map = collection.mutable.Map.empty[Int, Int]

    def dp(n: Int): Int = {
      if (n == 0) return nums(0)
      if (n == 1) return Math.max(nums(0), nums(1))
      map.getOrElseUpdate(n, Math.max(dp(n - 2) + nums(n), dp(n - 1)))
    }

    dp(nums.length - 1)
  }

  // 198. House Robber Bottom top
  def rob_bottomTop(nums: Array[Int]): Int = {
    if (nums.length == 1) return nums.last

    val dp = Array.ofDim[Int](nums.length)
    dp(0) = nums(0)
    dp(1) = Math.max(nums(0), nums(1))
    for (i <- 2 until nums.length) {
      dp(i) = Math.max(dp(i - 2) + nums(i), dp(i - 1))
    }

    dp.last
  }

  // 198. House Robber - bottom top - optimized space
  def rob_optimized_space(nums: Array[Int]): Int = {
    if (nums.length == 1) return nums.last

    var prevOne = Math.max(nums(0), nums(1))
    var prevTwo = nums(0)

    for (i <- 2 until nums.length) {
      val temp = prevOne
      prevOne = Math.max(prevOne, prevTwo + nums(i))
      prevTwo = temp
    }

    prevOne
  }

  // 300. Longest Increasing Subsequence
  def lengthOfLIS(nums: Array[Int]): Int = {
    val dp = Array.fill[Int](nums.length) {
      1
    }

    for (i <- nums.indices) {
      for (j <- 0 until i) {
        if (nums(i) > nums(j)) {
          dp(i) = Math.max(dp(j) + 1, dp(i))
        }
      }
    }

    dp.max
  }

  // 2140. Solving Questions With Brainpower
  def mostPoints(questions: Array[Array[Int]]): Long = {
    val dp = Array.ofDim[Long](questions.length + 1)

    for (i <- questions.length - 1 to 0 by -1) {
      val j = i + questions(i)(1) + 1
      dp(i) = Math.max(
        dp(i + 1),
        dp(Math.min(j, questions.length)).toLong + questions(i)(0).toLong
      )
    }

    dp.head
  }

  // 70. Climbing Stairs
  def climbStairs(n: Int): Int = {
    if (n == 1) return 1

    var dp = Array.ofDim[Int](n)
    dp(0) = 1
    dp(1) = 2

    for (i <- 2 until n) {
      dp(i) = dp(i - 1) + dp(i - 2)
    }

    dp.last
  }

  // 70. Climbing Stairs
  def minCostClimbingStairs_constant_memory(cost: Array[Int]): Int = {
    var downOne = 0
    var downTwo = 0

    for (i <- 2 to cost.length) {
      val temp = downOne
      downOne = Math.min(downTwo + cost(i - 2) , downOne + cost(i - 1))
      downTwo = temp
    }

    downOne
  }

  // 322. Coin Change
  def coinChange(coins: Array[Int], amount: Int): Int = {
    if (amount < 1) return 0
    val memo = collection.mutable.Map.empty[Int, Int]

    def dp(amountLeft: Int): Int = {
      if (amountLeft == 0) return 0
      if (amountLeft < 0) return -1
      if (memo.contains(amountLeft))
        return memo(amountLeft)

      val res = coins
        .map(c => dp(amountLeft - c))
        .filter(_ != -1)

      val value = if (res.isEmpty) -1 else res.min + 1
      memo(amountLeft) = value
      value
    }

    dp(amount)
  }


}
