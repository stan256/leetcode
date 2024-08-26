

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

  println(fibonacci_2(6))

}
