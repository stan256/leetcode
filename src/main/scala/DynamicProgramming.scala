

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

}
