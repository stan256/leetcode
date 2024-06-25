

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

  println(minCostClimbingStairs(Array(10, 15, 20)))

}
