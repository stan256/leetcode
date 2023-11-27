object SlidingWindow extends App {

  // 121. Best Time to Buy and Sell Stock
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length == 1)
      return 0

    var buy = 0
    var sell = 1
    var res = 0

    while (sell < prices.length) {
      val subResult = prices(sell) - prices(buy)

      if (subResult > res)
        res = subResult

      if (subResult < 0)
        buy = sell

      sell += 1
    }

    res
  }
  println(maxProfit(Array(2,1,2,1,0,1,2)))
  println(maxProfit(Array(7,1,5,3,6,4)))

}
