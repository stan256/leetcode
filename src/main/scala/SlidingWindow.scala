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
//  println(maxProfit(Array(2,1,2,1,0,1,2)))
//  println(maxProfit(Array(7,1,5,3,6,4)))

  // 3. Longest Substring Without Repeating Characters
  def lengthOfLongestSubstring(s: String): Int = {
    var res = 0
    var slow = 0
    var fast = 0
    val map = scala.collection.mutable.HashMap.empty[Char, Int]

    while (fast < s.length) {
      val fastChar = s.charAt(fast)
      if (map.contains(fastChar)) {
        slow = map(fastChar) + 1
        map.clear()
        s.slice(slow, fast).zipWithIndex.foreach(c => map.put(c._1, slow + c._2))
      }
      map.put(fastChar, fast)

      if (res < map.size)
        res = map.size

      fast += 1
    }

    res
  }

//  println(lengthOfLongestSubstring("yfsrsrpzuya"))
//  println(lengthOfLongestSubstring("au"))
//  println(lengthOfLongestSubstring("dvdf"))
//  println(lengthOfLongestSubstring("jauu"))
//  println(lengthOfLongestSubstring("abcabcbb"))
//  println(lengthOfLongestSubstring("bbbbb"))
//  println(lengthOfLongestSubstring("pwwkew"))

  // 424. Longest Repeating Character Replacement
  def characterReplacement(s: String, k: Int): Int = {
    var left, right = 0
    var result = 0
    val arr = scala.collection.mutable.ArrayBuffer.fill(26)(0)
    var mostOften = 0

    while (right < s.length) {
      arr(s.charAt(right) - 'A') += 1
      val frequency = arr(s.charAt(right) - 'A')
      mostOften = Math.max(mostOften, frequency)

      while (right - left + 1 - mostOften > k) {
        arr(s.charAt(left) - 'A') -= 1
        left += 1
      }

      result = Math.max(result, right - left + 1)

      right += 1
    }

    result
  }
  println(characterReplacement("AABABBA", 1))

}
