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
  //  println(characterReplacement("AABABBA", 1))

  // 567. Permutation in String
  def checkInclusion(s1: String, s2: String): Boolean = {

    val map = s1.foldLeft(scala.collection.mutable.HashMap.empty[Char, Int])((map, c) => {
      map.put(c, map.getOrElse(c, 0) + 1)
      map
    })

    var left, right = 0

    while (right < s2.length) {
      val charFreq = map.get(s2.charAt(right))

      if (charFreq.isEmpty) {
        while (left != right) {
          val c = s2.charAt(left)
          map.update(c, map(c) + 1)
          left += 1
        }
        left += 1
        right += 1
      } else {
        val value = charFreq.get
        if (value > 0) {
          map.put(s2.charAt(right), value - 1)
          right += 1
        } else {
          map.put(s2.charAt(left), map(s2.charAt(left)) + 1)
          left += 1
        }
      }

      if (map.values.forall(_ == 0)) {
        return true
      }
    }


    false
  }
  //  println(checkInclusion("hello", "ooolleoooleh"))
  //  println(checkInclusion("abc", "bbbca"))
  //  println(checkInclusion("abc", "bbbka"))
  //  println(checkInclusion("abc", "abc"))
  //  println(checkInclusion("abc", "acb"))
  //  println(checkInclusion("abc", "ac"))
  //  println(checkInclusion("abc", "abkab"))
  //  println(checkInclusion("abc", "abkabc"))

  // 76. Minimum Window Substring
  def minWindow(s: String, t: String): String = {
    val map = t.foldLeft(scala.collection.mutable.HashMap.empty[Char, Int])((map, c) => {
      map.put(c, map.getOrElse(c, 0) + 1)
      map
    })
    var res = ""
    var left, right = 0

    while (right < s.length || map.values.forall(_ <= 0)) {
      if (map.values.forall(_ <= 0)) {
        val maybeFreq = map.get(s.charAt(left))
        if (maybeFreq.nonEmpty) {
          map.update(s.charAt(left), maybeFreq.get + 1)
        }
        left += 1
      } else {
        val maybeFreq = map.get(s.charAt(right))
        if (maybeFreq.nonEmpty) {
          map.update(s.charAt(right), maybeFreq.get - 1)
        }

        right += 1
      }


      if (map.values.forall(_ <= 0)) {
        if (res == "" || res.length > right - left) {
          res = s.substring(left, right)
        }
      }
    }

    res
  } // OBECODEBANC
  //  println(minWindow("ab", "a"))
  //  println(minWindow("ab", "b"))
  //  println(minWindow("ba", "b"))
  //  println(minWindow("bb", "b"))
  //  println(minWindow("ABCBECODEBANC", "ABC"))
  //  println(minWindow("ADOBECODEBANC", "ABC"))
  //  println(minWindow("BAAC", "ABC"))
  //  println(minWindow("ABC", "ABC"))
  //  println(minWindow("A", "A"))
  //  println(minWindow("Acb", "cAb"))

  // 643. Maximum Average Subarray I
  def findMaxAverage(nums: Array[Int], k: Int): Double = {
    var (left, right) = (0, k - 1)
    var count: Double = nums.slice(0, k).sum
    var res = count / k

    while (right < nums.length - 1) {
      count -= nums(left)
      left += 1
      right += 1
      count += nums(right)

      if (res < count / k)
        res = count / k
    }

    res
  }
  //  println(findMaxAverage(Array(1,12,-5,-6,50,3), 4))

  // 1004. Max Consecutive Ones III
  def longestOnes(nums: Array[Int], k: Int): Int = {
    var left, right = 0
    var res = 0

    var counter = 0
    while (right < nums.length) {

      if (nums(right) == 0)
        counter += 1
      right += 1

      while (counter > k) {
        if (nums(left) == 0)
          counter -= 1
        left += 1
      }

      if (right - left > res)
        res = right - left
    }

    res
  }
  //  println(longestOnes(Array(1,1,1,0,0,0,1,1,1,1,0), 2))
  //  println(longestOnes(Array(1), 0))
  //  println(longestOnes(Array(0), 0))

  // 713. Subarray Product Less Than K
  def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int = {
    if (k <= 1)
      return 0

    var left, right = 0
    var product = 1
    var res = 0

    while (right < nums.length) {
      product *= nums(right)

      while (product >= k) {
        product /= nums(left)
        left += 1
      }

      res += right - left + 1
      right += 1
    }

    res
  }


  //  println(numSubarrayProductLessThanK(Array(1,2,3), 0))
  //  println(numSubarrayProductLessThanK(Array(100, 5, 50, 1), 100))
  //  println(numSubarrayProductLessThanK(Array(10, 5, 2, 6), 100))
}
