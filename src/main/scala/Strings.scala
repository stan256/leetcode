
object Strings extends App {

  // 415. Add Strings
  def addStrings(num1: String, num2: String): String = {
    var one: String = null
    var two: String = null
    if (num1.length > num2.length) {
      one = num1
      two = "0" * (num1.length - num2.length) + num2
    } else {
      one = "0" * (num2.length - num1.length) + num1
      two = num2
    }

    var res = ""
    var addOne = false
    for (i <- one.length - 1 to 0 by -1) {
      var sum = one(i).asDigit + two(i).asDigit
      if (addOne) {
        sum += 1
        addOne = false
      }
      if (sum > 9) {
        sum = sum % 10
        addOne = true
      }
      res = s"$sum$res"
    }
    if (addOne) res = "1" + res
    res
  }

  // 383. Ransom Note
  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    val map = collection.mutable.HashMap.from(magazine.groupBy(identity).map(x => x._1 -> x._2.length))
    for (c <- ransomNote) map(c) = map.getOrElse(c, 0) - 1
    map.values.forall(_ >= 0)
  }

  // 3. Longest Substring Without Repeating Characters
  def lengthOfLongestSubstring(s: String): Int = {
    var left = 0
    var right = 0
    var max = 0
    var set = collection.mutable.Set.empty[Char]

    while (right < s.length) {
      if (set.contains(s.charAt(right))) {
        set.remove(s.charAt(left))
        left += 1
      } else {
        set.addOne(s.charAt(right))
        max = max.max(right - left + 1)
        right += 1
      }
    }

    max
  }

  // 409. Longest Palindrome
  def longestPalindrome(s: String): Int = {
    val lengths = s.groupBy(identity).map(_._2.length).toList
    val addOne = lengths.exists(_ % 2 == 1)
    var counter = 0
    for (i <- 0 until lengths.length) {
      counter += lengths(i) / 2 * 2
    }
    if (addOne) counter + 1 else counter
  }
}
