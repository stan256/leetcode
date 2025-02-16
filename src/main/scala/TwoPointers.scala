
object TwoPointers extends App {

  // 125. Valid Palindrome
  def isPalindrome(s: String): Boolean = {
    val normalised = s.trim().replaceAll("[^a-zA-Z0-9]", "").toLowerCase
    var i = 0
    var j = normalised.length - 1
    while (i <= j) {
      if (normalised.charAt(i) != normalised.charAt(j))
        return false

      i += 1
      j -= 1
    }
    true
  }

  def isPalindrome_2(s: String): Boolean = {
    var i = 0
    var j = s.length - 1

    def charOrDigit(c: Char): Boolean = {
      val bool = (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || (c >= 48 && c <= 57)
      bool
    }

    def isDigit(c: Char): Boolean = c >= 48 && c <= 57

    def leftPointerMoveToNextCharacter(): Unit =
      while (i < s.length && (s.charAt(i) match {
        case a if charOrDigit(a) => false
        case _ =>
          i += 1
          true
      }
        )) {}

    def rightPointerMoveToNextCharacter(): Unit =
      while (j >= 0 && (s.charAt(j) match {
        case a if charOrDigit(a) => false
        case _ =>
          j -= 1
          true
      }
        )) {}

    while (i <= j) {
      leftPointerMoveToNextCharacter()
      rightPointerMoveToNextCharacter()

      if (i >= s.length || j < 0)
        return true

      val a = s.charAt(i)
      val b = s.charAt(j)

      if (!(a == b || (a + 32 == b && a > 57) || (b + 32 == a && b > 57)))
        return false
      i += 1
      j -= 1
    }
    true
  }

  //  println(isPalindrome("A man, a plan, a canal: Panama"))
  //  println(isPalindrome("ab_a"))
  //  println(isPalindrome(" "))
  //  println(isPalindrome("0P"))

  // 167. Two Sum II - Input Array Is Sorted
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    var i = 0
    var j = numbers.length - 1

    while (i <= j) {
      val res = numbers(i) + numbers(j)
      if (res == target) {
        return Array(i + 1, j + 1)
      } else if (res > target) {
        j -= 1
      } else {
        i += 1
      }
    }

    throw new IllegalStateException()
  }
  //  println(twoSum(Array(2,7,11,15), 9).mkString("Array(", ", ", ")"))

  // 15. 3Sum
  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val sorted = nums.sorted
    var res = Array.empty[Array[Int]]

    for (x <- sorted.indices) {
      var i = 0
      var j = sorted.length - 1

      val target = 0 - sorted(x)
      while (i < j) {
        val sum = sorted(i) + sorted(j)
        if (sum == target && i != j && x != i && x != j) {
          val subResult = Array(sorted(x), sorted(i), sorted(j)).sorted
          if (res.forall(m => !java.util.Arrays.equals(m, subResult))) {
            res = res :+ subResult
          }
          i += 1
          j -= 1
        } else if (sum < target) {
          i += 1
        } else {
          j -= 1
        }
      }
    }

    res.map(_.toList).toList
  }
  //  println(threeSum(Array(-1, 0, 1, 2, -1, -4)).mkString("Array(", ", ", ")"))
  //  println(threeSum(Array(-1,0,1,2,-1,-4,-2,-3,3,0,4)).mkString("Array(", ", ", ")"))

  // 11. Container With Most Water
  def maxArea(height: Array[Int]): Int = {
    var i = 0
    var j = height.length - 1
    var result = 0

    while (i < j) {
      val a = height(i)
      val b = height(j)
      val temp = Math.min(a, b) * (j - i)

      if (result < temp)
        result = temp

      if (a < b)
        i += 1
      else
        j -= 1
    }

    result
  }

  // 53. Maximum Subarray - I actually don't need a heap here
  def maxSubArray(nums: Array[Int]): Int = {
    var maxSum = Int.MinValue
    var currentSum = 0

    var left = 0
    var right = 0

    while (right < nums.length) {
      currentSum += nums(right)
      maxSum = maxSum.max(currentSum)

      while (currentSum < 0 && left <= right && left < nums.length - 1) {
        currentSum -= nums(left)
        left += 1
      }

      right += 1
    }

    maxSum
  }

  // 15. 3Sum
  def threeSum2(nums: Array[Int]): List[List[Int]] = {
    var set = collection.mutable.Set.empty[List[Int]]
    val sorted = nums.sorted

    def binary(x: Int, left: Int, right: Int): Int = {
      val middle = left + (right - left)/2
      if (sorted(middle) == x) middle
      else if (left >= right) -1
      else if (sorted(middle) > x) binary(x, left, middle - 1)
      else binary(x, middle + 1, right)
    }

    for (i <- 0 until nums.length) {
      for (j <- i + 1 until nums.length) {
        val k = binary(0 - sorted(i) - sorted(j), 0, sorted.length - 1)
        if (k != -1 && k != i && k != j) set.addOne(List(sorted(i), sorted(j), sorted(k)).sorted)
      }
    }

    set.toList
  }
}
