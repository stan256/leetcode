
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
//  println(isPalindrome("A man, a plan, a canal: Panama"))
  println(isPalindrome("ab_a"))

}
