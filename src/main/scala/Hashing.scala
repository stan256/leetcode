object Hashing extends App {
  // 1832. Check if the Sentence Is Pangram
  def checkIfPangram(sentence: String): Boolean = {
    val set = sentence.toSet
    ('a' to 'z').forall(set.contains)
  }
  //  println(checkIfPangram("thequickbrownfoxjumpsoverthelazydog"))
  //  println(checkIfPangram("leetcode"))

  // 268. Missing Number
  def missingNumber(nums: Array[Int]): Int = {
    val set = nums.toSet
    (0 to nums.length).find(x => !set(x)).get
  }
  //  println(missingNumber(Array(3,0,1)))
}
