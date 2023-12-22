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

  def missingNumber_arithmeticProgression(nums: Array[Int]): Int = nums.length * (nums.length + 1) / 2 - nums.sum
  //  println(missingNumber_arithmeticProgression(Array(3,0,1)))

  // 1426. Counting Elements
  def countElements(arr: Array[Int]): Int = {
    val set = arr.toSet
    arr.fold[Int](0) { (r, x) => if (set(x + 1)) r + 1 else r }
  }
  //  println(countElements(Array(1,2,3)))
  //  println(countElements(Array(1,3, 5,5)))

}
