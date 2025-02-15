
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
}
