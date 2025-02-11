import scala.collection.mutable

object Stack extends App {
  // 20. Valid Parentheses
  def isValid(s: String): Boolean = {
    import scala.collection.mutable

    if (s.isEmpty) return true
    else if (s.length == 1) return false

    val stack = mutable.Stack[Char](s(0))

    for (c <- s.substring(1)) {
      if (c == '{' || c == '(' || c == '[')
        stack.push(c)
      else {
        val maybeHead = stack.headOption

        if ((c == '}' && maybeHead.contains('{')) || (c == ')' && maybeHead.contains('(')) || (c == ']' && maybeHead.contains('['))) {
          stack.pop()
        } else {
          return false
        }
      }
    }

    stack.isEmpty
  }
  //  println(isValid("(])"))

  // 155. Min Stack
  class MinStack() {

    import scala.collection.mutable

    var stack = mutable.Stack[(Int, Int)]()
    var min = 0

    def push(i: Int): Unit = {
      if (stack.isEmpty) {
        min = i
      }

      if (i < min) {
        min = i
      }

      stack = stack.push((i, min))
    }

    def pop(): Unit = {
      val a = stack.pop()

      if (a._2 == min) {
        if (stack.isEmpty) {
          min = 0
        } else {
          min = stack.head._2
        }
      }
    }

    def top(): Int = stack.head._1

    def getMin(): Int = stack.head._2
  }

  //  println {
  //    val x = new MinStack()
  //    x.push(-2)
  //    x.push(0)
  //    x.push(-3)
  //    x.getMin()
  //    x.pop()
  //    x.top()
  //    x.getMin()
  //    x.pop()
  //    x.pop()
  //  }

  // 150. Evaluate Reverse Polish Notation
  def evalRPN(tokens: Array[String]): Int = {
    import scala.collection.mutable

    val signs = Set('+', '-', '*', '/')

    def isSign(s: String): Boolean = signs.contains(s(0)) && s.length == 1

    def isDigit(s: String): Boolean = !isSign(s)

    def calculate(sign: String, a: Int, b: Int): Int =
      sign match {
        case "+" => a + b
        case "-" => a - b
        case "*" => a * b
        case "/" => a / b
        case _ => throw new RuntimeException()
      }

    val stack = mutable.Stack.from(tokens)

    var first = tokens.length - 3
    var second = tokens.length - 2
    var third = tokens.length - 1

    while (stack.size != 1) {
      if (isDigit(stack(first)) && isDigit(stack(second)) && isSign(stack(third))) {
        val result = calculate(stack(third), stack(first).toInt, stack(second).toInt)
        stack(first) = result.toString
        stack.remove(third)
        stack.remove(second)
      } else {
        first += -1
        second += -1
        third += -1
      }
    }

    stack.pop().toInt
  }

  def evalRPN_betterStackApproach(tokens: Array[String]): Int = {
    import scala.collection.mutable

    val signs = "+-/*"

    def isSign(s: String): Boolean = signs.contains(s)

    def calculate(sign: String, a: Int, b: Int): Int = sign match {
      case "+" => a + b
      case "-" => a - b
      case "*" => a * b
      case "/" => a / b
      case _ => throw new RuntimeException()
    }

    val stack = mutable.Stack.empty[String]

    for (s: String <- tokens) {
      if (isSign(s)) {
        val first = stack.pop().toInt
        val second = stack.pop().toInt
        val i = calculate(s, second, first)
        stack.push(i.toString)
      }
      else {
        stack.push(s)
      }
    }

    stack.pop().toInt
  }

  //  println(evalRPN_betterStackApproach(Array("10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+")))

  // 22. Generate Parentheses
  def generateParenthesis(n: Int): List[String] = {
    val ans = scala.collection.mutable.ListBuffer[String]()

    def backtrack(tmp: String, open: Int, close: Int): Unit = {
      if (tmp.length == n * 2) ans += tmp
      if (open < n) backtrack(tmp :+ '(', open + 1, close)
      if (close < open) backtrack(tmp :+ ')', open, close + 1)
    }

    backtrack("", 0, 0)
    ans.toList
  }

  //  println(generateParenthesis(3))


  // 739. Daily Temperatures
  def dailyTemperatures2(temperatures: Array[Int]): Array[Int] = {
    val stack = scala.collection.mutable.Stack.empty[(Int, Int)]
    val result = Array.fill[Int](temperatures.length)(0)

    for (i <- temperatures.zipWithIndex) {
      if (stack.isEmpty) {
        stack.push(i)
      } else {
        var prev = stack.pop()
        var searchFurther = true

        while (searchFurther) {
          if (prev._1 < i._1) {
            val indexDiff = i._2 - prev._2


            result(prev._2) = indexDiff
            if (stack.nonEmpty) {
              prev = stack.pop()
            } else {
              stack.push(i)
              searchFurther = false
            }
          } else {
            stack.push(prev)
            stack.push(i)
            searchFurther = false
          }
        }
      }
    }

    result
  }


  // 739. Daily Temperatures -- 2.0
  def dailyTemperatures(temperatures: Array[Int]): Array[Int] = {
    val stack = collection.mutable.Stack.empty[(Int, Int)]
    val result = Array.fill[Int](temperatures.length)(0)

    val tz = temperatures.zipWithIndex
    for (e <- tz) {
      while (stack.headOption.exists(x => x._1 < e._1)) {
        val pair = stack.pop()
        result(pair._2) = e._2 - pair._2
      }
      stack.push(e)
    }

    result
  }

  //  println(dailyTemperatures(Array(73, 74, 75, 71, 69, 72, 76, 73)).mkString("Array(", ", ", ")"))

  // 853. Car Fleet
  // there was much better approach of comparing only the amount of time needed to get to destination by these cars
  def carFleet(target: Int, position: Array[Int], speed: Array[Int]): Int = {
    if (position.length > 1) {
      var counter = 0
      val priorityQueue = scala.collection.mutable.PriorityQueue.from(position.zip(speed))(_._1 - _._1)

      while (priorityQueue.length >= 2) {
        val front = priorityQueue.dequeue()
        val back = priorityQueue.dequeue()
        val frontReachesTarget = (target - front._1).toDouble / front._2
        val speedDifference = back._2 - front._2

        if (speedDifference <= 0) {
          counter += 1
          priorityQueue.enqueue(back)
        } else {
          val backCatchesFrontTime = (front._1 - back._1).toDouble / speedDifference
          if (frontReachesTarget < backCatchesFrontTime) {
            counter += 1
            priorityQueue.enqueue(back)
          } else {
            priorityQueue.enqueue(front)
          }
        }
      }

      counter += priorityQueue.length
      counter
    } else {
      position.length
    }
  }

  //  println(carFleet(12, Array(10, 8, 0, 5, 3), Array(2, 4, 1, 1, 3)))
  //  println(carFleet(100, Array(0, 2, 4), Array(4, 2, 1)))
  //  println(carFleet(10, Array(0, 4, 2), Array(2, 1, 3)))
  //  println(carFleet(20, Array(6, 2, 17), Array(3, 9, 2)))
  //  println(carFleet(12, Array(4,0,5,3,1,2), Array(6,10,9,6,7,2)))
  //  println(carFleet(13, Array(10,2,5,7,4,6,11), Array(7,5,10,5,9,4,1)))
  // todo to make better approach


  // 84. Largest Rectangle in Histogram
  // todo - to solve in TwoPointers approach
  def largestRectangleArea(heights: Array[Int]): Int = {
    val stack = scala.collection.mutable.Stack.empty[(Int, Int)]
    var res = 0

    for (e <- heights.zipWithIndex) {
      var start = e._2
      while (stack.headOption.exists(_._1 > e._1)) {
        val prev = stack.pop()
        val maybeResult = prev._1 * (e._2 - prev._2)
        if (maybeResult > res)
          res = maybeResult
        start = prev._2
      }
      stack.push((e._1, start))
    }

    stack.foreach(e => {
      val subResult = e._1 * (heights.length - e._2)
      if (subResult > res)
        res = subResult
    })

    res
  }

  //  println(largestRectangleArea(Array(2, 1, 5, 6, 2, 3)))
  //  println(largestRectangleArea(Array(2, 1, 6, 5, 2, 3)))
  //  println(largestRectangleArea(Array(2, 4)))
  //  println(largestRectangleArea(Array(1, 1)))
  //  println(largestRectangleArea(Array(2, 1, 2)))
  //  println(largestRectangleArea(Array(5, 4, 1, 2)))
  //  println(largestRectangleArea(Array(4, 2, 0, 3, 2, 5)))
  //  println(largestRectangleArea(Array(3, 2, 5)))

  // 1047. Remove All Adjacent Duplicates In String
  def removeDuplicates(s: String): String = {
    var stack = List.empty[Char]
    for (c <- s) {
      if (stack.headOption.contains(c))
        stack = stack.drop(1)
      else
        stack = c :: stack
    }
    stack.reverse.foldLeft("")(_ + _)
  }

  // println(removeDuplicates("abbaca"))
  // println(removeDuplicates("azxxzy"))

  // 844. Backspace String Compare
  def backspaceCompare(s: String, t: String): Boolean = {
    def backspace(s: String): String = {
      var stack = List.empty[Char]
      for (c <- s) {
        if (c == '#') stack = stack.drop(1)
        else stack = c :: stack
      }
      stack.mkString
    }

    backspace(s) == backspace(t)
  }
  // println(backspaceCompare("a#c", "b"))

  // 71. Simplify Path
  def simplifyPath(path: String): String = {
    val list = path.split("/+").drop(1)
    var stack: List[String] = List()
    val sign = "/"

    for (s <- list) {
      s match {
        case "." =>
        case ".." => if (stack.nonEmpty) stack = stack.drop(1)
        case x => stack = sign + x :: stack
      }
    }
    if (stack.isEmpty) sign else stack.reverse.mkString
  }

  //  println(simplifyPath("/home//foo/"))
  //  println(simplifyPath("/home//.foo/"))
  //  println(simplifyPath("/home//../"))
  //  println(simplifyPath("/home//./"))
  //  println(simplifyPath("/.....///.../..//./"))
  // println(simplifyPath("/a/./b/../../c/"))

  // 1544. Make The String Great
  def makeGood(str: String): String = {
    val stack = collection.mutable.Stack.empty[Char]
    for (c <- str) {
      if (c > 'Z' && stack.headOption.contains(c - 32))
        stack.pop()
      else if (c < 'a' && stack.headOption.contains(c + 32))
        stack.pop()
      else
        stack.push(c)
    }
    stack.reverse.mkString
  }
  // println(makeGood("leEeetcode"))

  // 20. Valid Parentheses
  def isValid_2(s: String): Boolean = {
    val stack = collection.mutable.Stack.empty[Character]
    val map = Map('}' -> '{', ')' -> '(', ']' -> '[')
    var i = 0

    while (i < s.length) {
      val c = s.charAt(i)
      if ("])}".contains(c)) {
        val mapping = map(c)
        if (stack.headOption.contains(mapping)) stack.pop()
        else return false
      } else {
        stack.push(c)
      }
      i += 1
    }

    stack.isEmpty
  }

  // 20. Valid Parentheses
  def isValid_3(s: String): Boolean = {
    val map = Map(')' -> '(', '}' -> '{', ']' -> '[')
    val openBrackets = Set('{', '(', '[')
    val stack = collection.mutable.Stack.empty[Char]
    for (c <- s) {
      if (openBrackets.contains(c)) {
        stack.push(c)
      } else {
        if (stack.nonEmpty) {
          if (stack.head == map(c)) {
            stack.pop()
          } else {
            stack.push(c)
          }
        } else {
          stack.push(c)
        }
      }
    }
    true
  }
}


