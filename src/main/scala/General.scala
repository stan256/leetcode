object General extends App {
  // 704. Binary Search
  def binarySearch(arr: Array[Int], n: Int): Int = {
    var left = 0
    var right = arr.length - 1

    while (left <= right) {
      val middleIndex = (left + right)/2

      val middle = arr(middleIndex)
      if (n == middle)
        return middleIndex
      else if (n > middle)
        left = middleIndex + 1
      else
        right = middleIndex - 1
    }

    -1
  }

  // 2043. Simple Bank System
  class Bank(balances: Array[Long]) {
    def addMoney(n: Int, money: Long): Unit = {
      balances(n) += money
    }

    def subtractMoney(n: Int, money: Long): Unit = {
      balances(n) -= money
    }

    def accountExists(account: Int): Boolean = account <= balances.length

    def transfer(account1: Int, account2: Int, money: Long): Boolean = {
      if (accountExists(account1) && accountExists(account2) && balances(account1 - 1) >= money) {
        addMoney(account2 - 1, money)
        subtractMoney(account1 - 1, money)
        true
      } else false
    }

    def deposit(account: Int, money: Long): Boolean = {
      if (accountExists(account)) {
        addMoney(account - 1, money)
        true
      } else false
    }

    def withdraw(account: Int, money: Long): Boolean = {
      if (accountExists(account) && balances(account - 1) >= money) {
        subtractMoney(account - 1, money)
        true
      } else false
    }
  }

}
