import org.scalatest.FunSuite

class StringBalanceTest extends FunSuite {
  test("Balanced_1") {
    assert(StringBalance.calc(List('(', ')')))
  }

  test("Balanced_2") {
    assert(StringBalance.calc(List()))
  }

  test("Balanced_3") {
    assert(StringBalance.calc(List('(','a','%','&',')','(','(','@','(',')',')','-',')')))
  }

  test ("Unbalanced_1") {
    assert(!StringBalance.calc(List(')')))
  }

  test ("Unbalanced_2") {
    assert(!StringBalance.calc(List(')', '(')))
  }

  test ("Unbalanced_3") {
    assert(!StringBalance.calc(List('a', '%', '&', ')', '(', '(', '@', '(', ')', ')', '-', ')')))
  }

  test ("Unbalanced_4") {
    assert(!StringBalance.calc(List('a','%','&','(','(','(','@','(',')',')','-',')')))
  }
}
