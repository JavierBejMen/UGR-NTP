import org.scalatest.FunSuite

class MoneyChangeTest extends FunSuite{
  test("sumCorrect"){
    MoneyChange.calc(10, List[Int](5,2,1)).foreach(f => assert(f.sum == 10))
    MoneyChange.calc(59, List[Int](5,2,1)).foreach(f => assert(f.sum == 59))
    MoneyChange.calc(20, List[Int](8,3,2,1)).foreach(f => assert(f.sum == 20))
  }
  test("notEmpty"){
    assert(MoneyChange.calc(10, List[Int](5,2,1)).nonEmpty)
    assert(MoneyChange.calc(59, List[Int](5,2,1)).nonEmpty)
  }
  test("Number"){
    assert(MoneyChange.calc(11, List[Int](5,2,1)).size == 11)
    assert(MoneyChange.calc(20, List[Int](8,3,2,1)).size == 67)
  }
}
