import org.scalatest.FunSuite

class GenericJumpSearchTest extends FunSuite {
  test("Found"){
    assert(GenericJumpSearch.calc[Int](
      List[Int](0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610),
      (x: Int, y: Int) => x < y,
      233) == 13)
  }

  test("NotFound"){
    assert(GenericJumpSearch.calc[Int](
      List[Int](0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610),
      (x: Int, y: Int) => x < y,
      9) == -1)
  }
}
