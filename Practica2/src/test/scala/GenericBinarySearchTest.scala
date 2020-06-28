import org.scalatest.FunSuite

class GenericBinarySearchTest extends FunSuite {
  test("Found"){
    assert(GenericBinarySearch.calc(
      Array(1,2,3,4,5,6,7,8,9),
      2,
      (x: Int, y: Int) => x < y) != -1)
    assert(GenericBinarySearch.calc(
      Array('1','2','3','4','5','6','7','8','9'),
    '2',
      (x: Char, y: Char) => x < y) != -1)
  }

  test("NotFound"){
    assert(GenericBinarySearch.calc(
      Array(1,2,3,4,5,6,7,8,9),
      10,
      (x: Int, y: Int) => x < y) == -1)
    assert(GenericBinarySearch.calc(
      Array('1','2','3','4','5','6','7','8','9'),
      'j',
      (x: Char, y: Char) => x < y) == -1)
  }
}
