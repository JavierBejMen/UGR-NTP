import org.scalatest.FunSuite

class TriangleTest extends FunSuite {



  test("Valor en extremo debe ser 1 independientemente de la funcion") {
    assert(Triangle.calcular(5,5) == 1)
    assert(Triangle.calcular(5,0) == 1)
  }
}