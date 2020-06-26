import org.scalatest.FunSuite

class GenericSerieTest extends FunSuite {
  test("Fibonacci"){
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 0, 1, 0) == 0)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 0, 1, 1) == 1)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 0, 1, 7) == 13)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 0, 1, 8) == 21)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 0, 1, 22) == 17711)
  }

  test("Lucas"){
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 2, 1, 0) == 2)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 2, 1, 1) == 1)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 2, 1, 7) == 29)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 2, 1, 8) == 47)
    assert(GenericSerie.calc((x:Int, y:Int) => x + y, 2, 1, 22) == 39603)
  }

  test("Pell"){
    assert(GenericSerie.calc((x:Int, y:Int)=>2*y+x, 2, 6, 0) == 2)
    assert(GenericSerie.calc((x:Int, y:Int)=>2*y+x, 2, 6, 1) == 6)
    assert(GenericSerie.calc((x:Int, y:Int)=>2*y+x, 2, 6, 2) == 14)
    assert(GenericSerie.calc((x:Int, y:Int)=>2*y+x, 2, 6, 3) == 34)
  }

  test("Jacobsthal"){
    assert(GenericSerie.calc((x:Int, y:Int) => y+2*x, 0, 1, 0) == 0)
    assert(GenericSerie.calc((x:Int, y:Int) => y+2*x, 0, 1, 1) == 1)
    assert(GenericSerie.calc((x:Int, y:Int) => y+2*x, 0, 1, 7) == 43)

  }
}
