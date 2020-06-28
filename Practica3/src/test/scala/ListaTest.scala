import java.security.InvalidParameterException

import org.scalatest.FunSuite

class ListaTest extends FunSuite {
  test("apply"){
    assert(Lista(1, 2, 3, 4, 5).isInstanceOf[Cons[Int]])
    assert(Lista(1.3, 2.3, 3.3, 4.3, 5.3).isInstanceOf[Cons[Double]])
    assert(Lista('1', '2', '3', '4', '5').isInstanceOf[Cons[Char]])
    assert(Lista("1", "2", "3", "4", "5").isInstanceOf[Cons[String]])
  }

  test("longitud"){
    assert(Lista.longitud(Lista(1, 2, 3, 4, 5)) == 5)
    assert(Lista.longitud(Lista(1.3, 2.3, 3.3, 4.3, 5.3)) == 5)
    assert(Lista.longitud(Lista('1', '2', '3', '4', '5')) == 5)
    assert(Lista.longitud(Lista("1", "2", "3", "4", "5")) == 5)
  }

  test("sumaEnteros"){
    assert(Lista.sumaEnteros(Lista[Int](1,1,1,1,1)) == 5)
  }

  test("productoEnteros"){
    assert(Lista.productoEnteros(Lista[Int](1,1,1,1,5)) == 5)
  }

  test("asignarCabeza"){
    assert(Lista.asignarCabeza(Lista(1,2), 3).asInstanceOf[Cons[Int]].cabeza == 3)
  }

  test("obtenerCabeza"){
    assert(Lista.obtenerCabeza(Lista("cabeza", "nocabeza")) == "cabeza")
    assertThrows[InvalidParameterException](Lista.obtenerCabeza(Lista()))
  }

  test("obtenerCola"){
    assert(Lista.obtenerCola(Lista(1,2,3,4,5)) == Lista(2,3,4,5))
  }

  test("eliminar"){
    assert(Lista.eliminar(Lista(1,2,3,4,5,6,7,8), 7) == Lista(8))
  }

  test("eliminarMientras"){
    assert(Lista.eliminarMientras(Lista(1,2,3,4,5), (x: Int) => x < 5) == Lista(5))
  }

  test("eliminarUltimo"){
    assert(Lista.eliminarUltimo(Lista(1,2,3,4,5,6)) == Lista(1,2,3,4,5))
  }

  test("foldLeft"){
    assert(Lista.foldLeft[Int, Double](Lista(2,2,2,2), 1)((x, y) => x*y) == 16)
  }

  test("sumaFoldLeft"){
    assert(Lista.sumaFoldLeft(Lista(1,1,1,1,1)) == 5)
  }


}
