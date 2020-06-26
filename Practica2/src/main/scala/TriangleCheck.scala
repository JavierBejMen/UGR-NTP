import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object  TriangleCheck extends Properties("Triangle Pascal"){
  private val MAXIMO=30

  // generador
  val coordenadasExtremos = for{
    fila <- Gen.choose(0, MAXIMO)
    columna <- Gen.oneOf(0, fila)
  } yield (fila, columna)

  // propiedad
  property("Extremos valen 1") = {
    forAll(coordenadasExtremos) { (i) => {
      val resultado = Triangle.calcular(i._1, i._2)
      println("Fila = "+i._1 + " columna: "+i._2 + " numero: "+resultado)
      resultado == 1
    }
    }
  }

  // generador coordenadas internas
  val coordenadasInteriores = for {
    fila <- Gen.choose(2, MAXIMO)
    columna <- Gen.choose(1, fila-1)
  } yield(fila, columna)

  property("Valores interiores: ") = {
    forAll(coordenadasInteriores) { (i) => {
      val resultado=Triangle.calcular(i._1, i._2)
      val supIzquierda=Triangle.calcular(i._1-1, i._2-1)
      val supDerecha=Triangle.calcular(i._1-1, i._2)
      println("Fila = "+i._1 + " columna: "+i._2 + " numero: "+resultado)

      resultado == (supDerecha + supIzquierda)
    }
    }
  }
}
