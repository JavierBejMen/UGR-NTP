import java.security.InvalidParameterException

import scala.runtime.Nothing$

/**
 * Interfaz generica para la lista
 *
* @tparam A
*/
sealed trait Lista[+A]

/**
* Objeto para definir lista vacia
*/
case object Nil extends Lista[Nothing]

/**
* Clase para definir la lista como compuesta por elemento inicial
* (cabeza) y resto (cola)
* @param cabeza
* @param cola
* @tparam A
*/
case class Cons[+A](cabeza : A, cola : Lista[A]) extends Lista[A]


object Lista{
  /**
  * Metodo para permitir crear listas sin usar new
  * @param elementos secuencia de elementos a incluir en la lista
  * @tparam A
  * @return
  */
  def apply[A](elementos : A*) : Lista[A] = elementos.size match {
    case 0 => Nil
    case _ => Cons(elementos.head, apply(elementos.tail: _*)).asInstanceOf[Lista[A]]
  }

  /**
  * Obtiene la longitud de una lista
  * @param lista
  * @tparam A
  * @return
  */
  def longitud[A](lista : Lista[A]) : Int = lista match {
    case Nil => 0
    case Cons(_ , cola) => 1 + longitud(cola)
  }

  /**
   * Metodo para sumar los valores de una lista de enteros
   * @param enteros
   * @return
   */
  def sumaEnteros(enteros : Lista[Int]) : Int = enteros match {
    case Nil => 0
    case Cons(head, tail) => head + sumaEnteros(tail)
  }

  /**
   *  Metodo para multiplicar los valores de una lista de enteros
   * @param enteros
   * @return
   */
  def productoEnteros(enteros : Lista[Int]) : Int = enteros match {
    case Nil => 1
    case Cons(head, tail) => head * productoEnteros(tail)
  }

  /**
  * Reemplaza la cabeza por nuevo valor. Se asume que si la lista esta vacia
  * se devuelve una lista con el nuevo elemento
  *
  * @param lista
  * @param cabezaNueva
  * @tparam A
  * @return
  */
  def asignarCabeza[A](lista : Lista[A], cabezaNueva : A) : Lista[A] = lista match {
    case Nil => Cons(cabezaNueva, Nil)
    case Cons(_, tail) => Cons(cabezaNueva, tail)
  }

  /**
  * Obtiene el elemento que ocupa la cabeza de la lista
  *
  * @param lista No puede ser vacia
  * @tparam A
   * @throws InvalidParameterException
  * @return
  */
  @throws[InvalidParameterException]
  def obtenerCabeza[A](lista : Lista[A]): A = lista match {
    case Nil => throw new InvalidParameterException
    case Cons(head, _) => head
  }

  /**
   * Elimina el elemento cabeza de la lista y devuelve la lista con
   * el resto de elementos
   * @param lista
   * @tparam A
   * @return
   */
  def obtenerCola[A](lista : Lista[A]): Lista[A] = lista match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
   * Elimina los n primeros elementos de una lista
  * @param lista lista con la que trabajar
  * @param n numero de elementos a eliminar
  * @tparam A tipo de datos
  * @return
  */
  def eliminar[A](lista : Lista[A], n: Int) : Lista[A] = lista match {
    case Nil => Nil
    case Cons(head, tail) => if(n > 0) eliminar(tail, n-1) else lista
  }

  
}
