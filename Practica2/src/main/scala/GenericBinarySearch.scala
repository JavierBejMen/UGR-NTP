object GenericBinarySearch {
  //Array[A] se asume ordenado ascendente, devuelve -1 si no encontrado
  def calc[A](colection: Array[A], toSearch: A, l: (A,A) => Boolean): Int = {

    @annotation.tailrec
    def recur(Index0: Int, Index1: Int): Int = {

      if (Index0 > Index1){
        -1
      }else{
        val IndexMitad = (Index0 + Index1)/2
        if (colection(IndexMitad).equals(toSearch)){
          IndexMitad
        }else if(l(toSearch, colection(IndexMitad))){
          recur(Index0, IndexMitad-1)
        }else{
          recur(IndexMitad+1, Index1)
        }
      }
    }

    recur(0, colection.length-1)
  }
}
