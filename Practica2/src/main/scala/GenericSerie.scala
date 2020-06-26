
object GenericSerie {
  def calc(f: (Int, Int) => Int, l0: Int, l1: Int, index: Int): Int = {
    @annotation.tailrec
    def recur(l0: Int, l1: Int, index: Int): Int ={
      if (index == 0){
        l0
      } else if (index == 1){
        l1
      } else{
        recur(l1, f(l0, l1), index - 1)
      }
    }

    recur(l0, l1, index)
  }
}
