object StringBalance {
  def calc(cadena: List[Char]): Boolean = {

    @annotation.tailrec
    def recur(index: Int, n0: Int, n1: Int): Boolean = {
      if (index < 0){
        n0 == n1
      }else{
        if(cadena(index) == ')'){
          recur(index-1, n0+1, n1)
        }else if(cadena(index) == '('){
          if(n1==n0){
            false
          }else{
            recur(index-1, n0, n1+1)
          }
        }else{
          recur(index-1, n0, n1)
        }
      }
    }

    recur(cadena.size-1, 0, 0)
  }
}
