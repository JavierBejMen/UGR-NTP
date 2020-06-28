import Math.sqrt

object GenericJumpSearch {
  def calc[A](collection: List[A],l: (A,A) => Boolean,  toSearch: A): Int = {
    val blockSize = sqrt(collection.size).intValue()

    @annotation.tailrec
    def recur(index: Int): Int ={
      if(l(toSearch, collection(index))){
        var found = -1
        for(i <- index-blockSize to index){
          if(collection(i).equals(toSearch)){
            found = i
          }
        }

        found
      }else{
        recur(if (index+blockSize > collection.size) collection.size else index+blockSize)
      }
    }

    recur(blockSize-1)
  }
}
