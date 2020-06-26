object Triangle extends App{
  def calcular(fila:Int, columna:Int) : Int = {
    if(columna == 0 || columna == fila) 1
    else calcular(fila-1, columna-1) + calcular(fila-1, columna)
  }

  for(fila <- 0 to 10){
    for(columna <- 0 to fila) {
      print(calcular(fila, columna)+ "  ")
    }
    println()
  }
}
