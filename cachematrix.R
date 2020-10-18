##This function creates a special "matrix" object that can cache its inverse.
## Función para ivertir matrices (invertibles)
makeCacheMatrix <- 
  function(x = matrix()) {
    
    ## entrada: una matriz cuadrada invertible
    ## salida: una lista de las funciones 
    
    matriz_invertida = NULL
    
    ##1. set the matrix (preparar_matriz)
    preparar_matriz = function(y) {
      # <<-`used to assign a value to an object in an environment that is different from the current environment 
      x <<- y
      matriz_invertida <<- NULL
    }
    
    ##2. get the matrix (obtener_matriz)
    obtener_matriz = function() {
      x
    }
    
    ##3. set the inverse (preparar_matriz_invertida)
    preparar_matriz_invertida = function(invertir) {
      matriz_invertida <<- invertir 
    }
    
    ##4. get the inverse (obtener_matriz_invertida)
    obtener_matriz_invertida = function() {
      matriz_invertida
    }
    
    ##esta lista es usada como entrada de la funcion cachesolve()
    list(preparar_matriz=preparar_matriz, 
         obtener_matriz=obtener_matriz, 
         preparar_matriz_invertida=preparar_matriz_invertida, 
         obtener_matriz_invertida=obtener_matriz_invertida)
  }

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Valida en cache para que en caso de que ya se tenga un valor no se vuelva a calcular
cacheSolve <- function(x, ...) {
  
  ## entrada: la lista de makeCacheMatrix()
  ## salida: la matriz invertida
  
  matriz_invertida = x$obtener_matriz_invertida()
  
  # if is not null the inverse (si no es null matriz invertida)
  if (!is.null(matriz_invertida)){
    # get it from the cache (tomar el valor guardado)
    message("Tomando la información del cache")
    return(matriz_invertida)
  }
  
  # Si no hay información, calculamos la matriz invertida 
  datos_matriz = x$obtener_matriz()
  matriz_invertida = solve(datos_matriz, ...)
  
  # envia la información de la matriz invertida con la funcion preparar_matriz_invertida
  
  x$preparar_matriz_invertida(matriz_invertida)
  
  return(matriz_invertida)  
  
}

