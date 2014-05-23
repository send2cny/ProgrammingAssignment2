#Prepare Accessors (i.e - set & get) for a Matrix object as well the inverse of itself
makeCacheMatrix <- function(x = matrix()) {
#vector i for inverse, just like m for mean in the example  
  i <- NULL
#Set for the Matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
#Get for the Matrix
  get <- function(){
    x
  }
#Set for the inverse, just like the example setmean  
  setinverse <- function(inverse){
    i <<- inverse
  }
#Get for the inverse
  getinverse <- function(){
    i
  }
#Return value
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#Just like the example, return the inverse if previous computed.
#Else compute using solve() and return inverse. 

cacheSolve <- function(x, ...) {
# Grap i from model (i.e - makeCacheMatrix)
  i <- x$getinverse()
# if previous the inverse of the matrix - (i) is computed, return value
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
# Grap matrix from model  
  data  <- x$get()
# inverse the matrix
  i  <- solve(data, ...)
# pass the inverse to model
  x$setinverse(i)
# display the inverse
  i
}

