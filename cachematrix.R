## Functions in this file:
## 
## makeCacheMatrix(x=matrix()) - creates a list for caching of matrices
## cacheSolve(specialMatrix)   - fills cache initially and uses it subsequently
## createAndExecTest           - test function

## create special "matrix" (actually a list) to cache inverted matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- matrix()               ##initialize result
  
  set <- function(y) {
    x <<- y
    m <- matrix()
  }
  get <- function() x
  setInverse <- function(im) m <<- im
  getInverse <- function() m

    list(set=set, get=get, 
       setInverse = setInverse, 
       getInverse=getInverse)  

}


## Fill/Access cached special matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  print("Original Matrix")
  m0 <- x$get()
  print(m0)
  print("Inversed Matrix")
  m1 <- x$getInverse()
  if (!all(is.na(m1))) {
    print("Taken from cache")
    
    return(m1) 
  } else {
    print("Calculated")
    m1 <- solve(x$get())
    x$setInverse(m1)
  }
  return(m1)
}

#Testdata to copy to console

#m1 <- rbind(c(1, -0.25), c(-0.25, 1))
#m2 <- rbind(c(2, -1, -3), c(-4, 4, 2), c(3, -5, -3))
#x1 <- makeCacheMatrix(m1)
#x2 <- makeCacheMatrix(m2) 

# call cacheSolve(x1/x2) twice for testing

