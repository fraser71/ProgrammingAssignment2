## The following functions allow for the creation of a matrix where the invers
## is cached after it has been calculated. This prevents the re-execution 
## of a rather expensive operation.

## makeCacheMatrix creates a 'special' matrix object that stores the 
## matrix as well as its inverce. Use this matrix with cacheSolve() 

makeCacheMatrix <- function(m.orig = matrix()) {
  m.result <- NULL
  set <- function(m.set) {
    m.orig <<- m.set
    m.result <<- NULL
  }
  get <- function() m.orig
  setresult <- function(result) m.result <<- result
  getresult <- function() m.result
  list(set = set, get = get,
       setresult = setresult,
       getresult = getresult)
}


## cacheSolve finds the inverse of the matrix created by makeCacheMatrix(). 
## The inverse of the matrix is only calculated once. On subsequent calls, the 
## cached result is used.

cacheSolve <- function(x, ...) {
  mcache <- NULL
  mlist <- x
  
  m <- mlist$getresult()
  if(!is.null(m)){
    print("using cached data")
    return(m)
  }
  data <- mlist$get()
  result <- solve(data, ...)
  mlist$setresult(result)
  result
}
