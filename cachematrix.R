## The following functions allow for the creation of a matrix where the invers
## is cached after it has been calculated. This prevents the re-execution 
## of a rather expensive operation.

## makeCacheMatrix creates a 'special' matrix object that stores the 
## matrix as well as its inverce. Use this matrix with cacheSolve() 

makeCacheMatrix <- function(m.orig = matrix()) {
  m.result <- NULL  #when created the result is not available
  set <- function(m.set) {
    m.orig <<- m.set #save the new matrix
    m.result <<- NULL #clear the result
  }
  
  get <- function() m.orig #return the original
  
  setresult <- function(result) m.result <<- result #save a result matrix
  
  getresult <- function() m.result #return the result matrix
  
  list(set = set, get = get,
       setresult = setresult,
       getresult = getresult)
}


## cacheSolve finds the inverse of the matrix created by makeCacheMatrix(). 
## The inverse of the matrix is only calculated once. On subsequent calls, the 
## cached result is used.

cacheSolve <- function(x, ...) {
  mcache <- NULL  #Cached matrix is initially null
  
  m <- x$getresult() #Get the cached matrix and check if it exists
  if(!is.null(m)){
    message("Using cached data")
    return(m) #Use and return the cached matrix if found
  }
  
  # Cached result was not found... Calculate
  data <- x$get() #Get the initial matrix
  result <- solve(data, ...) #Solve it
  x$setresult(result) #Cache it
  result #Return it
}

#done
