## cachematrix.R contains two functions. The firt function creates an R object that
## stores a matrix and its inverse. The second one, retrieves the mean from the cached  
## value that is stored in the first function object's environment.

## makeCacheMatrix builds four functions and returns those functions as a list into 
## the parent enviroment.It takes a matrix as argument.
## The first function, set(), assigns a value to an object in the parent environment.
## The second function, get(), retrieves the object from the parent environment.
## The third function, setinversematrix(), calculates the inverse of the matrix and
## assigns it to the objecto in the parent environment.
## The fourth function, getinversematrix(), retrieves the inverse matrix of the object
## matrix in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m is an object in the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## cacheSolve retrieves the inverse matrix of an object of type makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached matrix") ## message displayed when the inverse of a matrix is called more than once.
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
