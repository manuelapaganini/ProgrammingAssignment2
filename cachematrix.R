## The first function makeCacheMatrix, takes a matrix as an argument and returns a list of functions,
## permitting to store and retrieve the inverse of a matrix. 
## The items of the returned list are named, so the functions can be accessed individually by <objectname>$<function>.

## The second function takes the object created by makeCacheMatrix and returns the cached inverse 
## if it already has been calculated. If not, it calculates and prints the inverse and stores it in the 
## makeCacheMatrix object. 


## create a list object containing functions to store and retrieve the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-  function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## check if inverse has already been calculated - if so return it, if not calculate and return it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <-  x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

