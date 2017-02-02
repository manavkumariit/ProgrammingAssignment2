## Put comments here that give an overall description of what your
## functions do
## Given A Matrix, cache the matrix and it's inverse in a cache in the parent environment
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m){
        x <<- m
        inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) {
    print('Caching inverse')
    inv <<- inverse
    }
  getInv <- function() {
    
    if (is.null(inv)){
      print('Cached Inverse is null')
    } else {
      print('Getting Cached Inverse')  
    }
    inv
  }
  list(get = get, set = set, getInv = getInv, setInv = setInv)
    
}


## Write a short comment describing this function
## Get the cached inverse from the parent environment. If it does not exist, calculate and cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)){
    return(inv)
  }
  
  tmat <- x$get()
  tInv <- solve(tmat,...)
  x$setInv(tInv)
  tInv
  }
