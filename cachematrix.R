## MakeCacheMatrix creates a special "matrix", that can 
## cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      v <- NULL
      set <- function(y){
            x <<- y
            v <<- NULL
      }
      get <- function() x
      setinverse <- function(inv){
            v <<- inv
      }
      getinverse <- function() v
      list (set = set, get = get, setinverse = setinverse,
            getinverse = getinverse)
}


## The following function calculates the inverse of the 
## special "matrix" created with the above function. However,
## it first checks to see if the inverse has already been created.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      v <- x$getinverse()
      ## Check to see if the inverse is cached
      if(!is.null(v)){
            message("getting inverse matrix from cache")
            return(v)
      }
      data <- x$get()
      ## Create the Inverse of the matrix with the solve func
      v <- solve(data)
      ## Set the inverse matrix into cache
      x$setinverse(v)
      v
}
