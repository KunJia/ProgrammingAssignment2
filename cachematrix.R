## Create a cache marix object that can be used to repeatably solve the inverse of the marix


## Create a matrix object that can be cached its inverse

makeCacheMatrix <- function(x = matrix()) {
          invCache <- NULL
          set <- function(y){
                    x <<- y
                    invCache <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) invCache <<- inverse
          getinverse <- function() invCache
          list(set = set,
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Return the inverse of the above Matrix object

cacheSolve <- function(x, ...) {
          invFinal <- x$getinverse()
          if (!is.null(invFinal)){
                    message("getting cached data")
                    return(invFinal)
          }
          data <- x$get()
          invFinal <- solve(data, ...)
          x$setinverse(invFinal)
          invFinal
          ## Return a matrix that is the inverse of 'x'
}
