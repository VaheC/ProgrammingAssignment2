## Caching already calculated numbers, in order to use them in the future, instead of  recalculating.


## The makeCacheMatrix function creates a special "matrix".

makeCacheMatrix <- function(x = matrix()) {

                                            i <- NULL
                                              set <- function(y) {
                                                       x <<- y
                                                        i <<- NULL
                                                     }
        
                                           get <- function() x
                                           setinv <- function(inv) i <<- inv
                                           getinv <- function() i

                                          list(set = set, get = get,
                                          setinv = setinv,
                                          getinv= getinv)

}


## The following function calculates the inverse of the matrix created with the above function.:  it checks  if the inverse has already been calculated. If so, it gets the inverse from the cache. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
         i <- x$getinv()
         if(!is.null(i)) {
                 message("getting cached data")
                 return(i)
         }
         data <- x$get()
         i <- solve(data, ...)
         x$setinv(i)
         i

}

