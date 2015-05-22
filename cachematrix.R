# This code is able to return the inverse of a matrix and also cache the inverse of a matrix

# This function gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                
                #To set the matrix
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                #To get the matrix
                get <- function() x
                #To set the inverse of the matrix
                setInverse <- function(inverse) m <<- inverse
                #To get the inverse of the matrix
                getInverse <- function() m
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }        
        


# This function returns the inverse of a matrix. If the inverse is already
# calculated, the inverse is retrieved from the cache. If this is the case, a 
# message "getting cached data" appears.

cacheSolve<- function(x, ...) {
                m <- x$getInverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setInverse(m)
                m
        }
        
        
