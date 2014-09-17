##------------------------------------------------------------------------------
##
##  cachematrix.R
##
##  This file holds two functions:
##  - makeCacheMatrix(): Create a matrix object that also can cache its inverse
##  - cacheSolve(): Get cached matrix if existing, else inverse matrix object.
##  
##  Usage:
##      new <- makeCacheMatrix(matrix(1:4, 2, 2))       # Create matrix 'new'
##      new$get()                                       # Read 'new'
##      new$set(matrix(10:18, 3, 3))                    # Set 'new'
##      cacheSolve(new)                             # Return inverse of 'new'
##          new$getCache()                              # Read cached value
##          new$setCache()                              # Set cahced value
##  
##------------------------------------------------------------------------------

##------------------------------------------------------------------------------
##
##  makeCacheMatrix()
##
##  This function should create a matrix object, a cached matrix, and a list of 
##  functions:
##      get() - return the original matrix
##      set() - set the original matrix, indicate invalid cache by -> NULL
##      getCache() - return the cached matrix
##      setCache() - set the cached matrix
##

makeCacheMatrix <- function(data = matrix()) {
    cache <- NULL
    list (
        get = function() data,
        set = function(newData) {
            data <<- newData
            cache <<- NULL
        },
        getCache = function() cache,
        setCache = function(newCache) {
            cache <<- newCache
        }
    )
}


##------------------------------------------------------------------------------
##
##  cacheSolve()
##
##  This function should return the inverse of a matrix. If a cached result is 
##  stored by makeCacheMatrix(), this result should be returned.
##  If no inverse is stored the inverse should be computed and stored in 
##  makeCacheMatrix(), and the result returned.
##

cacheSolve <- function(input, ...) {
    data <- input$getCache()
    if (!is.null(data)) {
        print("Using cached data")
        return(data)
    }
    else {
        print("Inverse and store in cache")
        data <- input$get()
        data <- solve(data)
        input$setCache(data)
        data
    }
}


## EOF