## The intention behind the two functions is to provide inverse of a given invertible matrix.
## If the inverse has already been previously calculated then it is retrieved from the cache.
## If the inverse has not been previously calculated then a calculation is done and cached.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Input argument x: the invertible matrix to be inverted
        ## return: a list containing functions (set, get, setinverse, getinverse)
        ##         the functions are setters and getters for the original matrix and its inverse
        ##         this list is used as the input to cacheSolve()
        
        inverted.matrix = NULL
        set = function(y) {
                x <<- y
                inverted.matrix <<- NULL
        }
        get = function() x
        setinverse = function(inv.matrix) inverted.matrix <<- inv.matrix 
        getinverse = function() inverted.matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Input argument x: the list produced by makeCacheMatrix
        ## return: inverse of the original matrix inputted in makeCacheMatrix
        
        # retrieve the inverted matrix from the cache
        inverted.matrix <- x$getinverse()
        
        # return the inverted matrix from the cache if already calculated
        if (!is.null(inverted.matrix)){
                message("getting cached data")
                return(inverted.matrix)
        }
        
        # otherwise calculate the inverse and store it in cache 
        inverted.matrix = solve(x$get(), ...)
        x$setinverse(inverted.matrix)
        inverted.matrix
}

# This is a test function to check the above two functions
testInvert <- function() {
	mtx <- matrix(1:4,2,2)
	# cachedList object produced by makeCacheMatrix is used as an input to CacheSolve
	cachedList <- makeCacheMatrix(mtx)
	cacheSolve(cachedList)
# re-run it to retrieve from cache
	cacheSolve(cachedList)
}
