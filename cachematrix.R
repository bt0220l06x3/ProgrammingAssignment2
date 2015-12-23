##
## These functions provide a cache of matrix inverse calculations results
## from the solve() function.  In order to use the cache the matrix
## shoudl be passed to makeCacheMatrix() and the resulting matrix used
## for further operations.

## makeCacheMatrix(matrix) wraps additional functionality around a matrix
## object in order to store the inverse calculation for quick retrieval.

makeCacheMatrix <- function(input = matrix()) {
    inverseMatrix <- NULL
    set <- function(newInput) {
      input <<- newInput
      inverseMatrix <<- NULL
    }
    get <- function() input
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## calcSolve(input) calculates the inverse of the input matrix using the
## solve() function with the added benefit of storing the calculation result
## for subsequent requsts using the same input matrix.

cacheSolve <- function(input, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- input$getinverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- input$get()
    inverseMatrix <- solve(data, ...)
    input$setinverse(inverseMatrix)
    inverseMatrix
}
