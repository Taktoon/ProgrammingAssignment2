## Functions allow to store and compute 'matrix, inverted matrix' objects.

## Takes matrix, creates vector object containig four functions to
## set and store matrix and inverted matrix (if already computed)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverted <- function(solve) m <<- solve
        get_inverted <- function() m
        list(set = set, get = get,
             set_inverted = set_inverted,
             get_inverted = get_inverted)

}

## Takes 'makeCacheMatrix' object and returns inverted matrix with checking cache
## for already computed inverted_matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverted(m)
        m
}
