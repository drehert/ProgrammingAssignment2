## This function creates the special cache Matrix

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
	      set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## This returns the inverse of the matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
		m <- x$getmatrix()
		if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m

        ## Return a matrix that is the inverse of 'x'
}
