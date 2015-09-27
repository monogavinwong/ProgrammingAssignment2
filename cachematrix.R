## The two functions work together to increse the efficiency of 
## the matrix inversion computation.
## Due to the "expensive" hardwork to compute the invesion of matrix, 
## it is much more economic to build a programme to first check whether
## the current computation has been made or not; if so, just use the original
## value and no repeated computation is needed, if not, a new computation will be 
## made and the value will be cached.




## The following function creates a special "matrix", which is in effect a list
## containing a function to set the value of the matrix, get the value of the 
## matrix, compute and set the value of the invese matrix, and get the value of 
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_mtx <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv_mtx <<- NULL
        }
        get <- function() x
        set_inverse_matrix <- function(inverse_matrix) inv_mtx <<- inverse_matrix
        get_inverse_matrix <- function() inv_mtx
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$get_inverse_matrix()
        if(!is.null(inv_mtx)) {
                message("getting cached matrix inverse")
                return(inv_mtx)
        }
        data <- x$get()
        inv_mtx <- solve(data, ...)
        x$set_inverse_matrix(inv_mtx)
        inv_mtx
}
