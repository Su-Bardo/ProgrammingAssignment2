## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix assigns NULL to global variable inverse_matrix. 
## Further set, get, setinverse, getinverse functions are defined.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks whether global variable inverse_matrix is NULL. If not so
## it return cached value of inverse_matrix. If inverse_matrix is NULL function
## computes the value of inverse matrix by using solve function.

cacheSolve <- function(x, ...) {
## this part checks whether input argument is square matrix. there is more simple 
## function for check  squareness - is.square.matrix().
## But it is not in base package.
        if (dim(x$get())[1] != dim(x$get())[2]) {
                message("matrix is not square")
                return(invisible(x))
        }
## checks whether matrix has inverse matrix. If determinant of matrix is in range
## near Zero - between -0.1 and 0.1 then this matrix has no inverse matrix.
        if (det(x$get())<0.1 & det(x$get())> -0.1) {
                message("matrix has no inverse matrix")
                return(invisible(x))
        }
        
        
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data,...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
