
## Below pair of functions would compute the inverse of the matrix and would be cache the result. 
## If the inverse of a matrix is already calculated then it would be retrived from cache.
## We would be using a square matrix (square matrix is the one with same number of rows and columns) 

## Below "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(b){
				x <<- b
                mat <<- NULL 
        }
		
        get <- function() x
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Below "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       mat <- x$getinverse()
        if (!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
	