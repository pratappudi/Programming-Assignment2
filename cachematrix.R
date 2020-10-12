## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        ##create the details of the inverse matrix
        m <- NULL
        
        ## Creation of "Set" which aims to regulate the creation of the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## With "get" the matrix is obtained, it makes a copy and returns the matrix "m"
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        
        ## Its function is to return the details of the inverse matrix, by means of "inv"
        getinverse <- function() m
        
        ## returns everything done in (set, get, setInverseMatrix, getInverseMatrix) this through a list.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ##Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## if the "IF" is valid, m returns
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## data is a summary to call "x$get()"
        data <- x$get()
        m <- solve(data, ...)
        
        ## returns
        x$setinverse(m)
        m

}
