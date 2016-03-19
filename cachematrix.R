## This first function will be similar to the example presented in the assignment prompt.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
## x will serve as the matrix object that will be used by the function
        m <- NULL
        set <- function(y) {
                x <<- y 
				## using <<- will assign y, which is the matrix input, to x in 
				## the parent environment
                m <<- NULL
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) m <<- inverse ## this line makes it so
		## that the cache found in m is assigned the inverse of the matrix
		## assigned to x
        getinverse <- function() m ## this function gives the inverse we just assigned to the cache in m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function calculates the inverse of the matrix created in the 
## first function, so long as it hasn't been evaluated previously. If not, it ## will run the computations to solve it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}
