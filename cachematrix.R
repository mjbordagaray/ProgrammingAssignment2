
## makeCacheMatrix function initializes x and s objects, and then defines the functions set(), get(), setinverse() and getinverse(). 
## Finally the defined functions are organized in a list. This allows us to use de $ extract operator to call the functions later. 

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL 
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function completes the makeCacheMatrix function. In this function we use the $ extract operator, and finally we proceed 
## to use the solve() function to obtain the inverse matrix. 

cacheSolve <- function(x, ...) {
         s <- x$getinverse()
        if(!is.null(s)) {
                message("cached matrix")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...)
        x$setinverse(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
