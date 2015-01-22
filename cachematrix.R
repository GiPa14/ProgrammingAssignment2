## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following function generates a "matrix" that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        setinv <- function(value) inv <<- value
        getinv <- function() inv
        list(get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the "matrix" returned by
## the above function only if the inverse has not already been calculated
## If it has already been calculated, and there are not been changes,
## it recalls the previous calculated inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return(inv)
}

## The following part is for program testing
## You can notice how the program is slow in
## evaluating cacheSolve(cama)
## If you type again in prompt cacheSolve(cama)
## the program will give you the stored value
## obvously much more fastly
nEl=1000
ma<-matrix( rnorm(nEl*nEl), nEl, nEl)

cama <- makeCacheMatrix(ma)

cacheSolve(cama)