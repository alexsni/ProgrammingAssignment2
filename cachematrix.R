## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function chaches original and inverted matrixes
## original matrix stored by set() function and 
## retrieved by get() function; inverted matrix
## stored by setinvert(), and retrieved by getinvert()

makeCacheMatrix <- function(x = matrix()) {
    ##Caches matrix provided as an argument
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinvert <- function(inv) i <<- inv
    getinvert <- function() i
    list(set = set, setinvert = setinvert, 
         get = get, getinvert = getinvert)
}


## Write a short comment describing this function
## Return matrix inverted to one provided to above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## first check if we have inverted matrix stored already
    ## and return immidiatly if it is
    i <- x$getinvert()
    if (!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    ## if inverted matrix is not stored, retrieve orginal matrix
    data <- x$get()
    ## do the inversion based on original matris
    i <- solve(data)
    ## cache inverted matrix
    x$setinvert(i)
    ## return
    i
}
