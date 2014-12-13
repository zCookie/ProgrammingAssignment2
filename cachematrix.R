## Functions
##  makeCacheMatrix - Create a CacheMatrix object that 
##                    the inverse of the matrix is cacheable.
##  cacheSolve      - Retrieve the inverse of a matrix 
##                    from CacheMatrix object.
##

## Create a CacheMatrix object that the inverse of the matrix is cacheable.
makeCacheMatrix <- function(...) {
    mtrx <- matrix(...)
    ivm <- NULL
    getmtrx <- function() mtrx
    setivm <- function(y) {
        ivm <<- y
    }
    getivm <- function() ivm
    print("CacheMatrix Object created, try cacheSolve now.")
    list(getmtrx = getmtrx, setivm = setivm, getivm = getivm)
}

## Retrieve the inverse of a matrix from CacheMatrix object.
cacheSolve <- function(x, ...){
    ivm <- x$getivm()
    if(is.null(ivm)) {
        print("Generating&Caching the inverse of the matrix...")
        mtrx <- x$getmtrx()
        ivm <- solve(mtrx, ...)
        x$setivm(ivm)
    }
    ivm
}