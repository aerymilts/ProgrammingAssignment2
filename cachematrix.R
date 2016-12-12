## This function caches the matrix using getter (get and getsolve) and setter (set and setsolve) functions.

makeCacheMatrix <- function(x = matrix()) {
## Initialize m as a variable with NULL value
m <- NULL
## Set x to be the argument of set and m to be NULL in the parent environment (makeCacheMatrix)
set <- function(y) {
        x <<- y
        m <<- NULL
}
## Calls the variable x when get() is executed
get <- function() x
## Sets m to be the argument of solved matrix in 
setinv <- function(solvedmatrix) m <<- solvedmatrix
## Calls the variable m when getinv() is executed
getinv <- function() m
## Stores the functions in a list that can be called in the second function
list(set = set, get = get, 
     setinv = setinv, getinv = getinv)
}


## This function attempts to search for a cached solution in its argument (works in conjunction with makeCacheMatrix),
## otherwise, solve for the matrix and store it in m

cacheSolve <- function(x, ...) {
## Get the cached solution using the get function (getinv)
        m <- x$getinv()
## If the solution is not NULL, return the value and end the function.
        if(!is.null(m)){
                message("Getting cached solution")
                return(m)
        }
## Get the matrix to be solved using the get function (get)
        mymatrix <- x$get()
## Return a matrix that is the inverse of 'x'
        m <- solve(mymatrix, ...)
## Store the solution using the set function (setinv) in m in makeCacheMatrix environment
        x$setinv(m)
        m
}
