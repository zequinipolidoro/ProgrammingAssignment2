## This function will enable to cache inverse matrix calculations into a vector that may be got by the second function cachesolve, just avoiding unnecessary recalculations. 
## Defines the main function makeCacheMatrix. It takes one optional argument x, which is a matrix if no input is provided. 
makeCacheMatrix <- function(x = matrix()) {
# Calculated inverse matrix is defined as a new variable inv.
        inv <- NULL
# setter function redefines the vector x in the parent environment. It also resets inv variable to null for updated calculation.
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
# getter function simply returns updated x value.
        get <- function() x
# setinverse function will take a calculated inverse matrix and will assign it to inv variable.
        setinverse <- function(inverse) inv <- inverse
# getinverse function will take what has been stored in inv variable.
        getinverse <- function() inv
#final list enables that the designed functions will be able to interact with $ operator.
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)



## cacheSolve function is designed to get the cached inverse matrix calculated by makeCacheMatrix function
## ellipsis was added to enable optional/additional arguments to the function, besides object x (ellipsis was also added to solve() function too)
cacheSolve <- function(x, ...) {
## function then starts by calling the getinverse method from makeCacheMatrix function on the object x, i.e. current cached inverse matrix will then be assigned 
## to inv variable. If inv is not null, then a message "Getting cached data" will appear and the calculated value will be returned. Otherwise, the function will 
## skip the if block.
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        }

## get function will get the values inside the matrix x, which will be saved into a new variable (inversedata) 
        inversedata <- x$get()
## solve() function, from base R package, are then called to calculate inverse matrix based on the inversedata values. 
## The calculated inverse matrix is then assigned to a variable inv.
        inv <- solve(inversedata, ...)
#setinversefunction will then set this calculated inverse matrix to inv variable, saving it to cache.
        x$setinverse(inv)
#inv value (inverse matrix) is then returned
        inv
