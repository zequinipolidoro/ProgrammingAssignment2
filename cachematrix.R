```{r}
## Defines the main function makeCacheMatrix. It takes one optional argument x, which is a matrix if no input is provided. 

# Calculated inverse matrix is defined as a new variable inv.

# setter function redefines the vector x in the parent environment. It also resets inv variable to null for updated calculation.

# getter function simply returns updated x value.

# setinverse function will take a calculated inverse matrix and will assign it to inv variable.

# getinverse function will take what has been stored in inv variable.

#final list enables that the designed functions will be able to interact with $ operator.

## This function will enable to cache inverse matrix calculations into a vector that may be got by the second function cachesolve, just avoiding unnecessary recalculations. 
 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    }
```

```{r}
## cacheSolve function is designed to get the cached inverse matrix calculated by makeCacheMatrix function

## ellipsis was added to enable optional/additional arguments to the function, besides object x (ellipsis was also added to solve() function too)

## function then starts by calling the getinverse method from makeCacheMatrix function on the object x, i.e. current cached inverse matrix will then be assigned to inv variable. If inv is not null, then a message "Getting cached data" will appear and the calculated value will be returned. Otherwise, the function will skip the if block.

#get function will get the values inside the matrix x, which will be saved into a new variable (inversedata)

#solve() function, from base R package, are then called to calculate inverse matrix based on the inversedata values. The calculated inverse matrix is then assigned to a variable inv.

#setinversefunction will then set this calculated inverse matrix to inv variable, saving it to cache.

#inv value (inverse matrix) is then returned

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("Getting cached data")
            return(inv)
        }
        inversedata <- x$get()
        inv <- solve(inversedata, ...)
        x$setinverse(inv)
        inv
}
```
