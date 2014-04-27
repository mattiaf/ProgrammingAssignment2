## This function creates a list that contains a function to
## 1.  set the value of the matrix 
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                               ## initially sets inverse to null
        set <- function(y) {                      ## sets value of the matrix with user-provided matrix() value
                x <<- y
                inv <<- NULL                      ## sets inverse to null (one environment above)
        }
        get <- function() x                        ## gets back the value of the matrix
        setinv <- function(solve) inv <<- solve    ## sets value of inverse
        getinv <- function() inv                   ## gets value of inverse
        list(set = set, get = get,                 ## returns the list with the 4 functions
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of a matrix created with the function above.
## In first place, it checks if a value has already been computed beforehand. In that case
## it gets the inverse value from the cache. If not, it computes the inverse of the matrix
## and stores it in the list created above via the setinv() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {                       ## if there is a cached inverse...
                message("getting cached data")    ## ... no computation needed
                return(inv)                       ## ... return the cached value
        }
        data <- x$get()                           ## if there is no cached value:
        inv <- solve(data, ...)                   ## 1. compute the inverse here
        x$setinv(inv)                             ## 2. set the inverse in the list x
        inv                                       ## 3. return the result
}
