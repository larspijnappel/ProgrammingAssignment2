## ===================================================================================
## Calling the makeCacheMatrix function results in an object in the calling environment, which provides the  
## following 'object-oriented style' functionality:
##      - perform a solve() function on a new matrix once
##      - store the outcome (i.e. the inversed matrix) in the makeCacheMatrix object 'm'.
##      - retrieve multiple times the outcome of this calculation by using cached data.
## 
## When a different matrix is passed on, the cached data will be overwritten 
## with the outcome of the solve() calculation.
## ===================================================================================

## makeCacheMatrix creates an 'object-oriented style' object in the calling environment, which will contain
## four 'methods' available in the calling environment: 
##      1. set()
##      2. get()
##      3. setsolve()
##      4. getsolve()
## The variable 'x' is a matrix object passed on from the calling environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               # makeCacheMatrix object 'm' will eventually set by 'method' setsolve()
    set <- function(y) {    # method to set the new matrix object 'x' with the calling environment variable 'y'
        x <<- y             # set the makeCacheMatrix object 'm' with value 'y' (coming from the calling env.)
        m <<- NULL          # makeCacheMatrix object 'm' will eventually set by 'method' setsolve()
    }
    get <- function() x                     # method to get the makeCacheMatrix object 'x' (used by cacheSolve)
    setsolve <- function(solve) m <<- solve # method to set the makeCacheMatrix object 'm' (used by cacheSolve)
    getsolve <- function() m                # method to get the makeCacheMatrix object 'm' (used by cacheSolve)
    
    ## return the 4 'methods' as a list to the calling environment.
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve checks if for the passed on variable 'x' the solve() calculation has already been done.
## If so, then the cached outcome will be returned. 
## Otherwise a new calculation is done and the results are 'cached'.
## The variable 'x' is a makeCacheMatrix object (with its methods).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getsolve()     # cacheSolve-object 'm' is set with makeCacheMatrix-object 'm'
    
    ## IF cacheSolve-object 'm' exists THEN
    ##      quit this function, stating that cached data has been returned is NOT performed.)
    
    if(!is.null(m)) {     
        message("getting cached data...")
        return(m)         # cacheSolve-object 'm' is returned to the calling environment
    }
    
    ## ELSE following steps are performed:
    ##      1. get the matrix-data from the makeCachMatrix environment
    ##      2. perform the solve() function 
    ##      3. pass the outcome back to the makeCacheMatrix-environment
    ##      4. return the function with the value of the cacheSolve-object 'm'
    
    data <- x$get()       # cacheSolve-object 'data' is set with makeCacheMatrix-object 'x'
    m <- solve(data, ...) # cacheSolve-object 'm' is set with outcome solve-function (performed on 'data' object)
    x$setsolve(m)         # updated cacheSolve-object 'm' is passed back to the makeCacheMatrix environment
    m                     # cacheSolve-object 'm' is returned to the calling environment
}


## ===================================================================================
                    ### TEST CASES ###

## Originally provided by Thiago Kurovski
## https://class.coursera.org/rprog-031/forum/thread?thread_id=112#post-468
## ===================================================================================

## Check if creating a cacheMatrix and getting its content works fine
x1 <- matrix(rnorm(1000000), nrow = 1000)   # matrix 1
x2 <- matrix(rnorm(1000000), nrow = 1000)   # matrix 2
cm <- makeCacheMatrix( x1 )
identical( cm$get(), x1 )   ## TRUE
identical( cm$get(), x2 )   ## FALSE

## Check if cacheSolve gives the same result for the same matrix and if it truly uses caching
print(system.time( y1 <- cacheSolve(cm) ))  # 1st time calling cacheSolve: requires some calculation time
print(system.time( y2 <- cacheSolve(cm) ))  # 2nd time: same output but stating that cached data is used
identical( y1, y2 ) # TRUE
identical( y1, x2 ) # FALSE

## Check if cacheSolve gives the same result as solve()
z <- solve( x1 )
identical( y1, z )  # TRUE

## Check if updating the matrix with set works correctly
x3 <- matrix(rnorm(100), nrow = 10)
cm$set( x3 )
identical( cm$get(), x3 )    # TRUE

## Check if the cache is unvalidated after a set()
y3 <- cacheSolve( cm )
z <- solve( x3 )
identical( y3 , z ) # TRUE
