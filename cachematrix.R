## I edited this file in github, because I used my Company Notebook, and it does 
## not allow me to clone theis to git because of the firewall-restrictions
## while I have not other Notebook than the Companies this was the only way to try to pass thiss assignment
## Sorry for that - hope it works - regards Thomas
##
##
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' must be a matrix; otherwise the function returns NA 
        ##  
        if (!class(x) == "matrix") {
                print("Input object is not a matrix")  
                return(NA) 
        } 
        m <- NULL  
        ## The Cache for solve value 
        set <- function(y) {   
                x <<- y   
                m <<- NULL } 
        get <- function() x 
                setsolve <- function(solve) m <<- solve  ## sets the cache 
                getsolve <- function() m     ## gets cached value back 
                        list(set = set, get = get,   
                             setsolve = setsolve,   
                             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 'x' must be created with the makeCacheMatrix function above
        ##
        m <- x$getsolve() ## in m there is the cached data
        if(!is.null(m)) {  ## m was computed with the solve function
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...) ## otherwise the inverse matrix is to be computed 
        x$setsolve(m)   ## and set it in the cache m
}
