## Cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #initialise inverse as null
    
    #set the value of the matrix
    set <- function(y){ 
        x <<- y 
        inv <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x 
    
    #set the value of the inverse
    setinv <- function(input)   inv <<- input
    
    #get the value of the inverse
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Assumes matrix is invertible
    
    #get inverse
    inv <- x$getinv()
    
    #if inverse is present return inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #calculate the inverse of matrix in x
    data <- x$get()
    inv <- solve(data, ...)
    
    #set the inverse
    x$setinv(inv)
    
    #return inverse
    inv
}
