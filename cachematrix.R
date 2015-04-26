#**************************************************
#
#              R PROGRAMMING: ASSIGNMENT 2
#
#                    mgarza12
#
#**************************************************

# 0. Description

# The two functions below allow to compute the inverse
# of a matrix --assuming the matrix provided is indeed
# invertible-- and cache it. However, if the inverse
# has already been computed and stored, the functions
# retrieve it without having to compute it again.

#-----------

# 1. Cache matrix

# This function creates a *matrix* object that allows to cache
# its inverse.

makeCacheMatrix <- function(x = matrix())
{
    Inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        Inv <<- NULL
    }
    
    get <- function() {x}
    setinverse <- function(solve) Inv <<- solve
    getinverse <- function() {Inv}
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#-----------

# 2. Cache solve

# This function returns the inverse of a matrix either by computing it
# or by retriving it from memory cache.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getinverse
    
    if(!is.null(Inv))
    {
        message("Getting cache data...")
        return(Inv)
    }
    
    data <- x$get()
    Inv <- solve(data)
    x$setinverse(Inv)
    
    Inv
}
