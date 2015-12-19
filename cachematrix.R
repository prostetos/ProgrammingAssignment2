## With the makeCacheMatrix function, we take an invertible matrix as an input and create 
## a "special" object which stores the matrix and caches the inverse of the matrix if asked. 
## 
## With the cacheSolve function, we calculate the inverse of the matrix created with the previous 
## function, only if it wasn't calculated before. Otherwise, it just cashes the inverse, which 
## is stored in the special object as described previously .


## Inside the makeCacheMatrix function:
## 1)the set function changes the matrix stored in the main function. 
## 2)the setinverse and getinverse functions just store the value of the input in a variable 
##   inverse into the main function makeCacheMatrix(setinverse) and return it (getinverse). 
## It is important to say that we have to caclulate the inverse of a matrix manually, outside 
## the cachematrix function. But from that time and then, the inverse is being cached, as 
## explained below.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Inside the cacheSolve function:
## 1)Verifies if the inverse variable is null or not.
## 2)If it is null, then it calculates 
##   the inverse of the matrix entered as input and stores it to x$getinverse() object. 
## 2)If it's not null, then it simply caches theinverse matrix from the x$getinverse() 
##   object. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
