## Put comments here that give an overall description of what your
## functions do
## 
##
## for testing I used the data posted by TA Al Warren as follows: '
#m <- matrix(c(-1, -2, 1, 1), 2, 2)
# > x <- makeCacheMatrix(m)
# > x$get()
# [,1] [,2]
# [1,]   -1    1
# [2,]   -2    1
# > inv <- cacheSolve(x)
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
# > inv <- cacheSolve(x)
# getting cached data
# > inv
# [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1
##
##
## Write a short comment describing this function
## The point of this is to take a matrix of numbers (m in the example above) and create a cached version (x in the example above)
##that can be used to cache the inverted (inv in the example above) of the matrix.
## the variable "cachematrx" is used to hold the inverse which is create with the setinverse command. 
## four functions set, get, setinverse, getinverse are created that control the matrix and caching (see example above x$get()). 
##
makeCacheMatrix <- function(x = matrix()) {
        cachemtrx <- NULL
        set <- function(y){
                x <<- y
                cachemtrx <<- NULL
        }
        get <- function() {
                return(x)       
        }
        setinverse <- function(solve) {
                cachemtrx <<- solve
        }
        getinverse <- function() {
                return(cachemtrx)
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## this function is called after you call the other function and set a variable to the output which is an arg to this.
## this either gets cached inverse data for the matrix or will solve and cache depending the value in "cachemtrx" which is in the other function.
## the commands set, get, setinverse, getinverse are used in getting the cached inversion or setting it if it's not cached.
## 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtrx <- x$getinverse()
        if(!is.null(invmtrx)){
                message("getting cached data")
                return(invmtrx)
        }
        datatosolve <- x$get()
        invmtrx <- solve(datatosolve, ...)
        x$setinverse(invmtrx)
        invmtrx
}
