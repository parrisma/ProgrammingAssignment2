## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(this.mat = matrix()) {
        this.inv <- NULL
        set <- function(y) {
                this.mat <<- y
                this.inv <<- NULL
        }
        get <- function() this.mat
        setinv <- function(inverse) this.inv <<- inverse
        getinv <- function() this.inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMat, ...) {
        inv <- cacheMat$getinv()
        if(!is.null(inv)) {
                message("   ***returning cached inversion")
                return(inv)
        }
        mat <- cacheMat$get()
	  message("   *** Solve, as no cached inversion")
        inv <- solve(mat, ...)
        cacheMat$setinv(inv)
        inv
}

## Simple test function.

test <- function()
{
   message("Create test 3x3 matrix")
   testM <- matrix(c(1,2,3,0,1,4,5,6,0),ncol=3,byrow=TRUE)

   message("Create instance of CacheMatrix")
   cm <- makeCacheMatrix(testM)

   message("Solve, first time so will not get from cache")
   sol <- cacheSolve(cm)

   message("Solve, second time so will get from cache")
   sol <- cacheSolve(cm)

   message("Put new matrix into CacheMat so will have to resolve")
   cm <- makeCacheMatrix(sol)
   orig <- cacheSolve(cm)

   message("we passed in the inverse to be inverted again so we should be back at the original")
   return(round(orig) == round(testM))
}