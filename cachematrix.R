## The package provides two functions that can be used together
## ensure that the computational expense of calculating the inverse
## of a matrix is only done the first time the inverse value is used.
## Susiquent uses of the inversion will recall the value from a cache
## rather then re run the inversion computation.
## functions do

## The function allows the inversion of a matrix to be stored
## after the value is calculated. If the input matrix is updated
## this function will ensure the the now redundent cached inversion
## is deleted such that a new inversion is executed for the new
## input value.

makeCacheMatrix <- function(this.mat = matrix()) {
        ## This holds the inverted matrix once calculated
        this.inv <- NULL

	  ## This allows the input matrix to be set and ensures
        ## that any stored results from previous input values
        ## are delted.
        set <- function(y) {
                this.mat <<- y
                this.inv <<- NULL
        }
        ## This allows the value of the input to be obtained
        get <- function() this.mat
        ## This stored the inverted matrix once calculated
        setinv <- function(inverse) this.inv <<- inverse
	  ## This allows the inverted matrix to be obtained.
        getinv <- function() this.inv
	  ## This exposes the functions as named methods of this object.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes as input a CacheMatrix and for the first request for
## its inverted form will calculate the result and store that result in the
## input cacheMatrix. For subsiquent calls to get the inverted form the function
## will recover the previous value from cacheMatrix rather than calculate the
## inverted form again.

cacheSolve <- function(cacheMat, ...) {
	  ## Get the inverted form of the input matrix from cache matrix
        ## if the return value is null, this means that there is no cached
        ## result and we have to calculate it and store it back to cacheMatrix.
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

## This is a test function that exercies the functions above to prove they
## behave as required.

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