## SUMMARY: The following functions economically return the inverse of a matrix.
## ASSUMPTIONS:  A square matrix is assumed.
## DETAILED DESCRIPTION: The two functions, makeCacheMatrix and cacheSolve
## allow the user to store a matrix and retrieve a matrix outside the function,
## take the inverse of that matrix, and store and retrieve that.
## Since computing the inverse of a large matrix is costly, 
## it also checks to be sure that the inverse of a matrix has not already
## been computed.  If it has, then it skips computation and pulls 
## the inverse from the cache.  I went beyond the assignment, doing two things
## not asked for: (1) we check to see if the matrix has changed or is identical
## to the one saved and message the user in this case before also messaging them
## that we are getting chached data and (2) I check to see if the matrix
## is able to have an inverse computed by checking the determinant.  If it's 
## equal to 0, no inverse will exist and I message the user accordingly.


## The following function creates a matrix that caches both the 
## initial matrix and it's inverse outside the immediate environment.
## It also allows for recall of these two elements.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setf <- function(y) {
    ## If you see the matrx is identical, don't do any more work
    if (identical(x,y)) {
      message("Matrix is identical to one in cache")
    }
    else {
        x <<- y
        inv <<- NULL
    }
  }
  getf <- function() x
  
  ## inv takes invserse matrix that is passed to it and sets it in cache 
  setinvf <- function(inverse) inv <<- inverse 
  
  getinvf <- function() inv
  
  ## creating a list whose values are determined by the functions, above
  list(set = setf, get = getf,
       setinv = setinvf,
       getinv = getinvf)
}
## The following function retrieves the inverse of the matrix from the 
## cache if the cache has anything in it. If this condition is false, it 
## then determines if an inverse matrix can be computed. If it can, it
## does this and then stores the value in the matrix created in the above 
## function.

cacheSolve <- function(x, ...) {
  ## Code follows professor's model
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()

  ## solve produces inverse of matrix
  ## checks to see if inverse can exist by computing determinant
    if(det(data) == 0) {
    message("No inverse exists for this matrix")
  }
  else {
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
}

