## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##TEST matrices
##Test1 <- matrix(c(1,2,3,2,54,1,7,9,1), nrow=3,ncol=3)
##Test2 <- matrix(c(1,1,1,1,1,1,1,1,1), nrow=3,ncol=3)

##function makeCacheMatrix creates special object with inversed matrix inside the object environment
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##test objects
##CacheTest1 <- makeCacheMatrix(Test1)
##CacheTest2 <- makeCacheMatrix(Test2)

## Write a short comment describing this function

##function looks if the matrix has already been inversed. If no then function inverse the matrix and put the result into the object.
##also function checks if the matrix's determinant is not zero, what means that the original matrix is invertible
cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     
     if(!is.null(inverse)) {
          message("getting cached inverse matrix")
          return(inverse)
     }
     data <- x$get()
     if(det(data) == 0){
          message("matrix can not be inversed")
          x$setinverse("can not be inversed")
          inverse <- x$getinverse()
          #return()
     } else {
          inverse <- solve(data, ...)
          x$setinverse(inverse)
     }
     inverse
     ## Return a matrix which is the inverse of 'x'
}

##Solved1<-cacheSolve(CacheTest1)
##Solved2<-cacheSolve(CacheTest2)
