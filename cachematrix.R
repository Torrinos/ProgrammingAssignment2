## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix() prepares a list (similar to object) with four methods:
#to set/get matrix to invert 
#to set/get the resulting inverted matrix

#cacheSolve() finds an inverted matrix. Input should be the list prepared by function
#described above. It returns the inverted value. If matrix hasn't changed
#it will return the cached value immediately

#test() is a unit test to verify implementation
#it creates a random matrix and verifies that result provided by
#custom function is equal to result of solve()
#it also checks the execution time of cached function - should be ~zero

## Write a short comment describing this function
# Prepare a special list containing four elements:
# 1: Method set() to set matrix to invert
# 2: Method get() to get matrix
# 3: Method setinverse() to set the cached inverted value
# 4: Method getinverse() to get cached value
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
#1: get inverse from "prepared" matrix, if not null, then return cached value
#2: otherwise get matrix to invert
#3: solve matrix (assume it's solvable)
#4: save inverse value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##TEST implementation
#INPUT: any seed
#OUTPUT: "not matching" if wrong method, un-cached and cached system.times
#cached should be zero
test <-function (seed = numeric()) {
  set.seed(seed)
  x <- matrix(rnorm(1e3*1e3, 1), ncol = 1000, nrow =1000)
  t <- makeCacheMatrix(x)
  benchmark <- solve(x)
  testing <-cacheSolve(t)
  time1<-system.time(solve(x))
  time2<-system.time(cacheSolve(t))
  if (!identical(benchmark,testing)) message("Not matching!")
  message("Not cached duration:")
  print(time1)
  message("Cached duration:")
  print(time2)
}