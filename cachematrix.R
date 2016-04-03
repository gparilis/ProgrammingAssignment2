#caching the inversion of a matrix
#1. set the value of the matrix
#2. get the valyee of the matrix


#a function to cache a matrix, retrieve it, and invert it
makeCacheMatrix <- function(x = numeric()) {
  inv = NULL
  set <- function(y) {   #function called "set" 
    x <<- y              #cache y into an object called "x" in another environment 
    inv <<- NULL         #initialize inverted matrix, to be populated below
  }
  get <- function() x  # get the matrix x from the cached location
  setinv <- function(solve) inv <<- solve # function to invert the matix and cache into "inv", replacing NULL from above
  getinv <- function() inv # get inv from cache
  list(set, get = get,  #list the above functions
       setinv = setinv,
       getinv = getinv)
}

#a function to get the inverted matrix from cache, and if it doesn't exit, to get the original matrix and invert it
cacheSolve <- function(x, ...) {
  inv <- x$getinv()        #get cached inverted matrix
  if(!is.null(inv)) {    #if inverted matrix exists in cache, display this message and return the inverted matrix
    message("getting cached data")
    return(inv)
  }
  data <- X$get()  # get the original matrix 
  inv <- solve(data, ...)  #invert the matrix
  x$setinv(inv)
  inv
}


#test these functions
testmatrix <- matrix(rnorm(25),5,5)  #Create a random matrix
test1<-makeCacheMatrix()
test1$set(matrix(rnorm(25),5,5))
cacheSolve(test1)
