## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix() is a function that creates alist containing four functions:
# 1. set() to set the value of the matrix
# 2. get() to grt the value of the matrix
# 3. setinvers() to set the value of the inverse of the matrix
# 4. getinvers() to get the value of the inverse of the matrix

# "cacheSolve" is a function that calculates 
# the inverse of the matrix created with "makeCacheMatrix". 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix by slove() function
# and sets the value of the matrix in the cache via serinvers().

###############################################################
## Write a short comment describing this function

# makeCacheMatrix is a function that creates alist containing four functions:
# 1. set(), get(), setinvers() and getinvers() 
makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvers <- function(inv_local) invm <<- inv_local
  getinvers <- function() invm
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}


## Write a short comment describing this function
# "cacheSolve" chec whether the inverse have already been calculated before it 
# calculates the inverse of the matrix created with "makeCacheMatrix". 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvers()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvers(invm)
  invm
}

###test matrices for programming##################
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

amatrix <- makeCacheMatrix(m1)
amatrix$get()        # retrieve the value of x
amatrix$getinvers()  # retrieve the value of invm, which should be NULL
cacheSolve(amatrix)  # calculate the inverse of m1
amatrix$getinvers()  # get the nverse of m1

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

amatrix$set(n1)       # set x = n1
cacheSolve(amatrix)   #  calculate the inverse of n1
amatrix$getinvers()
cacheSolve(amatrix) 

cacheSolve(amatrix)   # the inverse of n1 "getting cached data"
