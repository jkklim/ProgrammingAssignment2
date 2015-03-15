
## James lim

## The makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse of the matrix 
## 4. get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL 
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  list( 
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse) 
} 

## The cacheSolve function calculates the inverse of the special "matrix" 
## the special "matrix" which created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinverse function. 

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
       i <- x$getinverse() 
       if(!is.null(i)) { 
           message("getting cached data") 
           return(i) 
       } 
       data <- x$get() 
       i <- solve(data, ...) 
       x$setinverse(i) 
       i 
   } 
   
   ## below shows how it is tested
   ## mdat <- matrix(c(4,12, 6,13), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2"))) 
   ## mdat
##     C.1 C.2
## row1   4  12
## row2   6  13

## c<-makeCacheMatrix(mdat)
## > c$get()
##     C.1 C.2
## row1   4  12
## row2   6  13
## > cacheSolve(c)
##     row1 row2
## C.1 -0.65  0.6
## C.2  0.30 -0.2


