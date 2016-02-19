#Create a speical matrix that can cache its inverse
#set matrix
#get matrix
#set matrix inverse
#get matrix inverse

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
                    x <<- y
                    m <<- NULL
          }
          get <- function() x
          setsolve <- function(solve) m <<- solve
          getsolve <- function() m
          list(set = set, get = get,
               setsolve = setsolve,
               getsolve = getsolve)
}

#Return the inverse matrix of x
#Grab matrix inverse from makeCacheMatrix
#check if matrix has previously been calculated and grab inverse if already calculated
cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          
          if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setsolve(m)
          m
}