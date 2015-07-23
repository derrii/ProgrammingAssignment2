## Extended matrix class, with matrix inverse caching capability

## Function which creates cached matrix object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function which returns an inverse of a matrix, it calculates it if necessary, otherwise it returns a cached data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

cMat <- makeCacheMatrix(matrix(c(6,-3,6,2,9,4,1,4,3),3,3))

print(cMat)

print(cacheSolve(cMat))

print(cacheSolve(cMat))
