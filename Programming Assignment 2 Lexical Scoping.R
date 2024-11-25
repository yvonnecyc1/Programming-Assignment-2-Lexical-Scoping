
##calculate the measn of special "vector"
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

##calculate the mean of data & sets value of mean in the cache via the setmean function
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

##cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##return a matrix y (inverse of x)
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
  message("getting cached data")
  return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  inv
}


