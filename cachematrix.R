## Assignment 2, caching the mean of a vector and caching
## the inverse of a matrix(assume always invertible)

## "makeCacheMatrix" creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  setmat <- function(y){
          x <<- y
          s <<- NULL }
  getmat <- function() x
  getinv <- function() s
  setinv <- function(solve) s <<- solve
  list(setmat = setmat,getmat = getmat, getinv = getinv, setinv = setinv)
  
}


## "cacheSolve" computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)){
    message("getting inversed matrix")
    return(s)}
  data <- x$getmat()
  s <- solve(data,...)
  x$setinv
  s
        ## Return a matrix that is the inverse of 'x'
}


## "cachemean" ckeck to see if the mean of the special 
## "vector" created with the above function has already
## been calculated, and if not, calculate it.

cachemean <- function(x,...){
      m <- x$getmean()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- mean(data,...)
      x$setmean(m)
      m
}

## "makeVector" creates a special "vector", which is really
## a list containing a function to

## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the mean
## 4.  get the value of the mean

makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y){ 
        x<<- y
        m<<- NULL }
      get <- function() x
      setmean <- function(mean) m <<- mean
      getmean <- function() m
      list(set = set, get = get, setmean = setmean, getmean = getmean)
}
