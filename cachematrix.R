## The two functions below enable the user to store in a cache the inverse
## of their matrix
## We assume that the matrix supplied is always invertible.

## The first function creates a list of 4 functions : "set", "get", "setsolve" and "getsolve" 
## While "set" and "get" respectively store and reach the initial matrix, 
## "setsolve" and "getsolve" respectively store and reach the inverse matrix.
## The "s" variable stores the inverse matrix that is obtained through the "solve" function.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  } 
  get <- function () x
  setsolve <- function(solve) s <<- solve 
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This 2nd function is written in 3 steps : 
## 1/ reach the value of s in the cache.
## 2/ check if s is not empty. If it has a value, the function returns the cached value.
## 3/ if s is empty, we neeed to calculate the new value of the inverse and cache it.
## On line 40 I added the variable new_s so I could better visualize the new value of s before caching it.

cacheSolve <- function(x, ...) { 	
  
  s <- x$getsolve()

  if (!is.null(s)) {
    print ("getting cached data")
    return (s)
  }

  else {
    data <- x$get()
    print("We have a new matrix. Let's compute its inverse matrix")
    new_s <- solve(data,...)
    print(new_s)
    x$setsolve(new_s)
    print("Our matrix has been cached for future requests")
  }
}

## Thanks for the review 