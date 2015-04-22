# The intention of the following functions is to save computational time by avoiding
# repeated calculations. The result is stored in the cache and then recalled if needed.
# How to use the functions:
#     - set a matrix A <- matrix(data, nrow, ncol, byrow, dimnames)
#     - define the list of functions M <- makeCacheMatrix(A)
#     - set the vector b such that length(b)=nrow(A)
#     - solve the linear system Ax=b, that is x=A^(-1)b, by calling cacheSolve(M, b).
# The result will be provided and possible comments printed.


# The function makeCacheMatrix takes a matrix as the input and defines a list in
# a different environment. This list is composed by four functions: two of them
# set the values of the input and of the result in the cache, and two of them are
# functions that are needed to recall these values.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}

# The function cacheSolve is a substitute of the function solve. Unlike the original
# function, before calculating the result cacheSolve checks whether the result is
# already in the cache; in this case any calculation will be avoided and the result
# will be printed and the information that the result was already in the cache provided.
cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m

}
