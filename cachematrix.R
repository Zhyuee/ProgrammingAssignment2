## This pair of functions cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
## Define the cache object and save it in the environment
## Create a list object which includes set, get, set_i_mat and get_i_mat. 
## One can visit those functions through the list object.

makeCacheMatrix <- function(x = matrix()) {
  imcache <- NULL
  set <- function(y) {
    x <<- y
    imcache <<- NULL
  }
  get <- function() x
  set_i_mat <- function(solve) imcache <<- solve   
  get_i_mat <- function() imcache                  
  list(set = set, get = get,                       
       set_i_mat = set_i_mat,                   
       get_i_mat = get_i_mat) 
}

## This function calculates the inverse matrix of the special "matrix" object created with the makeCacheMatrix function.
## if():checks if the inverse matrix has already been calculated. If so, it retrieves the cached inverse matrixit from
## the cache and print a "getting cached data" message.
## Otherwise, it calculates the inverse matrix of the object and sets the inverse matrix 
## in the cache via the set_i_mat function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  imcache <- x$get_i_mat()
  if(!is.null(imcache)) {
    message("getting cached data")
    return(imcache)
  }
  data <- x$get()
  imcache <- solve(data, ...)
  x$set_i_mat(imcache)
  imcache
}
