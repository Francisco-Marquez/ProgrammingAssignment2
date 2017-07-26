## Put comments here that give an overall description of what your
## functions do
#Description:
# - The makeCacheMatrix and cacheSolve functions collectively calculate 
#   and store in the cache the inverse of a squeared matrix.

## Write a short comment describing this function
# - This function allows to establish the value of an matrix and its inverse, 
#   and store it in the cache.
makeCacheMatrix = function(m = matrix())
{
  inv = NULL
  set = function(y)
  {
    m <<- y
    inv <<- NULL
  }
  get = function()
  { m }
  
  setinv = function(inversa)
  { inv <<- inversa }
  getinv = function()
  { inv }
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function
# - This function allows to recover the inversion of a matrix, if it was stored 
#   in the cache by the makeCacheMatrix function. And in case it does not exist 
#   calculate it and return its value.
cacheSolve = function(m, ...)
{
  ## Return a matrix that is the inverse of 'x'
  inv = m$getinv()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data = m$get()
  inv = solve(data)
  m$setinv(inv)
  inv
}
