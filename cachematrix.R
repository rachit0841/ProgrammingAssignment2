## function <makeCacheMatrix()> creates a <Matrix> that can
## cache its inverse.
## function <cacheSolve()> computes the inverse of the <matrix>
## returned by <makeCacheMatrix()>. If the inverse has already
## been calculated and there is no change in <Matrix>, it will
## retrieves the inverse of the <Matrix> directly from the cache.


## <makeCacheMatrix()> function:-
##    Argument: <x> is a invertible square matrix
##    Return: a list containing functions to:-
##      a.) Set the matrix <x>
##      b.) Get the matrix <x>
##      c.) Set the inverse of matrix <x>
##      d.) Get the inverse of matrix <x>

makeCacheMatrix <- function(x = matrix()) {
  InvMat = NULL
  SetMat = function(y) {
    x <<- y
    InvMat <<- NULL
    ## Operator `<<-` is used to assign a value to an object
    ## in an environment different from the current environment. 
  }
  GetMat = function() x
  SetMatInv = function(Inverse) InvMat <<- Inverse 
  GetMatInv = function() InvMat
  list(SetMat = SetMat, GetMat = GetMat, SetMatInv = SetMatInv, GetMatInv = GetMatInv)
}


## <cacheSolve()> function:-
##    Argument: <x> is the output of <makeCacheMatrix()> function.
##    Return: The inverse of the original matrix <x> that was input
##    to <makeCacheMatrix()> function.

cacheSolve <- function(x, ...) {
  InvMat = x$GetMatInv()
  if (!is.null(InvMat)){
    ## If the inverse of the matrix <x> has already been calculated
    ## then get it from the cache and skips the computation.
    
    return(InvMat)
  }
  mat.data = x$GetMat()
  InvMat = solve(mat.data, ...)
  ## otherwise, calculates the inverse of matrix <x> using <solve()>
  ## function
  
  x$SetMatInv(InvMat)
  ## sets the value of the inverse of the matrix in the cache using
  ## the <SetMatInv()> function.
  return(InvMat)
}
