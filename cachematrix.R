###############################################################################
# Coursera: R Programming (Aug 15 - Sept 18, 2016)                            #
# Week 1: Programming Assignment 2 - Lexical Scoping                          #
###############################################################################

## Implementation of a matrix with cached inverse

## Requests for the inverse of the matrix return a
## cached copy of inverse, thus avoiding redundant,
## computationally expensive inversion operations.


## Constructor for a "cache matrix"
## Inputs:
## X is assumed to be a non-singular matrix
## Return:
## list of function for manipulating "cache matrix"
makeCacheMatrix <- function( x = matrix() ) {

  inv <- NULL

  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x

  setInverse <- function( inverse ) inv <<- inverse

  getInverse <- function() inv

  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )

}


## Compute the inverse of a "cache matrix"
## Inputs:
## X is a valid "makeCacheMatrix" contructed using makeCacheMatrix
## ... additonal arguments passed to solve if inverse has not been
##  computed, otherwise ignored
## Return:
## inverse of matrix
cacheSolve <- function( x, ... ) {

  m <- x$getInverse()

  if ( !is.null( m ) ) {
    message( "getting cached matrix inverse" )
    return( m )
  }

  mat <- x$get( )

  inv <- solve( mat, ... )

  x$setInverse( inv )

  inv

}
