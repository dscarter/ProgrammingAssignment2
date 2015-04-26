## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list of functions.
##	the functions can set a matrix to store,
##	return that matrix, calculate the inverse of the matrix
##	and also return that matrix inverse
## cacheSolve checks if the inverse has already been calculated
##	and returns it if already done.  If it has not been done
##	then it calculates the inverse and stores it with 
##	makeCacheMatrix
##	


## Write a short comment describing this function
## makeCacheMatrix creates a list of functions to store
## 	a matrix and a cached version of matrix inverse.
##	setMat sets the matrix and clears out the cached inverse
##      getMat returns the cached matrix
##      setinvMat stores an inverse matrix
##      getinvMat returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        setMat <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        getMat <- function() x
        setinvMat <- function(inv) invMat <<- inv
        getinvMat <- function() invMat
        list(setMat=setMat,getMat=getMat
             ,setinvMat=setinvMat,getinvMat=getinvMat)
        
}


## Write a short comment describing this function
## CacheSolve checks to see if the inverse of a matrix has already been
##      solved.  If it has then it returns it.  If it has not then
##      it calculates it and then stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        MatrixInverse <- x$getinvMat()  #set the inverse from the cached matrix 

        if(!is.null(MatrixInverse)) {   
			#if the cached inverse is not empty then return it
                message("getting cached data")
                return(MatrixInverse)
        }

        #inverse was not calculated so do so now
        MatrixInverse <- solve(x$getMat(),...)  
        x$setinvMat(MatrixInverse)
        MatrixInverse
}