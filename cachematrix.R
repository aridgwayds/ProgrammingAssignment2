#R-Programming
#Programming Assignment 2
#06MAY2018
#A.Ridgway

#Caching the Inverse of a Matrix
#
#The following functions compute the inverse of an invertible matrix(x) and
#cache the result.
#
#To execute:
# 1. load the makeCacheMatrix and cacheSolve functions: source("cachematrix.R")
# 2. load/create invertible matrix, eg: mx<-matrix(rnorm(16), nrow=4)
#       Note:   To test for invertible matrix, check the determinant (det) of the 
#               input matrix, it should be >0. For example:
#                       det(mx)>0
# 3. create the "special" cache matrix, eg: ma<-makeCacheMatrix(mx)
# 4. execute cacheSolve to return the inverse of the matrix, eg:
#       cacheSolve(ma)
# 5. run cacheSolve again for the same matrix, should see message: 
#       "getting cached inverse"

#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache the inverse of
#a given matrix object "x", this function creates a matrix of functions
#which are used in setting and getting objects in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mi<<- inverse
        getInverse<-function() mi
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}      
        
        
##cacheSolve: 
#This function computes the inverse of the special "matrix" returned by the
#makeCacheMatrix function. If the inverse has already been calculated
#(and the matrix has not changed), then this function retrieves the inverse
#from the cache.
cacheSolve <- function(x, ...) {
        mi<-x$getInverse()
        if(!is.null(mi)){
                print("getting cached inverse")
                return(mi)
        }
        data<-x$get()
        mi<- solve(data, ...)
        x$setInverse(mi)
        mi
}




