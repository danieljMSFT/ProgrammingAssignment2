## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## x: parameter receiving an inversable matrix
		## returns a list of functions i.e. list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
		##  The key is the use of the operator <<- to assign a value in a different enviroment
		## Technique derived from the example provided
        m = NULL
        set = function(y) {
 
                x <<- y
                m <<- NULL
        }
        get = function() x
        setinverse = function(inverse) m <<- inverse 
        getinverse = function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
		## @x: output of makeCacheMatrix()
		## return: the inverse of the original matrix that was passed to the makeCacheMatrix funtion

        
        invertedMatrix = x$getinverse() ## getthe inverted matrix from cache if it exists
        
        ## Check a cache hit
        if (!is.null(invertedMatrix)){
                ## return from cache
                return(invertedMatrix)
        }
        

        originalMatrix  = x$get() ## no cache hit (never calculated the inverse) so the original matrix with x$get and solve to compute the inverse 
        invertedMatrix = solve(originalMatrix, ...) ## do de actual computation
        

        x$setinverse(invertedMatrix) ## write the cache with the inverted matrix to avoid remake the computation  next time cacheSolve is invoked
        
        return(invertedMatrix)
}


