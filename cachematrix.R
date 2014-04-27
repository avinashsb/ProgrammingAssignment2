## makeCacheMatrix creates a special matrix object that can hold its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ##iMatrix is short for inverse Of the matrix
  
        iMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                iMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverseMatrix <- function(invertedMatrix){
                iMatrix <<- invertedMatrix
        }
        
        getInverseMatrix <- function(){
                iMatrix
        }
        
        list(set = set, get = get,setInverseMatrix = setInverseMatrix,getInverseMatrix = getInverseMatrix)
}


## cacheSolve function computes the inverse matrix of the object returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##iM stands for inverted Matrix in this function
        iM <- x$getInverseMatrix()
        if(!is.null(iM)) {
                message("Getting cached inverse of the matrix")
                return(iM)
        }
        data <- x$get()
        iM <- solve(data)
        x$setInverseMatrix(iM)
        iM
}
