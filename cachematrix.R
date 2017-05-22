## These functions inverse a Matrix in R.

## This function creates a matric that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {  
        m <- NULL  
        set <- function(y) {    
                x <<- y    
                m <<- NULL  
        }
        get <- function() x  
        setinverse <- function(inverse) m <<- inverse  
        getinverse <- function() m  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the above matrix. If it has already been calculated,
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x) {  
        m <- x$getinverse()  
        if(!is.null(m)) {    
                message("getting inversed cached data")    
                return(m)  
        }  
        data <- x$get()  
        m <- solve(data)  
        x$setinverse(m)  
        m
}
        
