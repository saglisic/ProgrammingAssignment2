# Manual:
# 1. Make random matrix
# > x <- matrix(rnorm(9), nrow = 3)
# 2. make inverse cash matrix
# > invx <- makeCacheMatrix(x)
# > invx$get() // Return the matrix
# > cacheSolve(invx) // Return the inverse



# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix 
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # inv will store the cached inverse matrix
        minverse <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse
        setinverse  <- function(inverse) minverse <<- inverse
        
        # Getter for the inverse
        getinverse  <- function() minverse
        
        # Return the matrix with our newly defined functions
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
            minverse <- x$getinverse()
            
            # If the inverse is already calculated
            if (!is.null(minverse)) {
                  message("getting cached data")
                  return(minverse)
            }
            
            # The inverse is not yet calculated
            data <- x$get()
            minverse <- solve(data, ...)
            
            # Cache the inverse
            x$setinv(minverse)
            
            # Print inverse
            minverse
}
