## Below are the two functions used for storing a matrix and caching its inverse. 
## We use caching to prevent computing the inverse matrix every time. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # will assign the inverse of a matrix to it
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) { #check if an inverse of a matrix is assigned in inv
                message("getting cached data")
                return(inv)
        }
        
        # if there is no cached inverse matrix
        mat <- x$get() # get the matrix from object x and assign it to mat
        inv <- solve(mat) # calculate the inverse of the matrix mat, and assign it to inv
        x$setInverse(inv)
        inv
}

# Test with a square matrix created using a random generator
test <- matrix(sample(1:100, 25),5,5)
cat("\nThe generated matrix is\n")
print(test)

testing <- makeCacheMatrix(test)
cat("\nComputing the inverse matrix\n")
print(cacheSolve(testing))
cat("\nPrinting the inverse matrix again")
print(cacheSolve(testing))
cat("\n\nChecking the solution, if we get the [Identity Matrix] by \n[Matrix] x [Inverse of the Matrix]: \n\n")
check <- round(solve(test)%*%test, 10) # check if you get the identity matrix (rounded to the 10th decimal)
print(check)

