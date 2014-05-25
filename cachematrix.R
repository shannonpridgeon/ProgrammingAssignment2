##creates a special "matrix" object that can cache its inverse
##then checks for a cached inverse before doing the computation
##returns the inverse efficiently
makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL        #defining a function variable
        set <- function(y) #defining another function that won't be used
                {          #but allows us to change the function easily?
                x <<- y
                inv <<- NULL
                }
        get <- function() x          ##just returns the input matrix
        setinv <- function(solve) inv <<- solve  ##I don't get this
        getinv <- function() inv  ##callable function for the next step
        list(set = set, get = get,  ##puts 4 functions in a list for later
                       setinv = setinv,
                       getinv = getinv)
}
cacheSolve <- function(x, ...)   ##a new function with the same input
{    
        inv <- x$getinv()           ##extracts the function from the list
        if(!is.null(inv))           ##checks for cached inverse
        {
                message("getting cached data")  ##gets and returns it if 
                return(inv)                     ##possible
        }
        data <- x$get()             ##retrieves the original matrix
        inv <- solve(data, ...)     ##inverts it
        x$setinv(inv)               ##caches the inverse?
        inv                         ##prints the inverse
}
