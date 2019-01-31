## Put comments here that give an overall description of what your
## functions do
## Answer: The first function (makeCacheMatrix) creates an R object that stores a matrix and its inverse. 
## The second function (cachemean) requires an argument that is returned by makeCacheMtrix() in order to retrieve the inverse from the cached value that is stored in the makeCacheMatrix() object's environment.


makeCacheMatrix <- function(x = matrix()) {
        i = matrix()
        i <- NULL
        set <- function(y = matrix()) {
               
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                     setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
   
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        if(det(data)==0){
                message("The matrix is singular and its inverse doesn't exist.")
        }
        else{
        i <- solve(data)
        x$setinverse(i)
        i}
}