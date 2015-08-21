## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(myMatrix = matrix()) {
    globalMatrix <- NULL
    set <- function(newValue) {
        myMatrix <<- newValue
        ##Because the matrix has changed, any cached data is obsolete know
        globalMatrix <<- NULL
    }
    get <- function() myMatrix
    setInversematrix <- function(invertedMatrix) globalMatrix <<- invertedMatrix
    getInverseMatrix <- function() globalMatrix
    setmean <- function(mean) m <<- mean
    getmean <- function() globalMatrix
    list(set = set, get = get,
         setInversematrix = setInversematrix,
         getInverseMatrix = getInverseMatrix)
}

## Write a short comment describing this function
##The function uses the list made by the Cache Matrix, which consists of the functions to get and set
##the original matrix, and the functions to calculate the inverse matrix, and to retrieve it

cacheSolve <- function(myMatrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    globalMatrix <- myMatrix$getInverseMatrix()
    if(!is.null(globalMatrix)) {
        ##globalMatrix exists. But are we sure it is the same?
        ##That means the inversematrix is up-to-date. myMatrix$set NULLs the globalmatrix,
        ##so an update of the matrix will result in a clearance of the cache
        ##Great, so we kan use the cached data
        message("getting cached data")
        return(globalMatrix)   
    } else {
        ##Not really necessary, just to keep track:
        message("calculating data")
    }
    ##at this point, there either is no cached data, or this cached data is obsolete
    ##fetch the data:
    data <- myMatrix$get()
    globalMatrix <- solve(data, ...)
    myMatrix$setInversematrix(globalMatrix)
    ##and return the inversed matrix:
    globalMatrix
    
    ##note: I guess it would be smarter to use myMatrix$getInverseMatrix to calculate and/or cache
    ##the desired inverse matrix. But this is not the assignment, so I keep it like this.
}