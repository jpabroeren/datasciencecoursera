## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function accepts a Matrix as parameter, and creates with it, a new matrix-like list.
## This list consists of the original matrix, a couple of functions, and an inverse matrix
## These functions make it possible to check whether the original matrix hasn't changed. 
## If not, and the inverse matrix was already created, the 'cached' inverse matrix is returned
## If the original matrix has changed, or the inverse matrix wasn't created yet, the inverse matrix is generated.

makeCacheMatrix <- function(myMatrix = matrix()) {
    ##Reset:
    globalMatrix <- NULL
    ##Define the set-function
    set <- function(newValue) {
        ##Create a new matrix in our 'parent'
        myMatrix <<- newValue
        ##Because the matrix has changed, any cached data is obsolete know
        globalMatrix <<- NULL
    }
    ##Define the get-function (only returns the desired matrix)
    get <- function() myMatrix
    ##setInversematrix organises the 'storing' of the invertedmatrix
    setInversematrix <- function(invertedMatrix) globalMatrix <<- invertedMatrix
    ##the getInverseMatrix simply returns the inversedMatrix
    getInverseMatrix <- function() globalMatrix
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
        ##Great, so we can use the cached data
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