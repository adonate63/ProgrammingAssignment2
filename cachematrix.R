
# Coursera R Programming - Week #3 - Programming Assignment
# Caching the Inverse of a Matrix
# First function that creates a psuedo matrix (a list) and assigns it to - makeCacheMatrix
# Following the makeVector example - initialize the two objects myMatrix (function argument) and myCacheObj

makeCacheMatrix <- function(myMatrix = matrix()) {
  
  # Assign NULL to myCacheObj object
  myCacheObj <- NULL
  set <- function(y) {
    
    # Assigns ( <<- operator) the value on the RHS of the operator to an object in parent environment from LHS object (set); second assignment
    
    myMatrix <<- y
    myCacheObj <<- NULL
    
  }
  
  # defining the data values that will be retrieved by the function call and assign the inverse to myCacheObj in the parent scope
  get <- function() myMatrix
  
  setinverse <- function(inverse) {
    myCacheObj <<- inverse
  }
  
  getinverse <- function() myCacheObj
  
  # Assigns the functions as an element of an list() following makeVector example
  list(set = set, get = get, setinverser = setinverse, getinverse = getinverse)
  
}


#The second function of this program which calculates the inverse of the matrix pulling from the cache if the vlaue has already been calculated

cacheSolve <- function(myMatrix, ...) {
  
  # Will return a matrix that is truly the inverse of myMatrix; the getter will use the extract operator to obtain the contents of the matrix
  myCacheObj <- myMatrix$getinverse()
  
  
  # if calculated data is found in cache then will pull from the cache and write the first message; if not, the program will calculate the data and then return second 
  #   message...
  if(!is.null(myCacheObj))  {
    message("\n -> Data is being pulled from Cache...\n")
    return(myCacheObj)
  } 
  else {
    message("\n -> Data not in Cache - calculating data ...\n")
  }
  
  # the following program statements will either calculate or fetch the data and display the results using the R Solve function.
  dataset <- myMatrix$get()
  myCacheObj <- solve(dataset, ...)
  myMatrix$setinverse(myCacheObj)
  
  myCacheObj
  
}
