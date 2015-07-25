## These functions can be used to save time in matrix inversion calculation
## by storing the inverted matrix for future use. 


## The makeCacheMatrix function receives a matrix "m" and creates a corresponding CacheMatrix object
## This object contains two fields: The matrix and the inverted matrix
## and input/output (set/get) functions for both fields
## The inverted matrix is set to NULL until the user sets another value

makeCacheMatrix <- function(x = matrix()) {
  InvertedMatrix <- NULL    # The inverted matrix field, initially set to NULL
  set <- function(y) {      # This sub-function sets the value of the matrix field to "y"
    x <<- y
    InvertedMatrix <<- NULL # Since a new matrix was entered, the inverted matrix field is set to NULL
  }
  get <- function() x       # Return the matrix field
  setInverted <- function(Inv) InvertedMatrix <<- Inv # Set a new value for the inverted matrix field
  getInverted <- function() InvertedMatrix            # Return the inverted matrix field
  list(set = set, get = get,                          # List of all sub-functions
       setInverted = setInverted,
       getInverted = getInverted)
}


## The cacheSolve function recieves a CacheMatrix object and returns the inverted matrix
## The function checks whether the inverted matrix was calculted before
## If it has been calculated, the corresponding field is read from the CacheMatrix object and returned
## Otherwise, the inverted matrix is calculated, saved to the inverted matrix field and returned 

cacheSolve <- function(x, ...) {
  Inv <- x$getInverted()            # Get the value of the inverted matrix field of "m"
  if(!is.null(Inv)) {               # Check if this calculation was performed before
    message("getting cached data")  # If it has been, inform the user
    return(Inv)                     # and return the stored field 
  }
  # Reaching this line means the program has not reached the return command above 
  # and the inverted matrix was not previously calculted
  data <- x$get()                   # Get the matrix field           
  Inv <- solve(data, ...)           # Invert the matrix
  x$setInverted(Inv)                # Store the inverted matrix in "m" for future use
  Inv                               # Display the inverted matrix
}