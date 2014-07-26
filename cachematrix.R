## Computing the inverse of a matrix use a lot of computation.  
## These funtions allow the saving of the matrix and its inverse so that if
## the inverse has already been calulated it can be retireved else calculated
## and stored in case it needs to be done again 

#------------------------------------------
##  This function creates a list of functions that stores a single square matrix
##  passed in as the only parameter and it returns a list of functions 
##  1)  get()  returns the Saved Matrix(SM) (or NULL if there is'nt one)
##  2)  set(m)  replaces the SM with the paramter m
##  3)  setInverse - calculates and stores the inverse of the SM
##  4)  getInverse - return the (previously saved) inverse of the SM
#------------------------------------------
# save a square matrix and the inverse mean for future use
makeCacheMatrix <- function(sm = matrix()) {
	ism <- NULL
	set <- function(y) {
		sm <<- y
		ism <<- NULL
	}
	get <- function() sm
	setinv <- function(newism) ism <<- newism
	getinv <- function() ism
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## Return a matrix that is the inverse of the matrix stored in a 
## makeCacheMatrix()  
## do this by solving if required but then save the results so you
## can return them if request without resolving
## 
## to use this:
#  -- create a list of functions on the matrix using x<-makeCacheMatrix(squareMatrix)
#  -- pass results of the previous step to this funtion cacheSolve(x)
# ------------------------------------------------------------------
cacheSolve <- function(mcm, ...) {
	inv<-mcm$getinv()
	if (!is.null(inv)) {
		message("returning the stored inverse matrix")
		return(inv)
	}
	inv<-solve(mcm$get())
	mcm$setinv(inv)
	inv
}


#-----------  what follows is  some code to run and test the functions above
#-----  the following runs the program
#  setwd("~/a/highEd/Rprograming/ProgrammingAssignment2")
#  cachematrix(myMatrix)


## build a square matrix for testing
testMatrix <- function(x) {
	tm <- c(21,51,91)
	tm <- rbind(tm, c(52, 102, 22))
	tm <- rbind(tm, c(33, 13, 83))
}

## code to test the makeCacheMatrix()
showme<- function() {
	tm<-testMatrix()
	cm<-makeCacheMatrix(tm)
	cm$get()
	cm$setinv(solve(cm$get()))
	cm$getinv()
}

## code to test the cacheSolve() - pass this a square matrix it will request
## the inverse 2 times. The expected results are the inverse matrix
## caluclated the 1 time and returned without calculation the 2nd time.
tryme<- function(tm) {
	cm<-makeCacheMatrix(tm)
	cacheSolve(cm)
	cacheSolve(cm)
}
