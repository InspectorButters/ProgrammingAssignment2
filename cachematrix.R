## makeMatrix first builds a set of functions and then returns these functions
## within a list to the parent environment

## x is initialized as a function argument with a purpose to store information

makeMatrix <- function(x = matrix()) {
	
## m is initialized as an object with a purpose to store information
	
	m <- NULL

## Define the "set" function.  Assigns input argument to x object and  NULL to
## m object.  So if function has previously ran & generated a valid mean in m,
## x will reset and the value of m cached in memory will clear.
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

## Define the "get" function.  Since x is not defined within get(), R retrieves
## it from the parent environment (b/c of lexical scoping).
	
	get <- function() x

## Define 'setter' for inverse m.  As "m" is defined in parent environment and we
## need to access it after setmean() completes, assign the input argument to the
## value "m" in the parent environment.
	
	setinverse <- function(solve) m <<- solve
	
## Define 'getter' for mean m.  Just like the get() function for "x", the func-
## tion below uses lexical scoping to retrieve correct value.
	
	getinverse <- function() m
	
## list() function below names each of the 4 sub-functions and assigns each as 
## an element within a list.
	
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Function below calculates inverse of matrix created w/ the makeMatrix function.

cacheinverse <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

## Below is an example of how code works.  Entered twice to illustrate caching
## (note how "getting cached data" is included in the second call, but not first).

a <- makeMatrix(matrix(c(2,3,5,7,11,13,17,19,23), 3, 3))

>cacheinverse(a)
##             [,1]       [,2]        [,3]
## [1,] -0.07692308 -0.7692308  0.69230769
## [2,] -0.33333333  0.5000000 -0.16666667
## [3,]  0.20512821 -0.1153846 -0.01282051

>cacheinverse(a)
##getting cached data
##             [,1]       [,2]        [,3]
## [1,] -0.07692308 -0.7692308  0.69230769
## [2,] -0.33333333  0.5000000 -0.16666667
## [3,]  0.20512821 -0.1153846 -0.01282051
