##Hello fellow Coursera student/peer Marker! 
##makeCacheMatrix accepts matrix 'x' as its argument (input)
##during operation it uses solve() to calculate the inverse of x
##note use of <<- double arrow to manipulate values from parent environment

##Note to other visitors these functions are a rehash of the 'Caching the mean of a vector' 
##example provided at
##https://class.coursera.org/rprog-030/human_grading/view/courses/975104/assessments/3/submissions

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## cacheSolve provides the clever functionality by checking to see if the inverse of x 
## already exists
## If the inverse is already set (tested by checking if x$getinverse() is not null) it will 
## simply return the value of x$getinverse(), and in doing so exiting the function at the point.
## If the inverse value is null the inverse value is solved 
## which is more computationally expensive than simply returning the existing inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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



##EXTRA NOTES 

## Following sections are for testing the above functions
## Create a simple square matrix populated with rnorm values
## 3000 x 3000 rows seems to take about 2 minutes on my laptop to invert a matrix of this size
## y<-matrix(rnorm(9000000),3000)
## x<-makeCacheMatrix(y)
## cacheSolve(x)
## The first time cacheSolve(x) is called, it will take much longer. 
## Subsequent runs will return "getting cached data" message and be much quicker

## Following is just to explore output and ensure inversion worked
## based on knowing a matrix multiplied by its inversion should equate to 1
## check_multiply<-y%*%cacheSolve(x)
## round(check_multiply,1)

