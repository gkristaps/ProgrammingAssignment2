> ###Possible solution
> 
> ## this function creates list of 4 elements - set,get,setsolve, getsolve, all of those
> ## 4 elements are functions. 
> ## The first function, makeVector creates a special "vector", which is really a list containing a function to
> ##set the value of the matrix(set)
> ##get the value of the matrix(get)
> ##set the value of the inverse matrix(setsolve)
> ##get the value of the inverse matrix(getsolve)
> 
> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x
+     setsolve <- function(solve) inv <<- solve
+     getsolve <- function() inv
+     list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
+ }
> 
> ## this function uses list of functions, which were made by makeCacheMatrix,
> ## firstly, checks, if there is already calculated inverse matrix, if so, then 
> ## prints message "getting cached data" and inv matrix, if not so, then gets matrix data, solves 
> ## its inverse, writes it down to "inv"(because of checking already calculated inverse matrix)
> ## and prints down the results. 
> 
> cacheSolve <- function(x, ...) {
+     inv <- x$getsolve()
+     if(!is.null(inv)) {
+         message("getting cached data")
+         return(inv)
+     }
+     data <- x$get()
+     inv <- solve(data)
+     x$setsolve(inv)
+     inv
+ }
> 
> ###test
> testmatrix<-matrix(c(1:5,1,2,3,4),3,3)
> x<-makeCacheMatrix(testmatrix)
> cacheSolve(x)
     [,1] [,2] [,3]
[1,] -3.4  2.8 -0.4
[2,] -0.2  0.4 -0.2
[3,]  2.6 -2.2  0.6
> cacheSolve(x)
getting cached data
     [,1] [,2] [,3]
[1,] -3.4  2.8 -0.4
[2,] -0.2  0.4 -0.2
[3,]  2.6 -2.2  0.6
