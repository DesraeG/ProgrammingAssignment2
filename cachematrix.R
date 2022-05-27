##making a function to compute the inverse of a matrix and cache it using 
##lexical scoping, <<- operator
makeCacheMatrix <- function(x = matrix()){#define function with matrix type
  inv<-NULL##initialize the variable inv 
  set<-function(y){#define set function to set the value of the matrix
    x<<-y#caching to parent environment, Makecachematrix environ, with <<-
     }
  get <-function() x ##define get function getting the value of the matrix
  setinverse <- function(inverse) inv <<- inverse  ##putting in parent environ,Makecachematrix,
  ##define set (mutate) and get (access) function for the value of inverse matrix
  getinverse <- function() inv
  ##below builds a set of functions and returns the functions within a list 
  ##to the parent environment, so we can use $ extract operator later
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##The function below uses solve to make an inverse
##if it already exists it gets it and returns it 
##with a message "getting cached data.
cachesolve<-function(x,...){
  inv<-x$getinverse()## x from getinverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
##sample runs
x<-rbind(c(1,2),c(3,2))
m<-makeCacheMatrix(x)
m$get()
cachesolve(m)##cache for second run
##   [,1] [,2]
##[1,]    1    2
##[2,]    3    2
##> cachesolve(m)inverse matrix
##[,1]  [,2]
##[1,] -0.50  0.50
##[2,]  0.75 -0.25
##> cachesolve(m)
##getting cached data
##[,1]  [,2]
##[1,] -0.50  0.50
##[2,]  0.75 -0.25
> 