
makecachematrix <- function(x = numeric(),nr = integer(),nc=integer()) {
# create a matrix using a vector by specifying its dimensions
    mat <- matrix(x, nr, nc)
#assigning NULL to the value of inverse matrix  
    inv<-NULL
#set or update the matrix using  setmat function 
#and use <<- operator to modify an existing variable by walking up the parent environments
setmat <- function(y,y1,y2) {
    x <<- y
    nr<<-y1
    nc<<-y2
    mat <<- matrix(x, nr, nc)
    inv<<-NULL
  }
#use getmat function to see the matrix creates using cachematrix function or setmat function
getmat <- function() {mat}
# use setinv function if you already have the inverse of the matrix
setinv <- function(z) {inv <<- z}
#use getinv function to see the inverse matrix  
getinv <- function() {inv}
#create a list and assign the values calculated above to the list  
list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

#create a cachesolve function to see the value of the inverse matrix
cachesolve <- function(x, ...) {
  inv <- x$getinv()
# if there is a value available, show it
# else calculate such value using getmat and solve functions
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else {data <- x$getmat()
  inv <- solve(data)
  x$setinv(inv)
  inv
}}
