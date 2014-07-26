## Put comments here that give an overall description of what your
  ##... functions do
#below is my makecachematrix function along with its description
#following are the sub functions that i used in the above mentioned functions
#getmat() : which will show you the matrix you create 
  #...using makecachematrix function or setmat function
#setmat(y,y1,y2): you can update a matrix simply by using a vector and
  #...by specifying its dimensions, as in rows and columns 
    #... y is the vector
      #...y1 is the number of row
        #...y2 is the number of columns
#setinv(abc): if you already have the inverse matrix, use setinv function 
  #to assign inv the value of the inverse matrix
    #...abc is the inverse matrix which you already have
#getinv(): this function will give you the inverse matrix, provided the value is set 
  #...using setinv function or cachesolve function
    #otherwise a NULL value is given


## Write a short comment describing this function
#makecachematrix function first creates a matrix using a vector by specifying its dimensions
  #then it assigns NULL to the value of inverse matrix 
    #it also offers an option to set or update a matrix
      #Finally, it will you a list containing the values of all the 4 functions mentioned above

makecachematrix <- function(x = numeric(),nr = integer(),nc=integer()) {
  mat <- matrix(x, nr, nc)
  inv<-NULL   
  setmat <- function(y,y1,y2) {
    x <<- y
    nr<<-y1
    nc<<-y2
    mat <<- matrix(x, nr, nc)
    inv<<-NULL
  }
  getmat <- function() {mat}
  setinv <- function(z) {inv <<- z}
  getinv <- function() {inv}
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#cachesolve function first checks if there is already a value
  #available for inv i.e inverse matrix
    #else it applies the solve function on the matrix found using getmat function
      #this will give you the inv i.e inverse matrix
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
