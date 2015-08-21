#----------------------------------------------------------------#
#  R Programming - Assignment 2                                  #
#                                                                #
# This function creates a special matrix object that can         #
# cache its inverse                                              #
#                                                                #
#-----------------------------------august, 21, 2015-----C.W.----#

makeCacheMatrix <- function(x = matrix()) {

#initialize an empty matrix
m<-matrix(,nrow=0, ncol=0)

set<-function(y){
  x<<-y
  m<<-matrix(, nrow=0, ncol=0)
}
get<-function()x
setinv<-function(solve) m<<-solve
getinv<-function()m

#create the list that is returned by the function
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#-----------------------------------------------------------------#
# This function tests if the inverse matrix exists in cache       #
# if yes, gives a message and returns the cached inverse matrix   #
# if not, calculates the solv of the matrix                       #
#--------------------------------------------------------C.W.-----#

cacheSolve <- function(x, ...) {

m<-x$getinv()

# tests if the cached inverse matrix is empty
if ((ncol(m)>0) || (nrow(m)>0)){
# if NOT empty, returns the cached inverse matrix 
  message("getting cached inverse matrix")
  return (m)
}
# if the inverse matrix is empty, will calculate 
data<-x$get()

#--  the inverse matrix 

   m<-solve(data)
   x$setinv(m)
   
#--  m is the R object that contains the inverse matrix 
#--  and is the product of the function
   m
}
