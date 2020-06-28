## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){#creates a special matrix
  matrix_inverse <- NULL
  set_matrix <-function(y){ #set value of a matrix
    x<<-y # assigning y, the value of matrix, to x in a different environment
    matrix_inverse <<- NULL
  }
  get_matrix <- function() x #simple function hence no brackets
  set_matrix_inverse <-function(inverse){
    matrix_inverse <<- inverse
  } 
  get_matrix_inverse <- function(){
    matrix_inverse
  }
  list(set_matrix=set_matrix, get_matrix=get_matrix, set_matrix_inverse=set_matrix_inverse,
       get_matrix_inverse=get_matrix_inverse)
  
  
}




## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x,...) { #caching the inverse of the matrix
  matrix_inverse<-x$get_matrix_inverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
    
  }
  matrix_data<-x$get_matrix()
  matrix_inverse<-solve(matrix_data, ...)
  x$set_matrix_inverse(matrix_inverse)
  matrix_inverse
}

        ## Return a matrix that is the inverse of 'x'

