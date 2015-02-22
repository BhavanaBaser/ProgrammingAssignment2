# See README.md for instructions on running the code and output from it
# makeCacheMatrix is a function that returns a list of functions
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  	m<-NULL  						# we don't know matrix until it is set

# 'set' method below is used to set new data list instead of the one which was set by creating object
  	set<-function(y){
  	x<<-y   						# we just use unchanged matrix
  	m<<-NULL   						# we set new matrix, to prevent old matrix createed earlier 
}
	get<-function() x   				# This "method" just returns unchanged matrix 
	setmatrix<-function(solve) m<<- solve   	# This "method" cache matrix for future use
	getmatrix<-function() m  			# This "method" returns matrix cached before or NULL if it wasn't set before.
	
#This function returns list of 'methods' with their values

	list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {		#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

    		m<-x$getmatrix()  			# check if matrix is in cache of object x, created with makeCacheMatrix function
   		
# matrix is in cache - load it from there, return value and stop function executing

	if(!is.null(m)){
      	message("getting cached data")
      	return(m)  					# if this string was executed, function returns value and none of the below code will work!
    }

 # All the code below works only if m is null!
    
		matrix<-x$get()   			# get the matrix stored in makeCacheMatrix function object
    		m<-solve(matrix, ...)   		# returns its inverse
    		x$setmatrix(m) 				# write matrix to cache of makeCacheMatrix function object
    		m
}
