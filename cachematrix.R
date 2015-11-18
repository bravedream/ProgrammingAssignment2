#The file will take an input matrix (assumed to be invertible square matrix) and check 
#if the matrix already exist and have been run to get the invert. If not, return the inverted
matrix; otherwise, retrieve the inverted matrix from cachedmatrix


#the function below will create a matrix object and set the Inv to null as a marker that
#the new object has not been inverted. Set() method will first check if the new input matrix
#is the same as existing (if any) matrix and if yes, don't change Inv, don't reassign values to 
#x, which is the input matrix. If a different matrix input, then reset Inv and x to create
#a new object. The function returns the four sub-functions (set, get, setInv and getInv).
#setInv() is used to cache the inverse matrix from cacheSolve() function.
makeCacheMatrix <- function(x=matrix()) {
	Inv<-NULL
	set <- function(y) {
		if(all(y==x)==FALSE )  {
			x<<-y
			Inv<<-NULL
		}
	}
	get<-function() x
	setInv<-function(cacheInv) Inv<<-cacheInv
	getInv<-function() Inv
	list (
		set = set, 
		get = get,
            setInv = setInv,
            getInv = getInv)
}

#The cacheSolve function below actually do the inverse and caching. It first invoke the 
#getInv() method in makeCachMatrix() function to check if there is any existing Inv matrix. 
#If yes, then put the message and return the cachInv. If not, it means there is a new and 
#different matrix object. The get() function is run and solve function was used to do the 
#"inverse" and assigned to variable cacheInv. Then the setInv method in x matrix object
#was invoked to pass the cacheInv values to x object so that next time, Inv in the makeCachMatrix()
#function is not going to be null..

cacheSolve <-function (x,...) {
	cacheInv<-x$getInv()
	if(!is.null(cacheInv)) {
		message("getting cached inverse matrix")
		return (cacheInv)
	}
	data <-x$get()
	cacheInv<-solve(data,...)
	x$setInv(cacheInv)
	cacheInv
}
