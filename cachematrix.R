###Cacheing inverse of Matrix

#Caches a matrix
makeCacheMatrix <- function(x = matrix()) {
    n<-NULL
    set<-function(y){
        x<<-y
        n<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) n<<-inverse
    getinverse<-function() n
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

#Solves for matrix inverse

cacheSolve <- function(x, ...) {
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...)
    x$setinverse(n)
    n
}
