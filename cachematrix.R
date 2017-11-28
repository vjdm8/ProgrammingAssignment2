## Function takes a matrix and caches its inverse

## This function creates a list of functions to be called later

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<- function() x
        setinv<- function(solve) inv<<-solve
        getinv<- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## this function checks for a cached matrix inverse and if there is none it will calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
}



# test cases

#Case 1: unchached initiation
#x <-rbind(c(1, 3), c(2, 1))
#v<-makeCacheMatrix(x)
#cacheSolve(v)

# Case 2: test for cache
#cacheSolve(v)

#Case 3: test for new matrix
#x <-rbind(c(1, 2), c(2, 1))
#v<-makeCacheMatrix(x)
#cacheSolve(v)

