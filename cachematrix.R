## MakeCacheMatrix makes a special matrix object and caches the Inverse of the matrix. 
## CacheSolve computes the inverse of matrix it gets from MakeCacheMatrix. It checks if the Inverse value already present
## makecacheMatrix by getting the inverse with getInv() ,if not Null returns the cache Inverse
## else it computes the inverse with solve() and sets the inverse in MakeCaheMatrix with setInv()



makeCacheMatrix <- function(x = matrix()) {
          Inv<-NULL
        
        set<- function(y)
        {
                x<<-y
                Inv<<-NULL
        }
        
        get<-function() x
        setInv<- function(Inverse) Inv<<-Inverse
        getInv<- function() Inv
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      Inv<-x$getInv()
        if(!is.null(Inv))
        {
                message("Getting Cached Data")
                return(Inv)
        }
        
        data<-x$get()
        Inv<-solve(data,...)
        x$setInv(Inv)
        Inv
}
