## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates R object.
## The R object is composed from a data set(Matrix) initialy passed as an argument(x) and a value connected to that matrix (ans)
## The R object also contains a list of 4 elemenets that are two pairs of Set/Get functions.
## One pair of functions to set/get the value of the Matrix ('Set' and 'Get')
## Another pair to set/get the value connected to the Matrix.

makeCacheMatrix <- function(x = matrix()) {
        ans <- NULL
        
        ## set a new value to the matrix and make the ans null
        set <- function(y) {            
                x <<- y                 ## set x in the parent environment(makeCacheMatrix environment)
                ans <<- NULL            ## set ans in the parent environment(makeCacheMatrix environment)
        }
        
        get <- function() x             ## return the matirx
        
        setsolve <- function(solve) ans <<- solve ## set ans in the parent environment(makeCacheMatrix environment)
        
        getsolve <- function() ans      ## return the ans
        
        ##      return a list of the 4 functios
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## calculates the inverse of a square matrix value that is passed within a R object created by the makeCacheMatrix function.
##It first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m1 <- x$getsolve() # get the answer 
        if(!is.null(m1)) { #chec if it is null
                message("getting cached data")  #not null - already computed
                return(m1)
        }
        mat <- x$get()   # get the matrix 
        m1 <- solve(mat, ...) # calcaute the inverse
        x$setsolve(m1)        # save the inverese
        m1
}
