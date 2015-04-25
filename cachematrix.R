## Below two functions that are used to create a special object that stores a Matrix and cache's its inverese


## The makeCacheMatrix function creates R object.
## The R object is composed from a data set(Matrix) initialy passed as an argument(x) and a value connected to that matrix (called 'ans' in the function)
## The R object also contains a list of 4 elemenets that are two pairs of Set/Get functions.
## One pair of functions to set/get the value of the Matrix ('set' and 'get')
## Another pair to set/get the value connected to the Matrix.('setsolve' and 'getsolve')

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
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

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
