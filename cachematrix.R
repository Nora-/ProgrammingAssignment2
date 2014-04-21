################################################################################
#                                                                              #
#  Coursera's R Programming, Prof. Peng                                        #
#  Programming Assignment 2/ Peer Assessment                                   #
#                                                                              #
#  A pair of functions that "creates" a special type of "matrix object" that   #
#  computes its inverse only when needed- but when it does so, it caches this  #
#  inverse inthe same object storing the original matrix.                      #
#                                                                              #
#  The process of determining whether computation or retrieval of a cached     #
#  inverse is needed is handled by the function cacheSolve. If computation is  #
#  necessary, cacheSolve uses the built-in R function, solve.                  #
#                                                                              #
#  Future Improvements: Have cacheSolve be a helper function only called by    #
#  the matrix object's get.inverse method so that users need only use the      #
#  object's getter & setter methods.                                           #
#                                                                              #
#  Considerations re: CacheSolve implementation: maybe store the initial call  #
#  to x$get.inverse() in a var called cached_inverse to return.                #
#   - might be minimal savings in time (i.e. never re-perform the same         #
#     calculation), but this function call doesn't do any lengthy calculation, #
#     it just looks up the val bound to a var                                  #
#   - might inc. memory usage, having to store a potentially large inverse in  #
#     2 vars (NOTE: learn more about how R stores variable val assignments/    #
#     look into how pointers work, etc.)                                       #
#   - might make logic of function less modular as it is now:                  #
#     1. retrieve/calc inverse in one of the mutually exclusive cond branches  #
#     2. return inverse regardless of which cond branch you took.              #
#                                                                              #
################################################################################


## Function that initially creates the special matrix object
## & creates a getter & setter for the matrix
## & creates a getter & setter for the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # initialize matrix's inverse to be NULL
    inverse <- NULL
    
    # set matrix vals
    set <- function(m) {
        x <<- m
        
        # new matrix vals means cached inverse is inaccurate, 
        # set to NULL accordingly
        inverse <<- NULL
    }
    
    # get matrix vals
    get <- function() {
        x
    }
    
    # set matrix inverse
    set.inverse <- function(updated_inverse) {
        inverse <<- updated_inverse
    }
    
    # get matrix inverse
    get.inverse <- function() {
        inverse
    }
    
    # construct & return matrix object
    list(set = set, 
        get = get,  
        set.inverse = set.inverse,
        get.inverse = get.inverse)
}


## Determines whether computation or retrieval of a cached inverse is needed is.
## If computation is necessary, the built-in R function solve() is used.
## '...' arg allows users to retain ability to update default params to solve().
cacheSolve <- function(x, ...) {
        
        # if there isn't a cached inverse, use solve to compute it, 
        # & cache the result
        if (is.null(x$get.inverse())) {
            message("Calculating & caching inverse matrix...")
            inverse = solve(x$get(), ...)
            x$set.inverse(inverse)    
        }
        
        # no work to do, inverse has been computed previously
        else {
            message("Retrieving cached inverse matrix...")            
        }
    
        # Return x's inverse matrix
        x$get.inverse()
}
