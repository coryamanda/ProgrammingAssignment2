makeCacheMatrix <- function(x = matrix()) { #passed a matrix
        #x stores original matrix's value and inv, which will be the cached value but is now null
        
        inv <- NULL #this is our inverse, reset to NULL every time makeCacheMatrix is called
        
        set <- function(y){ #setter, lets you assign a new value to the object to save memory
                x <<- y #saves the input matrix
                inv <<- NULL #resets inverse to NULL, like we did when we created a new object
        }
        
        get<- function() { #getter - returns the value of the original matrix
                x 
        }
        
        setinversion<- function(inversion) { #setter - accesses and stores value
                inv <<- inversion 
        }
        
        getinversion <- function() { #getter - returns the cached value subsequent times
                inv
        }
        
        list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
        #accessed each time makeCacheMatrix() is called (when we make a new object).
}

cacheSolve<- function(x, ...) {
        #Input x is an object created by makeCacheMatrix
        #Accesses the object created when makeCacheMatrix was called (NOT the makeCacheMatrix function)
        #by fetching the value of the matrix used to create the object, which was stored
        #when the object was created
        #X is replaced by the name of the object when cacheSolve() is called
        
        inv <- x$getinversion() #Gets the inversion from the object
        
        if(!is.null(inv)) { #if inv IS NOT null it just returns the inversion
                message("getting cached data")
                return(inv)
        }
        
        data<- x$get() #if it IS null, it calculates the inversion and stores it 
        #in the object created by the call to makeCacheMatrix(), then returns it
        inv<- solve(data, ...) #calculation of the inverse of data
        x$setinversion(inv) #stores the inversion value back in x
        inv #returns inv to end the function
}