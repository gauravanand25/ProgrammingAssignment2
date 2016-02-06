makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(z) {
                x <<- z
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(y, ...) {
        m <- y$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- y$get()
        m <- mean(data, ...) # extra arguments are passed as it is to mean function ( na.rm, trim)
        y$setmean(m)
        m
}

# x <- c(1,2,3)
# y <- makeVector(x)
# cachemean(y, na.rm=TRUE) # mean would be computed
# cachemean(y) # mean was cached earlier
