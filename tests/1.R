test.testTesting <- function() {
    checkTrue(TRUE) 
}

test.MCM <- function() {
    testMatrix <- matrix(c(4,2,7,6),2,2)
    newmat <- matrix(c(1,2,3,4),2,2)
    testMCM <- makeCacheMatrix(testMatrix)
    #check that matrix is being set properly
    checkEquals(testMatrix, testMCM$get()) #1
    # check that inverse variable properly initialized to NULL
    checkEquals(NULL,testMCM$getInverse()) #2
    testMCM$set(newmat)
    # check that setter is working properly
    checkEquals(newmat, testMCM$get()) #3
    # check that inverse stays null after set
    checkEquals(NULL, testMCM$getInverse()) #4
    newSol <- cacheSolve(testMCM)
    #check that cacheSolve gives the correct answer (matrix newmat)
    checkEquals(matrix(c(-2,1,1.5,-0.5),2,2),newSol) #5
    # check that there is now a value for inverse and that 
    # getInverse works properly
    checkEquals(newSol, testMCM$getInverse()) #6
    testMCM$set(testMatrix)
    # check that setting the matrix resets inverse to NULL
    checkEquals(NULL,testMCM$getInverse()) #7
    testSol <- cacheSolve(testMCM)
    # check for the correct answer with another matrix (testMatrix)
    checkEquals(matrix(c(0.6,-0.2,-0.7,0.4),2,2),testSol) #8
    # check that the inverse cache is now set
    checkTrue(!is.null(testMCM$getInverse())) #9
    # check that the cached value is correct
    checkEquals(testMCM$getInverse(),testSol) #10
    # this set of checks creates two makeCacheMatrix objects and
    # checks to make sure their caches don't interfere
    testMCM$set(testMatrix)
    newMCM <- makeCacheMatrix(newmat)
    checkTrue(is.null(testMCM$getInverse())) #11
    checkTrue(is.null(newMCM$getInverse())) #12
    checkTrue(is.null(newMCM$inverse)) #13
    checkTrue(is.null(testMCM$inverse)) #14
    cacheSolve(testMCM)
    checkTrue(!is.null(testMCM$getInverse())) #15
    checkTrue(is.null(newMCM$getInverse())) #16
    # now we've confirmed that testMCM has a cached inverse
    # and newMCM doesn't, and that the value of xxxMCM$inverse
    # was null before, now we'll check that they were null
    # because you can't pull the data out that way, not
    # because the cache hadn't been set yet
    checkTrue(is.null(testMCM$inverse)) #17
    checkTrue(is.null(newMCM$inverse)) #18
}