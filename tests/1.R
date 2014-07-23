test.testTesting <- function() {
    checkTrue(TRUE) 
}

test.MCM <- function() {
    testMatrix <- matrix(c(4,2,7,6),2,2)
    newmat <- matrix(c(1,2,3,4),2,2)
    testMCM <- makeCacheMatrix(testMatrix)
    checkEquals(testMatrix, testMCM$get())
    checkEquals(NULL,testMCM$getInverse())
    testMCM$set(newmat)
    checkEquals(newmat, testMCM$get())
    checkEquals(NULL, testMCM$getInverse())
    newSol <- cacheSolve(testMCM)
    checkEquals(matrix(c(-2,1,1.5,-0.5),2,2),newSol)
    checkEquals(newSol, testMCM$getInverse())
    testMCM$set(testMatrix)
    checkEquals(NULL,testMCM$getInverse())
    testSol <- cacheSolve(testMCM)
    checkEquals(matrix(c(0.6,-0.2,-0.7,0.4),2,2),testSol)
    checkTrue(!is.null(testMCM$getInverse()))
    checkEquals(testMCM$getInverse(),testSol)
}