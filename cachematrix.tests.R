## A file to test that the functions work correctly

source("cachematrix.R")

m1 <- rbind(
  c( 4,  3),
  c( 3,  2)
)

m1i <- rbind(
  c(-2,  3),
  c( 3, -4)
)

m2 <- rbind(
  c( 1,  3,  3),
  c( 1,  4,  3),
  c( 1,  3,  4)
)

m2i <- rbind(
  c( 7, -3, -3),
  c(-1,  1,  0),
  c(-1,  0,  1)
)

assert_isClass <- function(testValue, expectedClass){
  if(class(testValue) == expectedClass){
    # message("assert successful")
  } else {
    stop("assert failed")
  }
}

assert_isNotNull <- function(testValue){
  if(!is.null(testValue)){
    # message("assert successful")
  } else {
    stop("assert failed")
  }
}

assert_areSameMatrix <- function(actual, expected){
  if(all.equal(actual, expected)){
    # message("assert successful")
  } else {
    stop("assert failed")
  }
}

runAll <- function(){
  makeCacheMatrix_isAFunction()
  makeCacheMatrix_returnedValue_isAList()
  makeCacheMatrix_returnedValue_hasAGet()
  makeCacheMatrix_returnedValue_hasASet()
  makeCacheMatrix_returnedValue_hasAGetInverse()
  makeCacheMatrix_returnedValue_hasASetInverse()
  
  cacheSolve_isAFunction()
  cacheSolve_returnedValue_isAMatrix_1()
  cacheSolve_returnedValue_isAMatrix_2()
  cacheSolve_inverts_cacheMatrices_1()
  cacheSolve_inverts_cacheMatrices_2()
  cacheSolve_invertsBack_original_1()
  cacheSolve_invertsBack_original_2()
  cacheSolve_inverts_whenTwice_sameResult_1()
  cacheSolve_inverts_whenTwice_sameResult_2()
  
  message("all tests ran successfully!")
}

makeCacheMatrix_isAFunction <- function(){
  assert_isNotNull(makeCacheMatrix)
  assert_isClass(makeCacheMatrix, "function")
}

makeCacheMatrix_returnedValue_isAList <- function(){
  returnedValue <- makeCacheMatrix()
  assert_isNotNull(returnedValue)
  assert_isClass(returnedValue, "list")
}

makeCacheMatrix_returnedValue_hasAGet <- function(){
  returnedValue <- makeCacheMatrix()
  assert_isNotNull(returnedValue$get)
}

makeCacheMatrix_returnedValue_hasASet <- function(){
  returnedValue <- makeCacheMatrix()
  assert_isNotNull(returnedValue$set)
}

makeCacheMatrix_returnedValue_hasAGetInverse <- function(){
  returnedValue <- makeCacheMatrix()
  assert_isNotNull(returnedValue$getInverse)
}

makeCacheMatrix_returnedValue_hasASetInverse <- function(){
  returnedValue <- makeCacheMatrix()
  assert_isNotNull(returnedValue$setInverse)
}

cacheSolve_isAFunction <- function(){
  assert_isNotNull(cacheSolve)
  assert_isClass(cacheSolve, "function")
}

cacheSolve_returnedValue_isAMatrix_1 <- function(){
  cacheMatrix <- makeCacheMatrix(m1)
  returnedValue <- cacheSolve(cacheMatrix)
  assert_isNotNull(returnedValue)
  assert_isClass(returnedValue, "matrix")
}

cacheSolve_returnedValue_isAMatrix_2 <- function(){
  cacheMatrix <- makeCacheMatrix(m2)
  returnedValue <- cacheSolve(cacheMatrix)
  assert_isNotNull(returnedValue)
  assert_isClass(returnedValue, "matrix")
}

cacheSolve_inverts_cacheMatrices_1 <- function(){
  cacheMatrix <- makeCacheMatrix(m1)
  returnedValue <- cacheSolve(cacheMatrix)
  assert_areSameMatrix(returnedValue, m1i)
}

cacheSolve_inverts_cacheMatrices_2 <- function(){
  cacheMatrix <- makeCacheMatrix(m2)
  returnedValue <- cacheSolve(cacheMatrix)
  assert_areSameMatrix(returnedValue, m2i)
}

cacheSolve_invertsBack_original_1 <- function(){
  cacheMatrixA <- makeCacheMatrix(m1)
  returnedValueA <- cacheSolve(cacheMatrixA)
  cacheMatrixB <- makeCacheMatrix(returnedValueA)
  returnedValueB <- cacheSolve(cacheMatrixB)
  assert_areSameMatrix(returnedValueB, m1)
}

cacheSolve_invertsBack_original_2 <- function(){
  cacheMatrixA <- makeCacheMatrix(m2)
  returnedValueA <- cacheSolve(cacheMatrixA)
  cacheMatrixB <- makeCacheMatrix(returnedValueA)
  returnedValueB <- cacheSolve(cacheMatrixB)
  assert_areSameMatrix(returnedValueB, m2)
}

cacheSolve_inverts_whenTwice_sameResult_1 <- function(){
  cacheMatrix <- makeCacheMatrix(m1)
  returnedValueA <- cacheSolve(cacheMatrix)
  returnedValueB <- cacheSolve(cacheMatrix)
  assert_areSameMatrix(returnedValueB, returnedValueA)
}

cacheSolve_inverts_whenTwice_sameResult_2 <- function(){
  cacheMatrix <- makeCacheMatrix(m2)
  returnedValueA <- cacheSolve(cacheMatrix)
  returnedValueB <- cacheSolve(cacheMatrix)
  assert_areSameMatrix(returnedValueB, returnedValueA)
}


