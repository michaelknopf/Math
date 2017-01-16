
debugSMD <- function(simUnifMatrix, simMaxDiffs, actualMaxDiff, simRank, k, n){
  cat("\n\n")
  cat("============ Check simUnifMatrix ============ \n")  
  simUnifMatrixChecks <- TRUE
  if(!all(dim(simUnifMatrix)==c(k, n))){
    cat("FAIL: simUnifMatrix has the wrong dimension. 
        Should have n columns and as many rows as the length of x. \n")
    simUnifMatrixChecks <- FALSE}
  if(class(simUnifMatrix) != "matrix"){
    cat("FAIL: simUnifMatrix should be a matrix. \n")
    simUnifMatrixChecks <- FALSE}
  if(!all(simUnifMatrix <= 720 & 
            simUnifMatrix >= 1 & 
            simUnifMatrix == round(simUnifMatrix))){
    cat("FAIL: simUnifMatrix must be integer values between 1 and 720, inclusive")
    simUnifMatrixChecks <- FALSE}
  if(simUnifMatrixChecks){
    cat("PASS: simUnifMatrix has the right dimension, class, and range of values.")}
  
  
  cat("\n\n")
  cat("============ Check simMaxDiffs ============ \n")
  if(simUnifMatrixChecks){
    simMaxDiffsChecks <- TRUE
    if(class(simMaxDiffsChecks) == "numeric"){
      cat("FAIL: simMaxDiffs should be a numeric vector. \n")
      simMaxDiffsChecks <- FALSE}
    if(simMaxDiffsChecks & length(simMaxDiffs) != n){
      cat("FAIL: simMaxDiffs should have length n. \n")
      simMaxDiffsChecks <- FALSE}
    if(simMaxDiffsChecks){
      cat("PASS: simMaxDiffs is of the correct class and length. \n")}
  } else{
    cat("NOT RUN: all tests for simUnifMatrix must pass before testing simMaxDiffs. \n")}
  
  
  
  cat("\n\n")
  cat("============ Check actualMaxDiff ============ \n")
  actualMaxDiffChecks <- TRUE
  if(!all(length(actualMaxDiff) == 1, 
          !is.na(actualMaxDiff),
          actualMaxDiff <= 1,
          actualMaxDiff >= 0)){
    cat("FAIL: actualMaxDiff should be a single non-NA value between 0 and 1. \n")
    actualMaxDiffChecks <- FALSE
  } else{
    cat("PASS: actualMaxDiff is a single non-NA value between 0 and 1. \n")}
  
  
  cat("\n\n")
  cat("============ Check simRank ============ \n")
  simRankCheck <- FALSE
  if(simMaxDiffsChecks & actualMaxDiffChecks){
    simRankCheck <- TRUE
    if(!all(length(simRank) == 1, 
            !is.na(simRank),
            simRank <= (n+1),
            simRank >= 1)){
      cat("FAIL: simRank should be a single non-NA value between 1 and n+1. \n")
      simRankCheck <- FALSE
    } else{
      cat("PASS: simRank is a single non-NA value between 1 and n+1. \n")}
  } else{
    cat("NOT RUN: all tests for simMaxDiffs and actualMaxDiff must pass before checking simRank. \n")
  }
  
  return(all(simUnifMatrixChecks, simMaxDiffsChecks, actualMaxDiffChecks, simRankCheck))
  
}


debugHR <- function(websiteRanks, scaledRanks, k){	
  websiteRanksChecks <- TRUE
  if(length(websiteRanks) != k){
    cat("FAIL: websiteRanks should have the same length as the input list. \n")
    websiteRanksChecks <- FALSE}
  if(!(class(websiteRanks) %in% c("integer", "numeric"))){
    cat("FAIL: websiteRanks should be an integer or numeric vector. \n")
    websiteRanksChecks <- FALSE}
  if(websiteRanksChecks){
    cat("PASS: websiteRanks is an integer or numeric vector of the right length. \n")}
  
  if(websiteRanksChecks){
    scaledRanksChecks <- TRUE
    if(length(scaledRanks) != length(websiteRanks)){
      cat("FAIL: scaledRanks should have the same length as websiteRanks. \n")
      scaledRanksChecks <- FALSE}
    if(!(class(scaledRanks) %in% c("integer", "numeric"))){
      cat("FAIL: scaledRanks should be an integer or numeric vector. \n")
      scaledRanksChecks <- FALSE}		
    if(!all(scaledRanks <= 1 & scaledRanks >= 0)){
      cat("FAIL: All values should be between 0 and 1, inclusive. \n")
      scaledRanksChecks <- FALSE}
    if(scaledRanksChecks){
      cat("PASS: scaledRanks is an integer or numeric vector with the right length and value range. \n")}
  } else{
    cat("NOT RUN: all tests for websiteRanks must pass before checking scaledRanks. \n")}
  
  return(all(websiteRanksChecks, scaledRanksChecks))
}

#### Test function ####

mainEnv <- environment()

testHW5 <- function(filePath){
  # run your .R file  
  source(filePath)
  mainEnv <- environment()
  
  # Problem 2
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n")
  cat("       TESTING simMaxDiff() FUNCTION \n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n\n")
  #  allSimMaxDiff <- FALSE
  
  cat("============ Check arguments ============ \n")
  argsCheck2 <- TRUE
  if(setequal(names(formals(simMaxDiff)), c("x", "n"))){
    cat("PASS: simMaxDiff() has two arguments, x and n. \n")
    if(formals(simMaxDiff)$n != 1000){
      cat("FAIL: The argument n should have a default value of 1000. \n")
      argsCheck2 <- FALSE
    } else{
      cat("PASS: The argument n has a default value of 1000. \n")}
  } else{
    cat("FAIL: simMaxDiff() should have two arguments, x and n. \n")
    argsCheck2 <- FALSE}
  
  cat("\n\n")
  cat("============ Check return ============ \n")
  retInd2 <- grep("return", body(simMaxDiff))
  if(body(simMaxDiff)[[retInd2]] != "return(simRank)"){
    cat("FAIL: function should return simRank as output. \n")
    argsCheck2 <- FALSE
  } else{
    cat("PASS: function returns simRank as output. \n")
  }
  
  if(argsCheck2){
    cat("\n\n")
    cat("============ Check simMaxDiff() function body: ============ \n")
    trace("simMaxDiff", quote(assign("allSimMaxDiff", debugSMD(simUnifMatrix, simMaxDiffs, actualMaxDiff, simRank, length(x), n), envir=mainEnv)),
          at=retInd2, print=FALSE)
    tmp1 <- simMaxDiff(x=c(1, 2, 720), n=1000)
    untrace(simMaxDiff)}
  
  # Problem 3
  cat("\n\n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n")
  cat("       TESTING histRanks() FUNCTION \n")
  cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n\n")
  if(!allSimMaxDiff){
    cat("NOT RUN: all tests for simMaxDiff() must pass before checking histRanks(). \n")
  } else{
    
    argsCheck3 <- TRUE
    cat("============ Check arguments ============ \n")
    if(setequal(names(formals(histRanks)), c("x", "n"))){
      cat("PASS: histRanks() has two arguments, x and n. \n")
      if(formals(histRanks)$n != 1000){
        cat("FAIL: The argument n should have a default value of 1000. \n")
        argsCheck3 <- FALSE
      } else{
        cat("PASS: The argument n has a default value of 1000. \n")}
    } else{
      cat("FAIL: histRanks() should have two arguments, x and n. \n")
      argsCheck3 <- FALSE}
    
    cat("\n\n")
    cat("============ Check return ============ \n")
    retInd3 <- grep("return", body(histRanks))
    if(body(histRanks)[[retInd3]] != "return(scaledRanks)"){
      cat("FAIL: function should return scaledRanks as output. \n")
      argsCheck3 <- FALSE
    } else{
      cat("PASS: function returns scaledRanks as output. \n")
    }
    
    if(argsCheck3){
      cat("\n\n")
      cat("============ Check histRanks() function body: ============ \n")
      mainEnv <- environment()
      trace("histRanks", quote(assign("allHistRanks", debugHR(websiteRanks, scaledRanks, length(x)), envir=mainEnv)),
            at=retInd3, print=FALSE)
      tmp2 <- histRanks(Cache500[1:8])
      untrace(histRanks)}
  }
  
  
  # Problem 4
  cat("\n\n")
  cat("============ Check whichOKRanks ============ \n")
  if(!all(allSimMaxDiff & allHistRanks)){
    cat("NOT RUN: all tests for simMaxDiff and histRanks must pass before checking rest of HW \n")
    whichOKChecks <- FALSE
  } else{
    whichOKChecks <- TRUE
    if(exists("whichOKRanks")){
      if(!all(length(whichOKRanks) == 23,
              class(whichOKRanks) == "numeric",
              all(whichOKRanks <= 1),
              all(whichOKRanks >= 0))){
        cat("FAIL: whichOKRanks should be a numeric vector of the same length as whichOK with values in [0, 1]. \n")
        whichOKChecks <- FALSE
      } else{
        cat("PASS: whichOKRanks is a numeric vector of the right length and value range. \n")}
    } else{
      cat("FAIL: whichOKRanks does not exist. \n")
      whichOKChecks <- FALSE}
  }
  
  # Problem 5
  cat("\n\n")
  cat("============ Check hiRankSite ============ \n")
  if(!whichOKChecks){
    cat("NOT RUN: all tests for whichOKRanks must pass before checking this part \n")
    hiRankSiteCheck <- FALSE
  } else{
    if(exists("hiRankSite")){
      if(length(hiRankSite) == 1 & !is.na(hiRankSite) & hiRankSite %in% whichOK){
        cat("PASS: hiRankSite is an element of whichOK. \n")
        hiRankSiteCheck <- TRUE
      } else{
        cat("FAIL: hiRankSite should be an element of whichOK. \n")
        hiRankSiteCheck <- FALSE}
    } else{
      cat("FAIL: hiRankSite does not exist. \n")
      hiRankSiteCheck <- FALSE}
  }
  
  cat("\n\n")
  cat("============ Check unifQuantiles and hiRankQuantiles ============ \n")
  if(!hiRankSiteCheck){
    cat("NOT RUN: all tests for hiRankSite must pass before checking this part \n")
  } else{
    if(!all(exists("unifQuantiles") & exists("hiRankQuantiles"))){
      cat("FAIL: unifQuantiles and hiRankQuantiles do not exist. \n")
    } else{
      if(length(unifQuantiles) == length(hiRankQuantiles) & length(hiRankQuantiles) ==  99){
        cat("PASS: unifQuantiles and hiRankQuantiles are the correct length (99) \n")
      } else{
        cat("FAIL: unifQuantiles and hiRankQuantiles should each be 99 elements long. \n")
      }
      if(all(unifQuantiles <= 1, 
             unifQuantiles >= 0,
             hiRankQuantiles <= 720,
             hiRankQuantiles >= 1)){
        cat("PASS: unifQuantiles has values in [0, 1], and hiRankQuantiles has values in 1...720. \n")
      } else{
        cat("FAIL: unifQuantiles should have values in [0, 1]; hiRankQuantiles should have values in 1...720 \n")}
    }
  }
  
  cat("\n\n")
  cat("============ Check unifQuantiles and hiRankQuantiles ============ \n")
  if(length(list.files(pattern="Hist.png")) >= 1){
    cat("PASS: code creates a single image with filename ...Hist.png in the working directory \n")
  } else{
    cat("FAIL: code does not create a single image with filename ...Hist.png in the working directory. \n")
  }
  
  if(length(list.files(pattern="QQ.png")) >= 1){
    cat("PASS: code creates a single image with filename ...QQ.png in the working directory \n")
  } else{
    cat("FAIL: code does not create a single image with filename ...QQ.png in the working directory. \n")
  }
  
  cat("\n\n")
  cat("Finished running test code. \n")    
  
}