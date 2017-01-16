testHW4 <- function(filePath){
# function to check value of the 1% percentile using rough
# estimates from empirical (sample) percentiles
checkFirstPercentile <- function(quantiles, changeTimes){
  empiricalQuantiles <- seq(0, 1, length.out=length(changeTimes))
  inds <- c(max(which.min(empiricalQuantiles <= 0.01)-1, 1),
            which.max(empiricalQuantiles > 0.01))
  # sort the data
  changeTimes <- sort(changeTimes)
  # rough range for the 1st percentile
  percentileRange <- changeTimes[inds]
  aboveLower <- quantiles[1] >= percentileRange[1] # check if above lower bound
  belowUpper <- quantiles[1] <= percentileRange[2] # check if below upper bound
  
  if(aboveLower){
    if(belowUpper){
      msg <- "......PASS: 1% percentile in expected range. \n"
      pass <- TRUE
    } else{
      msg <- "!.....FAIL: 1% percentile is too large. \n"
      pass <- FALSE}
  } else{
    msg <- "!.....FAIL: 1% percentile is too small. \n"
    pass <- FALSE}
  
  return(list(pass=pass, msg=msg))
}	
	
  
  # source your HW4 file
  source(filePath)
  
  cat("\n\n\nBASIC TESTS FOR YOUR HW 4 .R FILE \n")
  cat("............................................. \n")
  cat(".................    #1    .................. \n")
  cat("............................................. \n\n")
  
  if(exists("Cache500")){
    cat("PASS: A variable named Cache500 exists. \n")
  } else{
    cat("FAIL: The data should be loaded in as a variable named Cache500. \n")
  }
  
  if(exists("Cache500_class")){
  	cat("PASS: Cache500_class exists. \n")
  	
  	# check length
  	if(length(Cache500_class) == 500){
  	cat("......PASS: correct length.")
  	} else{
  	cat("......FAIL: should be same length as Cache500.")}
  	
  	} else{
  	cat("FAIL: Define a vector called Cache500_class. \n")
  	}
  
  cat("\n\n............................................. \n")
  cat(".................    #2    .................. \n")
  cat("............................................. \n\n")
  
  for(var_Prob2 in c("numChanges", "firstChange", "lastChange")){
    if(exists(var_Prob2)){
      cat(sprintf("PASS: A variable named %s exists.",
                  var_Prob2), "\n")
      
      # check class
      if(class(get(var_Prob2)) %in% c("numeric", "integer")){
        cat(sprintf("......PASS: correct class.",
                    var_Prob2), "\n")
      } else{
        cat(sprintf("!.....FAIL: %s should be a numeric or integer vector.",
                    var_Prob2), "\n")}
      
      # check length
      if(length(get(var_Prob2)) == 500){
        cat(sprintf("......PASS: correct length.",
                    var_Prob2), "\n")
      } else{
        cat(sprintf("!.....FAIL: %s should be the same length as Cache500.",
                    var_Prob2), "\n")}
      
      
    } else{
      cat(sprint("! FAIL: Create a vector called %s.", 
                 var_Prob2), "\n")}
    cat("\n")
  }
  
  cat("\n\n............................................. \n")
  cat(".................    #3    .................. \n")
  cat("............................................. \n\n")
  
  prob3Test <- TRUE
  if(exists("whichOK")){
    cat("PASS: A variable called whichOK exists. \n")
    
    # check class
    if(class(whichOK) == "integer"){
      cat("......PASS: correct class. \n")
    } else{
      cat("!.....FAIL: should be an integer vector. \n")
      prob3Test <- FALSE}
    
    # check for no NAs
    if(all(!is.na(whichOK))){
      cat("......PASS: no missing values. \n")
    } else{
      cat("!.....FAIL: shouldn't have missing values. \n")
      prob3Test <- FALSE}
    
    # check values
    if(all(whichOK <= 500 & whichOK >= 1)){
      cat("......PASS: values in right range (1 to 500). \n")
    } else{
      cat("!.....FAIL: Are you sure these are indices?
         They should be between 1 and 500. \n")
      prob3Test <- FALSE}
    
  } else{
    cat("!.....FAIL: Define a vector called whichOK.")
    prob3Test <- FALSE
  }
  
  
  cat("\n\n............................................. \n")
  cat(".................    #4    .................. \n")
  cat("............................................. \n\n")
  
  cat("~~~~~~~~~TEST unifQuantiles~~~~~~~~~\n")
  if(exists("unifQuantiles")){
    cat("PASS: unifQuantiles exists. \n")
    
    # check class
    if(class(unifQuantiles) == "numeric"){
      cat("......PASS: correct class. \n")
    } else{
      cat("!.....FAIL: should be a numeric vector. \n")
    }
    
    # check length
    if(length(unifQuantiles)==99){
      cat("......PASS: correct length. \n")
    } else{
      cat("!.....FAIL: should have 99 values, for 1st to 99th percentile")}
    
    # check values
    if(all(unifQuantiles < 1 & unifQuantiles > 0)){
      cat("......PASS: values in right range (0, 1). \n")
    } else{
      cat("!.....FAIL: elements should be strictly between 0 and 1. \n")}
    
  } else{
    cat("FAIL: Define a vector called unifQuantiles. \n")
  }
  
  cat("\n\n")
  cat("~~~~~~~~~TEST siteIndex~~~~~~~~~\n")
  siteIndexTest <- TRUE
  if(prob3Test){
    if(exists("siteIndex")){
      cat("PASS: siteIndex exists. \n")
      
      # check if only one value
      if(length(siteIndex) == 1){
        cat("......PASS: a single value. \n")
        
        # check if integer
        if(as.integer(siteIndex) == siteIndex){
          cat("......PASS: is an integer. \n")
        } else{
          cat("!.....FAIL: an index should be an integer. \n")
          siteIndexTest <- FALSE}
        
        # check value
        if(is.element(siteIndex, whichOK)){
          cat("......PASS: is an element of whichOK. \n")
        } else{
          cat("!.....FAIL: should be equal to an element of whichOK. \n")
          siteIndexTest <- FALSE}
        
      } else{
        cat("!.....FAIL: should be a single value. \n")
        siteIndexTest <- FALSE}
    } else{
    	cat("FAIL: siteIndex should be defined a value. \n")
    	siteIndexTest <- FALSE}
 }else{
    cat("NOT RUN: All tests for Problem 3 must pass before running this test.
    Please fix the whichOK vector.")
    siteIndexTest <- FALSE
  }
  
  
  cat("\n\n")
  cat("~~~~~~~~~TEST siteQuantiles~~~~~~~~~\n")
  if(siteIndexTest){
    if(exists("siteQuantiles")){
      cat("PASS: siteQuantiles exists. \n")
      
      # check length
      if(length(siteQuantiles)==99){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have 99 values, for 1st to 99th percentile \n")}
      
      # check first percentile value
      valueCheck <- checkFirstPercentile(siteQuantiles, Cache500[[siteIndex]])
      if(valueCheck$pass){
        cat(valueCheck$msg)
      } else{
        cat(valueCheck$msg, "This test assumes that you are using the
          element of Cache500 given by siteIndex. For example, if
          siteIndex is 5, you should be using the 5th element of
          Cache500. Otherwise it will fail. \n")
      }
      
    } else{
      cat("FAIL: Define a vector called siteQuantiles. \n")}
  } else{
    cat("NOT RUN: All tests for siteIndex must pass before running this test.
      Fix the value of siteIndex. \n")
  }
  
  
  cat("\n\n............................................. \n")
  cat(".................    #5    .................. \n")
  cat("............................................. \n\n")
  cat("~~~~~~~~~TEST lumpedChanges~~~~~~~~~ \n")
lumpedChangesTest <- TRUE
  if(prob3Test){
    if(exists("lumpedChanges")){
      cat("PASS: lumpedChanges exists. \n")
      
      # check class
      if(class(lumpedChanges)=="integer"){
        cat("......PASS: correct class. \n")
      } else{
        cat("!.....FAIL: should be an integer vector. \n")
        lumpedChangesTest <- FALSE}
      
      # check length
      lumpedLength = sum(sapply(Cache500[whichOK], length))
      if(length(lumpedChanges) == lumpedLength){
        cat("......PASS: correct length. \n")
      } else{
        ifelse(length(lumpedChanges) > lumpedLength,
               cat("!.....FAIL: vector too long \n"), 
               cat("!.....FAIL: vector too short \n"))
        lumpedChangesTest <- FALSE}
    } else{
      cat("FAIL: Define a vector named lumpedChanges. \n")
      lumpedChangesTest <- FALSE}
  } else{
    cat("NOT RUN: All tests for Problem 3 must pass before running this test.
    Please fix the whichOK vector. \n")
    lumpedChangesTest <- FALSE
  }
  
  
  cat("\n\n")
  cat("~~~~~~~~~TEST lumpedQuantiles~~~~~~~~~ \n")
  if(lumpedChangesTest){
    if(exists("lumpedQuantiles")){
      cat("PASS: lumpedQuantiles exists. \n")
      
      # check length
      if(length(lumpedQuantiles)==99){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have 99 values, for 1st to 99th percentile \n")}
      
      # check first percentile value
      cat(checkFirstPercentile(lumpedQuantiles, lumpedChanges)[[2]])
    } else{
      cat("FAIL: Define a vector called lumpedQuantiles. \n")}
  } else{
    cat("NOT RUN: All tests for lumpedChanges must pass before running this test.
      Correct lumpedChanges first. \n")
  }  
  
  
  
  
  cat("\n\n............................................. \n")
  cat(".................    #6    .................. \n")
  cat("............................................. \n\n")
  cat("~~~~~~~~~TEST simSiteChanges~~~~~~~~~ \n")
  simSiteChangesTest <- TRUE
  if(siteIndexTest){
    if(exists("simSiteChanges")){
      cat("PASS: simSiteChanges exists. \n")
      
      # check length
      if(length(simSiteChanges) == length(Cache500[[siteIndex]])){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have the same length as the 
          number of changes for the website chosen in #4 \n")
        simSiteChangesTest <- FALSE}
      
      # if there are no missing values, check values
      if(all(!is.na(simSiteChanges))){
        
        # check integer-valued
        if(all(as.integer(simSiteChanges)==simSiteChanges)){
          cat("......PASS: all integers \n")
        } else{
          cat("!.....FAIL: all values should be integers. \n")
          simSiteChangesTest <- FALSE}
        
        # check that values are between 0 and 720
        if(all(simSiteChanges <= 720 & simSiteChanges > 0)){
          cat("......PASS: values in correct range. \n")
        } else{
          cat("!.....FAIL: all values should be between 0 and 720. \n")
        }
        
      } else{
        cat("!.....FAIL: should not have missing values. \n")
        simSiteChangesTest <- FALSE}
    } else{
      cat("FAIL: Define a vector named simSiteChanges. \n")
      simSiteChangesTest <- FALSE
    }
  } else{
    cat("NOT RUN: All tests for siteIndex must pass before running this test.
      Fix the value of siteIndex. \n")
      simSiteChangesTest <- FALSE
  }  
  
  cat("\n\n")
  cat("~~~~~~~~~TEST simSiteQuantiles~~~~~~~~~ \n")
  if(simSiteChangesTest){
    if(exists("simSiteQuantiles")){
      cat("PASS: siteQuantiles exists. \n")
      
      # check length
      if(length(simSiteQuantiles)==99){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have 99 values, for 1st to 99th percentile \n")}
      
      # check first percentile value
      cat(checkFirstPercentile(simSiteQuantiles, simSiteChanges)[[2]])
    } else{
      cat("FAIL: Define a vector called siteQuantiles. \n")}
  } else{
    cat("NOT RUN: All tests for simSiteChanges must pass before running this test.
      Correct simSiteChanges first. \n")
  }
  
  
    cat("\n\n")  
    cat("~~~~~~~~~TEST normChanges~~~~~~~~~ \n")
  normChangesTest <- TRUE
  if(siteIndexTest){
    if(exists("normChanges")){
      cat("PASS: normChanges exists. \n")
      
      # check length
      if(length(normChanges) == length(Cache500[[siteIndex]])){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have the same length as the 
          number of changes for the website chosen in #4 \n")
        normChangesTest <- FALSE}
      
      # if there are no missing values, check values
      if(all(!is.na(normChanges))){
      	cat("......PASS: no missing values. \n")        
      } else{
        cat("!.....FAIL: should not have missing values. \n")
        normChangesTest <- FALSE}
    } else{
      cat("FAIL: Define a vector named normChanges. \n")
      normChangesTest <- FALSE}
  } else{
    cat("NOT RUN: All tests for siteIndex must pass before running this test.
      Fix the value of siteIndex. \n")
      normChangesTest <- FALSE
  }  
  
  
  cat("\n\n")
  cat("~~~~~~~~~TEST normQuantiles~~~~~~~~~ \n")
  if(normChangesTest){
    if(exists("normQuantiles")){
      cat("PASS: normQuantiles exists. \n")
      
      # check length
      if(length(normQuantiles)==99){
        cat("......PASS: correct length. \n")
      } else{
        cat("!.....FAIL: should have 99 values, for 1st to 99th percentile \n")}
      
      # check first percentile value
      cat(checkFirstPercentile(normQuantiles, normChanges)[[2]])
    } else{
      cat("FAIL: Define a vector called normQuantiles. \n")}
  } else{
    cat("NOT RUN: All tests for normChanges must pass before running this test.
      Correct normChanges first. \n")
  }  
  
}
