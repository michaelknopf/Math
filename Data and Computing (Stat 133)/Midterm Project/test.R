testrun = function(params = matrix(c(1, 1, 1, 2, 2, 1, 2, 3, 1, 2), ncol = 2, byrow = FALSE)){
#    Rprof("mainprof")
  mcOutput = MCBA(params = params)
#    Rprof(NULL)  
  mcOutput
}

###    Professor Ibser told us that we could use extra parameters in our report.

####   The commented line below will test the simulation using the parameters
####   that produced the results we have published in our html report.  However, it will
####   take approximately ten minutes to run using these parameters.

####   To test the simulation using only the parameters given by Professor Ibser,
####   run the first line of code, which has not been commented out.  It runs
####   testrun with the default parameters, which are those given by Professor Ibser.

# mcOutput=testrun(matrix(c( 1, 1, 1, 2, 1.2, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2,
#                            1, 2, 3, 2,   1,   1,   1,   1,   1,   1,   1, 1), ncol = 2, byrow = FALSE))

mcOutput = testrun()



###### First Plot  #######

survivalproportion = 1-mcOutput$numEnd[]/100
names(survivalproportion) = c("(1,1)","(1,2)","(1,3)","(2,2)","(1.2,1)","(1.4,1)",
                              "(1.5,1)","(1.6,1)","(1.7,1)","(1.8,1)","(1.9,1)","(2,1)")
png("LalKnopf_survivalproportion.png", width=500, height=500)
barplot(survivalproportion, ylim = c(0,1), las=3,
        main = "Survival Proportion\nvs.\nParameters",
        ylab = "Survival Proportion",
        xlab = "")
mtext(side = 1, text = expression(paste("Parameters (", lambda,",", kappa,")")), line = 4)
dev.off()



###### Second Plot  #######

thirdQuart = mcOutput$OS75
names(thirdQuart) = c("(1,1)","(1,2)","(1,3)","(2,2)","(1.2,1)","(1.4,1)",
               "(1.5,1)","(1.6,1)","(1.7,1)","(1.8,1)","(1.9,1)","(2,1)")
png("LalKnopf_thirdquartile.png", width=500, height=500)
barplot(thirdQuart, ylim = c(0,500000), las = 3,
        main = "Number of Offspring (Third Quartile)\nvs.\nParameters",
        ylab = "Offspring (3rd Quartile)",
        xlab = "")
mtext(side = 1, text = expression(paste("Parameters (", lambda,",", kappa,")")), line = 4)
dev.off()


###### Third Plot  #######

png("LalKnopf_genmedian.png", width=500, height=500)
plot(c(1,.5,.33,1,1.2,1.4,1.5,1.6,1.7,1.8,1.9,2),mcOutput$Gen50,
     main = "Number of Generations (Median)\nvs.\n Ratio of Birth Rate to Death Rate",
     xlab = expression(paste(lambda,"/", kappa)),
     ylab = "Number of Generations (Median)",
     pch = 19,
     cex = .9,
     xlim = c(0,2))
dev.off()



