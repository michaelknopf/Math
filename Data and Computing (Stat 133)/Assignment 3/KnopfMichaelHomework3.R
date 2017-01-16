### Michael Knopf
### ID 24457981



#install.packages("RColorBrewer")
library(RColorBrewer)


####################### Question 1 ########################




load(url("http://www.stat.berkeley.edu/users/nolan/data/KaiserBabies.rda"))
hist(infants$wt, breaks = c(75,100,125,150,200,250), main = "Weights of Mothers",
     xlab = "Weights", ylim = c(0,.02), xaxt = 'n', col = "lightblue")
axis(side=1, c(75,100,125,150,200,250))
text(230,.004, 'max')
arrows(236,.0032,248,.0005)




####################### Question 2 ########################




levels(infants$smoke)[3:5] = "Other"
levels(infants$ed)[c(1,2,3,7)] = "HS or Less"
levels(infants$ed)[-1] = "More than HS"
mosaicplot(table(infants$ed, infants$smoke), main = "Education vs. Smoking Status",
           col = brewer.pal(3,"Set3"), cex = 1.1)




####################### Question 3 ########################




# Get means of smoker and non-smoker birthweights
nosmoke.mean = mean(infants$bwt[as.numeric(infants$smoke) == 1])
smoke.mean = mean(infants$bwt[as.numeric(infants$smoke) == 2])

# Plot birthweight vs. gestation
cols = c(rgb(0, 1, 0, 0.5), rgb(0, 0, 0, 0.5), rgb(1, 0, 0, 0.7)) # never, now, other
pc = c(19,25) # HS or Less, More than HS
plot(infants$gestation, infants$bwt, pch = pc[infants$ed],  col = cols[infants$smoke],
     cex = .7, xlab = "Gestation Period", ylab = "Birthweight", main = "Birthweight vs. Gestation Period")
abline(h = nosmoke.mean, col = rgb(0,1,0,.9))
abline(h = smoke.mean, col = rgb(0,0,0,.9))
text(170, nosmoke.mean - 2, 'Nonsmoker Average', cex = .6, pos = 3, col = rgb(0,1,0,.9))
text(166, smoke.mean - 2, 'Smoker Average', cex = .6, pos = 3, col = rgb(0,0,0,.9))

pc = c(15,15,15,19,25)
cols = c(rgb(0, 1, 0, 0.5), rgb(0, 0, 0, 0.5), rgb(1, 0, 0, 0.7), "black", "black")
legend("topleft", bty = 'n',
       c("Never Smoked","Current Smoker","Other","More than HS","Some HS"), cex = .8, pch = pc, col = cols)

