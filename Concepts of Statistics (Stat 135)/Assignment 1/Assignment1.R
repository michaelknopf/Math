### Michael Knopf
### Lecture 2

data = read.table("http://www.stat.berkeley.edu/~statlabs/data/babiesI.data", header = TRUE);
smoke = data$smoke
nsmokers = data$bwt[smoke==0];
smokers = data$bwt[smoke==1];
boxplot(main = "Boxplots", nsmokers, smokers,
        ylab = "Birthweight", names = c("Non-Smokers", "Smokers"))
hist(nsmokers, freq = FALSE, main = "Histogram of Non-Smokers",
     xlab = "Birthweights", xlim = c(50,180), ylim = c(0,0.025))
hist(smokers, freq = FALSE, main = "Histogram of Smokers",
     xlab = "Birthweights", xlim = c(50,180), ylim = c(0,0.025))