### Michael Knopf
### ID 24457981

# 1. Read in the data using read.csv because the file uses commas to separate the data.
dat = read.csv("http://www.stat.berkeley.edu/users/nolan/stat133/data/flow-occ-table.txt")

summary(dat)

# 2. The 2nd lane has the most flow, then the 1st, and the 3rd lane has the least.

plot(dat$Flow2 ~ dat$Flow3, main = "Flow 2 vs. Flow 3",
     xlab = "Flow 3", ylab = "Flow 2", pch = 16, cex = 0.4)
abline(a = 0, b = 1.5, col = 'red')

# 3. The the statement, "The flow in lane 2 is typically about 50% higher than in lane 3,"
#    is an accurate description of the relationship.  The scatterplot appears to follow
#    the line y = 1.5x.  Since y is 50% higher than x for all points on this line, and 
#    the vertical axis represents the flow in lane 2 while the horizontal axis represents
#    the flow in lane 3, this statement is accurate.


# 4. fracday

fracday = (0 : (length(dat$Occ1) - 1)) %% (24*60/5) * 5/(24*60)

plot((dat$Flow1 + dat$Flow2 + dat$Flow3) ~ fracday,
     main = "Total Flow vs. Time of Day (Lane 1)",
     xlab = "Time (days)",
     ylab = "Total Flow",
     pch = 20, cex = 0.2)

# 5. Flow peaks around both rush hours: around 8:00 am and around 5:00 pm.  Flow dies out
#    during the late hours of the night, reaching its lowest levels around 3:30 am, which
#    is very reasonable since even morning birds don't begin their days until at least 4:00 am,
#    for the most part.  The overall trend is a sharp spike and decline at 8:00 am, followed by
#    a gradual buildup throughout the workday, to a peak during evening rush hour, and finally
#    a smooth decline throughout the night hours.

plot(dat$Flow1 ~ dat$Occ1, pch = 20, cex = 0.2,
     main = "Flow vs. Occupancy (Lane 1)", xlab = "Occupancy", ylab = "Flow")

# 6. The shape is linear for the most part, but with some scattered outliers for which the ratio
#    of occupancy to flow is very high.  This is the manifestation of traffic, especially of
#    stop-and-go traffic.  Usually, high occupancy is the result of high flow.  More cars passing
#    the detectors results in higher total occupancy.  However, during traffic, cars are likely
#    to occupy detectors for abnormally long periods of time without actually increasing the
#    overall flow proportionately.  In the most extreme case, when traffic is stop-and-go,
#    a single car might actually be stopped on top of a detector for a long time.

breaks = which(fracday == 0)
cols = c('red', 'blue', 'green', 'dark blue', 'purple', 'orange','pink')
par(mfrow=c(2,3))
for (i in 1:(length(breaks)-1))
{
plot((dat$Flow1 + dat$Flow2 + dat$Flow3) ~ fracday,
     subset = breaks[i]:(breaks[i+1]-1),
     xlim = c(0,1),
     ylim = c(0,500),
     xlab = paste("Day", i),
     ylab = "Total Flow",
     col = cols[i],
     pch = 20, cex = 0.5)
}

# 7. It appears that Day 2 and Day 3 are on the weekend, because the rush hour spikes at 8am and 5pm
#    are absent on their graphs.  It would make sense, then that Day 1 is Friday, and so people are
#    active in the evening, going out with friends to do things (this explains the spike in the late
#    evening on Day 1).  However, this also means that Day 5 is a Tuesday.  The spike in the late evening
#    on this day does not have an obvious explanation other than possibly that there was an event that
#    night.  Perhaps there was a large concert or a Kings game on that night.


ass =
function(x , y)
{
  if (x %in% y)
    return (TRUE)
  else
    b = a - 3
    a = x+y;
    return (a)
}
ass(101,1:100)

fib =
function(n)
{
  if (n <= 2) return (1)
  else return (fib(n-2) + fib(n-1))
}














