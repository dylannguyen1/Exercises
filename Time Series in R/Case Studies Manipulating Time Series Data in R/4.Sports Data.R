#
## Advanced features of xts
#
unemployment <- readRDS("unemployment.RData")
## generate index of final measurement of unemployment per year
years <- endpoints(unemployment, on = "years")
unemployment[years] ## show final measure of unemployment that year

period.apply(unemployment,
              INDEX= years,
              FUN = mean)

#
## Encoding and plotting Red Sox data
#
redsox <- readRDS("sports.RData")
##Redsox already xts data
# View summary information about your redsox data
summary(redsox)
# Convert the date column to a time-based format
redsox$date <- as.Date(redsox$Index)

# Convert your red sox data to xts
redsox_xts <- as.xts(redsox[,-1], order.by = redsox$date)

# Plot the Red Sox score and the opponent score over time
plot.zoo(redsox_xts[, c("boston_score", "opponent_score")])

redsox_xts <- redsox
#
## Calculate a closing average
#
# Generate a new variable coding for red sox wins
redsox_xts$win_loss <- ifelse(redsox_xts$boston_score > redsox_xts$opponent_score, 1, 0)

# Identify the date of the last game each season
close <- endpoints(redsox_xts, on = "years")

# Calculate average win/loss record at the end of each season
period.apply(redsox_xts[, "win_loss"], 
             INDEX = close , FUN = mean)
#
## Calculate and plot the seasonal average
#
# Split redsox_xts win_loss data into years 
redsox_seasons <- split(redsox_xts$win_loss, f = "years")

# Use lapply to calculate the cumulative mean for each season
redsox_ytd <- lapply(redsox_seasons, cummean)

# Use do.call to rbind the results
redsox_winloss <- do.call(rbind, redsox_ytd)

# Plot the win_loss average for the 2013 season
plot.xts(redsox_winloss["2013"], ylim = c(0, 1))

#
## Calculate and plot a rolling average ( last 10 games of 2013 season)
#
# Select only the 2013 season
redsox_2013 <- redsox_xts["2013"]

# Use rollapply to generate the last ten average of 2013 season
lastten_2013 <- rollapply(redsox_2013$win_loss, width = 10, FUN = mean)

# Plot the last ten average during the 2013 season
plot.xts(lastten_2013, ylim = c(0, 1))

#
## index commands in xts
#
.index(unemployment) ## time series stored by xts in form of fractional of seconds
.indexwday(unemployment) ## weekday of each observation 0 = Sunday, 1 = Monday
sunday_games <- which(.indexwday(redsox_xts) == 0)

#
##Extract weekend games
#
sports <- redsox_xts
# Extract the day of the week of each observation
weekday <- .indexwday(sports)
head(weekday)

# Generate an index of weekend dates
weekend <- which(.indexwday(sports) == 0 | .indexwday(sports) == 6)

# Subset only weekend games
weekend_games <- sports[weekend]
head(weekend_games)

#
## Calculate a rolling average across all sports
#
# Generate a subset of sports data with only homegames
homegames <- sports[sports$homegame == 1]

# Calculate the win/loss average of the last 20 home games
homegames$win_loss_20 <- rollapply(homegames$win_loss, width = 20, FUN = mean)

# Calculate the win/loss average of the last 100 home games
homegames$win_loss_100 <- rollapply(homegames$win_loss, width = 100, FUN = mean) 

lty = c(3,1)
lwd = c(1,2)
# Use plot.xts to generate
plot.zoo(homegames[, c("win_loss_20", "win_loss_100")], plot.type = "single", lty = lty, lwd = lwd)
