spt <- read.csv("sptbntracker.csv", stringsAsFactors = FALSE, header = TRUE)
grp <- read.csv("grpaward.csv", stringsAsFactors = FALSE, header= TRUE)
head(spt)
#Characters to Dates
spt$PROPOSED.DATE <- as.Date(spt$PROPOSED.DATE, "%m/%d/%Y")
spt$BN.S1 <- as.Date(spt$BN.S1, "%m/%d/%Y")
spt$RETURNED.CO <- as.Date(spt$RETURNED.CO, "%m/%d/%Y")
spt$TO.G1 <- as.Date(spt$TO.G1, "%m/%d/%Y")
spt$SWCS.G1 <- as.Date(spt$SWCS.G1, "%m/%d/%Y")
spt$DATE.COMPLETE <- as.Date(spt$DATE.COMPLETE, "%m/%d/%Y")
spt$STATUS.DATE <- as.Date(spt$STATUS.DATE, "%m/%d/%Y")

#Measuring Days support battalion tracker
award.bn <- as.Date(spt$BN.S1, "%m/%d/%Y")
award.grp <- as.Date(spt$TO.G1, "%m/%d/%Y")
bn_date_diff <- (award.grp-award.bn)
award.swcs <- as.Date(spt$SWCS.G1, "%m/%d/%Y")
bn_date_diff2 <- (award.swcs-award.grp)
award.complete <- as.Date(spt$DATE.COMPLETE, "%m/%d/%Y")
award.status.date <- as.Date(spt$STATUS.DATE, "%m/%d/%Y")
award.time <- (award.status.date-award.bn)

#Measuring Days Group Tracker
award.modified <- as.Date(grp$MODIFIED, "%m/%d/%Y")
award.start.date <- as.Date(grp$START.DATE, "%m/%d/%Y")
grp_date_diff <- (award.modified - award.start.date)

#combination
spt["TIME.GRP"] <- NA
spt["TIME.SWCS"] <- NA
spt["TOTAL.TIME"] <- NA
spt$TIME.GRP <- bn_date_diff
spt$TIME.SWCS <- bn_date_diff2
spt$TOTAL.TIME <- award.time
grp["TOTAL.TIME"] <- NA
grp$TOTAL.TIME <- grp_date_diff
summary(spt)
#Plot
s <- ggplot(data = spt, aes(x = RECOMMENDED, y = TIME.SWCS, fill = STATUS)) + 
  geom_col()
s
g <- ggplot(data = grp, aes(x = UNIT, y = TOTAL.TIME, fill = STATUS)) +
  geom_col(position = "dodge")
g
