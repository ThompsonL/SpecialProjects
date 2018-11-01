getwd()
manpower <- read.csv("Manpower.csv")
taskings <- read.csv("TaskingTracker.csv")
library(ggplot2)
library(scales)
factor(manpower$Year)
factor(manpower$Ots)
factor(manpower$Month)
#manpower
m <- ggplot(data = manpower, aes(x = Month, y = Ots, color = Year))
m + geom_point()

s <- ggplot(data = manpower, aes(x = Month, y = Ots, fill = Year)) + 
  geom_col(position = "dodge")
s +
  xlab("Months") +
  ylab("OTs Available") +
  theme(axis.title.x = element_text(color="Black", size=30), 
        axis.title.y=element_text(color="Black", size= 30),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        legend.title=element_text(size=30),
        legend.text=element_text(size=20),
        legend.position=c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="Black", size=40, 
                                  family = "Arial", hjust = .5))

manpower_comparables <- (manpower[3,3] - manpower[15,3])
manpower_comparables / manpower[3,3]

#taskings
factor(taskings$Status)
factor(taskings$Suspense.Date)
t <- ggplot(data = taskings, aes(x = Status)) +
  geom_bar()
t +   
  xlab("Status") +
  ylab("Count") +
  theme(axis.title.x = element_text(color="Black", size=30), 
        axis.title.y=element_text(color="Black", size= 30),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        legend.title=element_text(size=30),
        legend.text=element_text(size=20),
        legend.position=c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="Black", size=40, 
                                  family = "Arial", hjust = .5))

t2 <- ggplot(data = taskings, aes(x = Status, y = Suspense.Date)) +
  geom_jitter(aes(color = Status))
t2 +
xlab("Status") +
  ylab("Count") +
  ggtitle("Historic Taskings Supported by Support BN") +
  theme(axis.title.x = element_text(color="Black", size=30), 
        axis.title.y=element_text(color="Black", size= 30),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        legend.title=element_text(size=30),
        legend.text=element_text(size=20),
        legend.position=c(.8,.05),
        legend.justification = c(.2,.1),
        plot.title = element_text(color="Black", size=40, 
                                  family = "Arial", hjust = .5))

#diverging bars
t3df <- data.frame(taskings$Status, taskings$Suspense.Date)
head(t3df)
t3r <- split(t3df, t3df$taskings.Status, drop = F)
summary(t3r)
t3r
r <- t3r$RECLAMMA
r
sup <- t3r$SUPPORTING
comp <- t3r$COMPLETE
ac <- t3r$`AWAITING COMMITMENT`
canc <- t3r$CANCELED
cont <- t3r$CONTINUOUS
rl <- lengths(r, use.names = T)
supl <- lengths(sup, use.names = T)
compl <- lengths(comp, use.names = T)
acl <- lengths(ac, use.names = T)
cancl <- lengths(canc, use.names = T)
contl <- lengths(cont, use.names = T)

total <- acl + cancl + compl+ contl + rl + supl
rpercentage <- (rl / total)
percent(rpercentage)

ggplot(data = rpercentage, aes(x = rpercentage$taskings.Status)) +
         geom_bar() +
         scale_fill_manual() +
         labs(subtitle = "Diverging Bars") +
         coord_flip()

#visualization
task_manpower <- .63
acl_percent <-.01
cancl_percent <- .01
compl_percent <- .90
rl_percent <- .01
contl_percent <- .03
supl_percent <- .03

