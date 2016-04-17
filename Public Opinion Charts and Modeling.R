library(gdata)  
library(ggplot2)
library(ggthemes)
library(reshape)
library(gridExtra)
library(xlsx)

# Constructing plot 1: Level of personal worry about drug use

drugsWorry <- read.xlsx("Drugs.xlsx", sheetIndex = 1)

x <- drugsWorry$Year
y1 <- drugsWorry$Great_deal
y2 <- drugsWorry$Fair_amount
y3 <- drugsWorry$Only_a_little
y4 <- drugsWorry$Not_at_all
drugplotWorry <- data.frame(x,y1,y2,y3,y4)

p1 <- ggplot(drugplotWorry, aes(x)) +                 
  geom_line(aes(y = y1, colour = "A great deal")) + 
  geom_line(aes(y = y2, colour = "Fair amount")) + 
  geom_line(aes(y = y3, colour = "Only a little")) + 
  geom_line(aes(y = y4, colour = "Not at all")) + 
  ggtitle("Personal worry about drug use in America") +
  scale_x_continuous(breaks = c(2001,2005,2010,2015),limits = c(2001,2015))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60),limits = c(0,60))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# Constructing plot 2: Opinon on the severity of the drug problem in America

drugsProblem <- read.xlsx("Drugs.xlsx", sheetIndex = 2)

xyear <- drugsProblem$Year
yex <- drugsProblem$Extremely_serious
yvs <- drugsProblem$Very_serious
yms <- drugsProblem$Moderately_serious
ynts<- drugsProblem$Not_too_serious
ynsa <- drugsProblem$Not_serious_at_all

drugplotProblem <- data.frame(xyear,yex,yvs, yms, ynts, ynsa)

p2 <- ggplot(drugplotProblem, aes(xyear)) +                    
  geom_line(aes(y = yex, colour="Extremely Serious")) +     
  geom_line(aes(y = yvs, colour="Very Serious")) +  
  geom_line(aes(y = yms, colour="Moderately Serious")) + 
  geom_line(aes(y = ynts, colour="Not too serious")) + 
  geom_line(aes(y = ynsa, colour="Not serious at all")) + 
  ggtitle("Severity of the problem of drug use in America") +
  scale_x_continuous(breaks = c(2000,2003,2006,2009),limits = c(2000,2009))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50),limits = c(0,50))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) 

# Constructing plot 3: Percantage of people describing drug abuse as the most serious problem facing America

drugsMostProblem <- read.xlsx("Drugs.xlsx", sheetIndex = 3)

xyr <- drugsMostProblem$Year
yp <- drugsMostProblem$Percentage
drugplotMostProblem <- data.frame(xyr, yp)

p3 <- ggplot(drugplotMostProblem, aes(xyr)) +       
  geom_line(aes(y = yp), colour="red") +        
  ggtitle("Drug Abuse Most Serious Problem Facing America") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits=c(1989,2010))+
  ylim(c(0,30)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) 

# Constructing plot 4: Percentage of people describing drug abuse as the most serious problem facing schools

drugschools<- read.xlsx("Drugs.xlsx", sheetIndex = 7)

xsch <- drugschools$Year
ysch <- drugschools$Percentage
drugplotMostProblem <- data.frame(xsch, ysch)

p4 <- ggplot(drugplotMostProblem, aes(xyr)) +       
  geom_line(aes(y = ysch), colour="orange") +        
  ggtitle("Drugs Most Serious Problem in Schools") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits = c(1989,2010))+
  ylim(c(0,40)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) 

# Initial construction of the model

drugsMod1 = read.xlsx("Drugs.xlsx", sheetIndex = 6)

ann <- drugsMod1$Year
pmp <- drugsMod1$PercentMostProblem
pmood <- drugsMod1$Mood
arrests <- drugsMod1$Rate
pun <- drugsMod1$punitiveness
sch <- drugsMod1$schools
fresh <- drugsMod1$freshmen

mod1frame <- data.frame(ann,pmp,pmood,arrests,pun,sch,fresh)

# Constructing plot 5: Public desire to be "tough on crime"

p5 <- ggplot(mod1frame, aes(ann)) +      
  geom_line(aes(y = pun), colour = "purple") +      
  ggtitle("Public Punitiveness") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits = c(1989,2010))+
  scale_y_continuous(breaks = c(0,25,50,75),limits = c(0,75))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# doing some correlations
cor(mod1frame$arrests, mod1frame$pmp)
cor(mod1frame$arrests, mod1frame$sch)
cor(mod1frame$arrests, mod1frame$pmood)
cor(mod1frame$arrests, mod1frame$pun)
cor(mod1frame$arrests, mod1frame$fresh)

# Constructing plot 6: Drug Possesion/Use Arrest rate per 100,000

drugsArrestRate <- read.xlsx("Drugs.xlsx", sheetIndex = 4)

xpyr <- drugsArrestRate$Year
ypp <- drugsArrestRate$Rate
drugplotRate <- data.frame(xpyr, ypp)

p6 <- ggplot(drugplotRate, aes(xpyr)) +      
  geom_line(aes(y = ypp), colour="purple") +      
  ggtitle("Drug Possesion/Use Arrest rate per 100,000") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits = c(1990,2010))+
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600),limits = c(0,600))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# Constructing plot 7: College freshman on marijuana legalization ("agree strongly" or "agree somewhat")

freshmen <- read.xlsx("Drugs.xlsx", sheetIndex = 8)

fy <- freshmen$Year
fp <- freshmen$Percentage
freshmen_legal <- data.frame(fy, fp)

p7 <- ggplot(freshmen_legal, aes(fy)) +      
  geom_line(aes(y = fp), colour="blue") +      
  ggtitle("College Freshman Supporting Legalization of Marijuana  ") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits = c(1990,2010))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50),limits = c(0,50))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# Constructing plot 8: General measures of public opinion

p8 <- ggplot(mod1frame, aes(ann)) +      
  geom_line(aes(y = pun, colour = "Public Punitiveness")) +   
  geom_line(aes(y = pmp, colour = "Drug Abuse Most Serious Problem")) +  
  geom_line(aes(y = pmood, colour = "Drug Mood")) + 
  geom_line(aes(y = sch, colour = "Drugs Most Serious Problem in Schools")) + 
  geom_line(aes(y = fresh, colour = "College Freshman Supporting Legalization of Marijuana")) + 
  ggtitle("General Measures of Public Opinion") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010),limits = c(1989,2010)) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100),limits = c(0,100)) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())


# Show different graphs together

grid.arrange(p1, p2, ncol=1)

grid.arrange(p3, p4,p5, p6, ncol=2)

grid.arrange(p8,p6, ncol=1)

#linear model

#This includes the general public mood and college freshmen opinions, which
#run counter to the narrative of drug punitiveness, and thus reduce the 
#effectiveness of the model

mod1.rate <- lm(arrests~pmp + pmood + pun + sch + fresh, data = drugsMod1)

summary(mod1.rate)

#Remove the pmood and freshmen variables
mod1.rate <- lm(arrests~pmp + pun + sch , data = drugsMod1)

# Plotting the residuals

plot(y = mod1.rate$residuals, x = mod1.rate$fitted.values, xlab = "fitted values", ylab = "residuals")

