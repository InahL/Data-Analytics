library(readr)
library(EnvStats)

# Set working directory
epi.data <- read_csv("C:/Users/leei6/csci4600/lab/lab1/epi2024results06022024.csv")
setwd("C:/Users/leei6/csci4600/lab/lab1/")

# Read data from CSV file
epi.data <- read_csv("epi2024results06022024.csv")

# Display the dataframe
View(epi.data)

# Print summary statistics of all variables in dataframe
summary(epi.data)

# Print values in variable
# epi.data$EPI.new
EPI.new

# Attach the dataframe and display values in variable
attach(epi.data)
EPI.new

# Find NAs inv variable
NAs <- is.na(EPI.new)

# print values in variable
MHP.new

# Find NAs inv variable
NAs <- is.na(MHP.new)

# Create a subset of NOT NAs
MHP.new.noNAs <- MHP.new[!NAs]

# Generate summary statistics
summary(EPI.new) 

# Compute fivenumber
fivenum(EPI.new,na.rm=TRUE)

# Create boxplot to compare EPI and MHP
boxplot(EPI.new, MHP.new, names = c("EPI","MHP"))

# Print histogram
hist(EPI.new) 

# Histogram with range
hist(EPI.new, seq(10., 80., 1.0), prob=TRUE) 

# Print estimated density curve and rug on histogram
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

# Print histogram
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

# Define a range
x<-seq(20,80,1) 

# Normal distribution density for mean and standard deviation
q1 <- dnorm(x,mean=42, sd=5,log=FALSE) 
q2 <- dnorm(x,mean=65, sd=5,log=FALSE) 
# Print density curve
lines(x,q1)
lines(x,.5*q1) 
lines(x,q2)
lines(x,.25*q2)

# Print ecdf
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

# Generate Q-Q plot
qqnorm(EPI.new); 
qqline(EPI.new) 

# Subset EPI.new for values less than 62
EPI.new.sub <- EPI.new[EPI.new<62]
EPI.new.sub <- epi.data$EPI.new[epi.data$EPI.new<62]

# Histogram for EPI.new.sub values
hist(EPI.new.sub, seq(20., 70., 1.0), prob=TRUE) 

# Define range for normal distribution
x<-seq(20,70,1) 

# Compute the normal density
q <- dnorm(x,mean=42, sd=5,log=FALSE) 

# Print density values
lines(x,q)
lines(x,.75*q)

# Print ecdf plot
plot(ecdf(EPI.new.sub), do.points=FALSE, verticals=TRUE) 

# Print normal random values and center EPI.new.sub
vals <- rnorm(180)
EPI.new.cent <- EPI.new.sub - mean(EPI.new.sub)
qqnorm(vals*50)
qqline(vals*50)
qqnorm(EPI.new.sub)
qqline(EPI.new.sub)

# Print Q-Q plots for variable
qqplot(rnorm(180), EPI.new.sub, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new.sub)
qqplot(rt(250, df = 150), EPI.new.sub, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new.sub)