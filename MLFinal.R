# read data in from an Excel spreadsheet
library(readxl)
imdb.data <- read_excel("C:/Users/maura/Downloads/IMDb_Data.xlsx")

####################################################################
# look at summaries of eleven variables
# 1. IMDbRating
# look at numerical summary
summary(imdb.data$IMDbRating)
# Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
# 7.500   7.700   7.900   7.957   8.100   9.300 
# make a histogram to look at the spread (Figure 1)
hist(imdb.data$IMDbRating, freq=F, xlab="IMDb Rating", 
     main="Histogram of Top 1000 Films' Ratings")

# 2. Director
# sort the directors by number of films they have in the dataset
sort(table(imdb.data$Director))

# 3. TopFourStars
# get all actors' names into a list
four.thousand.actors <- NULL
actor1s <- rep(NA,1000); actor2s <- rep(NA,1000)
actor3s <- rep(NA,1000); actor4s <- rep(NA,1000)
for(i in 1:1000){
  four.actors <- strsplit(imdb.data$TopFourStars[i], split=", ")
  actor1s[i] <- four.actors[[1]][1] # store first actors
  actor2s[i] <- four.actors[[1]][2] # store second actors
  actor3s[i] <- four.actors[[1]][3] # store third actors
  actor4s[i] <- four.actors[[1]][4] # store fourth actors
}
imdb.data$Actor1 <- actor1s # add 1st actors to dataframe
imdb.data$Actor2 <- actor2s # add 2nd actors to dataframe
imdb.data$Actor3 <- actor3s # add 3rd actors to dataframe
imdb.data$Actor4 <- actor4s # add 4th actors to dataframe
four.thousand.actors <- c(actor1s, actor2s, actor3s, actor4s)
# determine which actors are in more than 10 movies
big.actors <- table(four.thousand.actors)>10 # T if in more than 10
big.actors.names <- NULL # store names of those in more than 10 here
for(i in 1:2689){ # for each of the 2689 unique actors
  if(big.actors[i]==T){ # if the actor has been in more than 10
    big.actors.names <- c(big.actors.names, big.actors[i]) # store
  }
}
big.actors.names # look at the actors that were in more than 10
sum(four.thousand.actors=="AlPacino") # Al Pacino in 13
sum(four.thousand.actors=="BradPitt") # Brad Pitt in 11
sum(four.thousand.actors=="ChristianBale") # Christian Bale in 11
sum(four.thousand.actors=="JamesStewart") # James Stewart in 11
sum(four.thousand.actors=="LeonardoDiCaprio") # Leo DiCaprio in 11
sum(four.thousand.actors=="MattDamon") # Matt Damon in 11
sum(four.thousand.actors=="RobertDeNiro") # Rob De Niro in 16
sum(four.thousand.actors=="TomHanks") # Tom Hanks in 14

# 4. Genre
# put all genres into one list
all.genres <- NULL
genre1s <- rep(NA, 1000); genre2s <- rep(NA, 1000) 
genre3s <- rep(NA, 1000) # store up to 3 genres for each film
for(i in 1:1000){
  genres <- strsplit(imdb.data$Genre[i],split=",")
  genre1s[i] <- genres[[1]][1] # first listed genre
  genre2s[i] <- genres[[1]][2] # second listed genre
  genre3s[i] <- genres[[1]][3] # third listed genre
}
all.genres <- c(genre1s, genre2s, genre3s) # make list of all genres
all.genres <- na.omit(all.genres) # get rid of NAs
length(all.genres)/1000 # 2539/1000 = average of 2.5 genres/film
# look at how many films were in each genre (Table 4)
table(all.genres)
# change genre to factor of Action, Comedy, Crime, Drama, or Misc.
genre1 <- rep("Misc.", 1000)
for(i in 1:1000){
  if     (genre1s[i]=="Action"){genre1[i] <- "Action"}
  else if(genre1s[i]=="Comedy"){genre1[i] <- "Comedy"}
  else if(genre1s[i]=="Crime"){genre1[i] <- "Crime"}
  else if(genre1s[i]=="Drama"){genre1[i] <- "Drama"}
}
imdb.data$Genre1 <- as.factor(genre1) # add 1st genre to dataframe
# look at table of first-listed genres (Table 5)
table(imdb.data$Genre1)
# make boxplot to look at IMDbRating spread for each genre (Figure 2)
plot(imdb.data$IMDbRating ~ imdb.data$Genre1, 
     xlab = "First Genre", ylab = "IMDb Rating", 
     main="Top 1000 Films' IMDb Ratings vs. Most Popular 1st Genres")

# 5. Duration 
# look at numerical summary
summary(imdb.data$Duration)
# Min.  1st Qu.  Median  Mean   3rd Qu.   Max. 
# 45.0   103.0   120.0   123.7   138.0   321.0 
# make a histogram to look at the spread (Figure 3)
hist(imdb.data$Duration, freq=F, xlab="Minutes", 
     main="Histogram of Top 1000 Films' Durations")
# scatterplot to look at relationship between rating and runtime
# with slightly jittered points (Figure 4)
set.seed(12)
dur.jit <- runif(1000, -1, 1) # for Duration
set.seed(12)
rat.jit <- runif(1000, -.05, .05) # for IMDbRating
plot((imdb.data$Duration+dur.jit), (imdb.data$IMDbRating+rat.jit), 
     xlab="Duration (mins)", ylab = "IMDb Rating", pch=20,
     main = "Top 1000 Films' Ratings vs. Durations", cex=.8)
# add a lowess line to visualize the trend
lines(lowess(imdb.data$Duration, imdb.data$IMDbRating), col="blue") 
# look at correlation between rating and runtime
cor(imdb.data$Duration, imdb.data$IMDbRating) # 0.26
# scale Duration to standardize it
imdb.data$Duration <- scale(imdb.data$Duration)

# 6. AgeRating
# see how many films were in each rating category (Table 6)
table(imdb.data$AgeRating)
# G   NC-17   NR   Passed  PG    PG-13    R 
# 39    5    189     78    151    171    367
# look at IMDb rating spread for each category (Figure 5)
boxplot(imdb.data$IMDbRating ~ imdb.data$AgeRating, 
        xlab="Age Rating", ylab="IMDb Rating", 
        main = "Top 1000 Films' IMDb Ratings vs. Age Ratings")
# convert the variable to a factor in the dataframe
imdb.data$AgeRating <- as.factor(imdb.data$AgeRating)

# 7. ReleaseYear
# look at numerical summary
summary(imdb.data$ReleaseYear)
# Min.   1st Qu. Median   Mean   3rd Qu.  Max. 
# 1920    1975    1999    1991    2010    2023 
# make histogram to look at the spread (Figure 6)
hist(imdb.data$ReleaseYear, freq=F, xlab="Year", 
     main = "Histogram of Top 1000 Films' Release Years")
# make scatterplot to look at rating and year, w/ jitter (Figure 7)
set.seed(12)
year.jit <- runif(1000,-1,1)
plot((imdb.data$ReleaseYear+year.jit),(imdb.data$IMDbRating+rat.jit), 
     xlab="Year", ylab = "IMDb Rating", pch=20,
     main = "Top 1000 Films' Ratings vs. Release Years")
# add a lowess line to the plot to visualize the trend
lines(lowess(imdb.data$ReleaseYear,imdb.data$IMDbRating), col="blue") 
# look at the correlation between rating and release year
cor(imdb.data$ReleaseYear, imdb.data$IMDbRating) # -0.10
# use ReleaseYear to make ReleaseDecade and add to dataframe
decade <- rep(NA, 1000)
for(i in 1:1000){
  if     (imdb.data$ReleaseYear[i]<1930){decade[i]<-"1920s"}
  else if(imdb.data$ReleaseYear[i]<1940){decade[i]<-"1930s"}
  else if(imdb.data$ReleaseYear[i]<1950){decade[i]<-"1940s"}
  else if(imdb.data$ReleaseYear[i]<1960){decade[i]<-"1950s"}
  else if(imdb.data$ReleaseYear[i]<1970){decade[i]<-"1960s"}
  else if(imdb.data$ReleaseYear[i]<1980){decade[i]<-"1970s"}
  else if(imdb.data$ReleaseYear[i]<1990){decade[i]<-"1980s"}
  else if(imdb.data$ReleaseYear[i]<2000){decade[i]<-"1990s"}
  else if(imdb.data$ReleaseYear[i]<2010){decade[i]<-"2000s"}
  else if(imdb.data$ReleaseYear[i]<2020){decade[i]<-"2010s"}
  else if(imdb.data$ReleaseYear[i]<2024){decade[i]<-"2020s"}
}
imdb.data$ReleaseDecade <- as.factor(decade)
table(decade) # look at summary of decades (Table 7)
# make boxplot of IMDbRating vs. ReleaseDecade (Figure 8)
boxplot(imdb.data$IMDbRating~imdb.data$ReleaseDecade, 
        xlab = "Decade of Release", ylab = "IMDb Rating", 
        main = "Top 1000 Films' IMDb Ratings vs. Release Decade")

# 8. Language
# see how many films were in each language (Table 8)
table(imdb.data$Language)
# see spread of rating for languages with > 15 films (Figure 9)
pop.langs <- ifelse( imdb.data$Language=="English" | 
     imdb.data$Language=="French" | 
                     imdb.data$Language=="German" | 
     imdb.data$Language=="Hindi" | 
                     imdb.data$Language=="Japanese" | 
     imdb.data$Language=="Spanish", 1, 0)
boxplot(imdb.data$IMDbRating[pop.langs==1] ~ 
        imdb.data$Language[pop.langs==1], 
        xlab="Language", ylab="IMDb Rating", 
        main="Boxplot of IMDb Rating vs. Language")
# modify Language to have English, Hindi, Japanese, French, or Misc.
language <- rep("Misc.", 1000)
for(i in 1:1000){
  if     (imdb.data$Language[i]=="English"){language[i]<-"English"}
  else if(imdb.data$Language[i]=="Hindi"){language[i]<-"Hindi"}
  else if(imdb.data$Language[i]=="Japanese"){language[i]<-"Japanese"}
  else if(imdb.data$Language[i]=="French"){language[i]<-"French"}
}
# store languages as a factor in the dataframe
imdb.data$Language <- as.factor(language)

# 9. Metacritic Score
# store Metacritic Score as an int, not a char, in dataframe
imdb.data$MetacriticScore <- as.integer(imdb.data$MetacriticScore)
# look at numerical summary
summary(imdb.data$MetacriticScore)
#  Min.  1st Qu.  Median  Mean   3rd Qu.  Max.    NA's 
# 28.00   71.00   80.00   78.79   88.00  100.00   149 
# make separate dataset with films that have MC scores
MC.score.films <- na.omit(imdb.data)
# make histogram to look at spread (Figure 10)
hist(MC.score.films$MetacriticScore,
     xlab = "Metacritic Score", freq=F,
     main = "Histogram of Top 1000 Films' Scores on Metacritic")
# make scatterplot for IMDb score and Metacritic score (Figure 11)
set.seed(12) # jitter Metacritic Scores
mc.jit <- runif(851, -1, 1)
set.seed(12) # jitter IMDb Ratings
rat.jit <- runif(851, -.05, .05)
plot((MC.score.films$MetacriticScore+mc.jit),
     (MC.score.films$IMDbRating+rat.jit), pch=20, 
     xlab = "Metacritic Score", ylab = "IMDb Rating", 
     main = "Top 1000 Films' Ratings vs. Metacritic Scores")
# add lowess line to plot to visualize trend
lines(lowess(MC.score.films$MetacriticScore, 
      MC.score.films$IMDbRating), col="blue") 
# correlation between critics’ ratings and audiences’ ratings
cor(MC.score.films$MetacriticScore, MC.score.films$IMDbRating)#0.25
# scale MetacriticScore to standardize it
imdb.data$MetacriticScore <- scale(imdb.data$MetacriticScore)

# 10. Lead Gender
# see how many films had a lead actor of different genders
table(imdb.data$LeadGender)
#  F   M 
# 169 831
# look at spread of ratings for different genders (Figure 12)
boxplot(imdb.data$IMDbRating ~ imdb.data$LeadGender, 
        xlab="Lead Actor's Gender", names=c("Female","Male"), 
        ylab = "IMDb Rating", 
   main="Boxplot of IMDb Rating vs. Lead Gender")
# store variable as a factor in dataframe
imdb.data$LeadGender <- as.factor(imdb.data$LeadGender)

# 11. Lead Race
# see how many films had a lead actor of different races
table(imdb.data$LeadRace)
# Asian    Black     Latino   Pacific Islander    White 
# 170       31         27            2             770 
# look at spread of ratings for different races (Figure 13)
boxplot(imdb.data$IMDbRating ~ imdb.data$LeadRace, 
        xlab="Lead Actor's Race",
        names=c("Asian","Black","Latino","Pac. Isl.","White"),
        main="Boxplot of IMDb Rating vs. Lead Race")
# change LeadRace to White, Asian, Black, or Latino/Pac.Isl.
race <- rep(NA, 1000)
for(i in 1:1000){
  if     (imdb.data$LeadRace[i]=="White"){race[i]<-"White"}
  else if(imdb.data$LeadRace[i]=="Asian"){race[i]<-"Asian"}
  else if(imdb.data$LeadRace[i]=="Black"){race[i]<-"Black"}
  else if(imdb.data$LeadRace[i]=="Latino" | 
          imdb.data$LeadRace[i]=="Pacific Islander")
          {race[i]<-"Latino/Pac.Isl."}
}
# store variable as a factor in dataframe
imdb.data$LeadRace <- as.factor(race)

#####################################################################
# fit models to predict IMDbRating on training data
# start at seed 12, then use seed 24, then seed 42, then seed 22
set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
train.rows <- sample(1:1000, 750)
train.data <- imdb.data[train.rows,]
test.data <- imdb.data[-train.rows,]

# 1. Linear Regression
# fit linear regression model
lm.mod <- lm(IMDbRating ~ Genre1 + Duration + AgeRating + Language + 
             ReleaseDecade + MetacriticScore + LeadGender + LeadRace, 
             data=train.data)
summary(lm.mod) # look at parameter estimates and p-values
lm.mod.preds <- predict(lm.mod,na.omit(test.data)) # predict for test
mean((lm.mod.preds - na.omit(test.data)$IMDbRating)^2) # test MSE

# 2. Ridge Regression
library(glmnet)
# set up x and y matrices for training data
train.x <- model.matrix(IMDbRating ~ Genre1 + Duration + AgeRating + 
                        ReleaseDecade + Language + MetacriticScore + 
                        LeadGender + LeadRace, data=train.data)
train.y <- na.omit(train.data)$IMDbRating
set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
grid <- 10^seq(1,-5,length=500) # grid of possible lambda values
# determine best value of lambda for this dataset
ridge.cv.out <- cv.glmnet(train.x, train.y, alpha=0, lambda=grid)
ridge.bestlam <- ridge.cv.out$lambda.min
# run ridge model on training set with best lambda
ridge.mod <- glmnet(train.x, train.y, alpha=0, lambda=ridge.bestlam, 
                    thresh = 1e-15)
# set up x and y matrices for test data
test.x <- model.matrix(IMDbRating ~ Genre1 + Duration + AgeRating + 
                      ReleaseDecade + Language + MetacriticScore + 
                      LeadGender + LeadRace, data=test.data)
test.y <- na.omit(test.data)$IMDbRating
# predict IMDbRating and calculate MSE for test data
ridge.pred <- predict(ridge.mod, s=ridge.bestlam, newx=test.x)
mean((ridge.pred - test.y)^2)
ridge.mod$beta[order(ridge.mod$beta),] # parameter estimates

# 3. Lasso Regression
set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
# determine best value of lambda for this dataset
lasso.cv.out <- cv.glmnet(train.x, train.y, alpha=1, lambda=grid)
lasso.bestlam <- lasso.cv.out$lambda.min
# run lasso model on training data
lasso.mod <- glmnet(train.x, train.y, alpha=1, lambda=lasso.bestlam, 
                    thresh = 1e-15)
# predict IMDbRating and calculate MSE for test data
lasso.pred <- predict(lasso.mod, s=lasso.bestlam, newx=test.x)
mean((lasso.pred - test.y)^2) 
lasso.mod$beta[order(lasso.mod$beta),] # parameter estimates

# 4. Best Subset Selection
library(leaps)
set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
# see which 10 variables are determined to be most important
bss.mod <- regsubsets(IMDbRating ~ Genre1 + Duration + AgeRating + 
                      ReleaseDecade + Language + MetacriticScore + 
                      LeadGender + LeadRace, data=train.data, 
                      nvmax = 10)
summary(bss.mod)
# fit the best 3 variable model and make predictions for test data
bss.lm.mod <- lm(IMDbRating ~ MetacriticScore + Duration + 
                 ReleaseDecade, data=train.data)
summary(bss.lm.mod)
bss.lm.mod.preds <- predict(bss.lm.mod, na.omit(test.data)) 
mean((bss.lm.mod.preds - na.omit(test.data)$IMDbRating)^2) #test MSE

# 5. Regression Trees and Random Forest
library(rpart)
library(rpart.plot)
library(randomForest)
set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
# make a tree from training data, prune it, and plot it (Figure 14)
tree <- rpart(IMDbRating ~ Genre1 + Duration + AgeRating + 
              ReleaseDecade + Language + MetacriticScore + LeadGender 
              + LeadRace, data=train.data, method='anova',
              control=rpart.control(minsplit=50,cp=.01,xval=10))
tree.pruned <- prune(tree, cp=.01)
prp(tree.pruned, faclen=0, extra=1, roundint=F, digits=5)
# predict IMDbRating for test data and find test MSE
tree.preds <- predict(tree.pruned, test.data)
mean((tree.preds - test.data$IMDbRating)^2)

set.seed(12)
set.seed(24)
set.seed(42)
set.seed(22)
# run randomForest, trying 5 variables at each split
tree.RF <- randomForest(IMDbRating ~ Genre1 + Duration + AgeRating + 
                        ReleaseDecade + Language + MetacriticScore + 
                        LeadGender + LeadRace, mtry = 5,
                        data=na.omit(train.data), importance=TRUE)
# see which variables were deemed most important (Figure 15)
importance(tree.RF); varImpPlot(tree.RF)
# predict IMDbRating for test data and find test MSE
tree.RF.preds <- predict(tree.RF, na.omit(test.data))
mean((tree.RF.preds - na.omit(test.data)$IMDbRating)^2)

##################################################################
# try to fit models to new test data
train.data <- imdb.data
test.data<-read_excel("C:/Users/maura/Documents/IMDb_Data_New.xlsx")
test.data$AgeRating <- as.factor(test.data$AgeRating)
test.data$ReleaseDecade <- as.factor(test.data$ReleaseDecade)
test.data$Genre1 <- as.factor(test.data$Genre1)
test.data$Language <- as.factor(test.data$Language)
test.data$LeadGender <- as.factor(test.data$LeadGender)
test.data$LeadRace <- as.factor(test.data$LeadRace)
test.data$Duration <- scale(test.data$Duration)
test.data$MetacriticScore <- scale(test.data$MetacriticScore)

# re-run linear regression, best subset selection, and regression 
# trees using the above code
# get errors when trying to re-run ridge, lasso, and random forest
# using the above code

# visualize standard linear reg. model’s predictions (Figure 16)
plot(test.data$IMDbRating, lm.mod.preds, xlab="Actual Test Ratings",
     ylab = "Predicted Test Ratings", pch=20,
     main = "Predicted vs. Actual IMDbRatings for Test Data")
abline(0,1) # add the line y=x to the plot
