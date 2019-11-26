### Install and Load neccessary packages
if ( require(randomForest) == FALSE ) {
  install.packages("randomForest",repos="https://cloud.r-project.org")
  library(randomForest)
}

if ( require(tidyverse) == FALSE ) {
  install.packages("randomForest",repos="https://cloud.r-project.org")
  library(randomForest)
}
if ( require(mclust) == FALSE ) {
  install.packages("mclust",repos="https://cloud.r-project.org")
  library(mclust)
}
if ( require(ggpubr) == FALSE ) {
  install.packages("ggpubr",repos="https://cloud.r-project.org")
  library(mclust)
}


### Upload and Format Data
all_nba_data <- read.csv("all_nba_data_twentyfive_years.csv")
nba <- all_nba_data[!is.na(all_nba_data$Positions),]
nba[is.na(nba)] <- 0
nba$Positions <- as.factor(nba$Positions)
sapply(nba[,33],is.factor)
colnames(nba)[colnames(nba)== "ï..player_name"] <- "player_name"


### Choose Predictor Variables
nba_model <- nba %>% select(-c(Player, season, Fguse_per, ShotUse_per, FG3r, PF.48min,
                             YearsInLeague, FG3_per, FG2_per,
                             PossUsg,NetPts.48,NetPtsAR, eFG_per, AstRate,
                             StartYear,team, Positions, DR_per,
                            TO_per, BLKr, STLr, ORTG, Min,
                             OR_per, FTr, TREB_per, Floor_per))
### Relevel Factors
nba_viz <- nba_model %>% select(c(PtsProd, WinsAR, all_nba))
nba_viz$all_nba <- as.factor(nba_viz$all_nba)
nba_viz <- nba_viz %>% mutate(all_nba = fct_relevel(all_nba, "1", "0"))


### Manipulate Data to plot All-NBA Selections on top
nba_viz$PtsProd3 <- nba_viz$PtsProd
nba3 <- ifelse(nba_viz$all_nba == 1, nba_viz$PtsProd, NA)

### Visualization of Points Produced vs Wins Above Replacement
viz <- ggplot(nba_viz, aes(x = WinsAR, col = all_nba)) +
  geom_point(aes(y=PtsProd)) + geom_point(aes(y=nba3)) + 
  scale_color_manual(labels = c("All-NBA Selection", "Not"),values =c("firebrick1","skyblue")) +
  labs(x = "Wins Above Replacement",y = "Total Points Produced",
       title = "All-NBA Selections",
       subtitle = "Points Produced vs Wins Above Replacement") +
  theme(plot.title = element_text(size = 26),
        plot.subtitle = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16)) + theme_gray() +
  theme(legend.title = element_blank())


### 70% Train 30% Test
set.seed(132)
s = sample(nrow(nba_model), 0.7*nrow(nba_model))
pred.train <- nba_model[s,-c(1,6)]
pred.test <- nba_model[-s,-c(1,6)]
resp.train <- nba_model[s,6]
resp.test <- nba_model[-s,6]

### Create a Logistic Regression Model that can classify All_NBA players
allnba_mod1 <- glm(resp.train~.,data=pred.train,family=binomial)
resp.prob1 <- predict(allnba_mod1,newdata=pred.test,type="response")
resp.pred1 <- ifelse(resp.prob1>0.4,"All_NBA","Not")
t1 <- table(resp.pred1,resp.test)


### Positive Predictive Percentage
ppp1 <- t1[1,2]/(t1[1,2]+t1[1,1])
### Sensitivity
sens1 <- t1[1,2]/(t1[1,2]+t1[2,2])
### Specificity
spec1 <- t1[2,1]/(t1[1,1]+t1[2,1])
### Correctly Classified
correct1 <- (t1[1,2]+t1[2,1])/sum(t1)


### Random Forest Classification Model
set.seed(113)
out.rf = randomForest(resp.train~.,data=pred.train)
resp.prob2 = predict(out.rf,newdata=pred.test)
resp.pred2 <- ifelse(resp.prob2>0.4,"All_NBA", "Not")
t2 = table(resp.pred2,resp.test)
importance(out.rf)
varImpPlot(out.rf)

### Positive Predictive Percentage
ppp2 <- t2[1,2]/(t2[1,2]+t2[1,1])
### Sensitivity
sens2 <- t2[1,2]/(t2[1,2]+t2[2,2])
### Specificity
spec2 <- t2[2,1]/(t2[1,1]+t2[2,1])
### Correctly Classified
correct2 <- (t2[1,2]+t2[2,1])/sum(t2)

### Choose Classification Regression with 4 predictors
### Forecasting

### Load and Format 2016-2019 Data from Luka, Towns, Kyrie and Curry
add_data <- read.csv("16-19playerdata.csv")
colnames(add_data)[colnames(add_data) == "ï..player"] <- "player_name"
add_data$Positions <- as.factor(add_data$Positions)
add_data <- add_data[-14,]

### Build Dataframe
forcast <- nba %>% select(c(player_name, season, G, PtsProd,
                            DRTG, WinsAR, StartYear, YearsInLeague,
                            all_nba, team, Positions))
names(add_data) = names(forcast)
nba_data <- rbind(forcast, add_data)

# Scaling the variables
cluster_data <- nba_data %>% select(-c(player_name, season, StartYear,
                                          YearsInLeague, all_nba, team, Positions))

n_players <- nrow(cluster_data); n_stats <- ncol(cluster_data)


nba_scaled <- matrix(nrow = n_players, ncol = n_stats)
for(i in 1:n_stats)
{ 
  x <- scale(cluster_data[,i])
  nba_scaled[,i] <- x
}
scale_data <- cbind(nba_data$player_name, nba_scaled,
      nba_data[,c("StartYear", "YearsInLeague", "Positions")])
colnames(scale_data) <- c("player", "G", "PtsProd", "DRTG", "WinsAR",
                   "StartYear", "YearsInLeague", "Positions")

### similarity Metric
similarity <- function(dataset, Game, PointsProd, DefRating, WAR) {
  g <- (Game - dataset$G)^2
  p <- (PointsProd - dataset$PtsProd)^2
  d <- (DefRating - dataset$DRTG)^2
  w <- (WAR - dataset$WinsAR)^2
  sim <- sqrt(g+p+d+w)
  print(sim)
}
### Rookie Year Similarites
rookie <- scale_data %>% filter(YearsInLeague == 1)
rookie$Luka1 <- similarity(rookie, 0.6845623, 2.04463, 0.4110337, 2.866649)
rookie$Kyrie1 <- similarity(rookie, -0.1561415, 0.8941675, 0.7851926, 0.6485291)
rookie$Curry1 <- similarity(rookie, 1.00483, 1.921777, 0.8208267, 0.2860912)
rookie$Towns1 <- similarity(rookie, 1.084897, 1.774752, -0.1234789, 2.475216)

### 2nd Year Similarities
second_year <- scale_data %>% filter(YearsInLeague == 2)
second_year$Kyrie2 <- similarity(second_year, 0.1641266, 1.674816, 0.5892046, 1.373405)
second_year$Curry2 <- similarity(second_year, 0.7646293, 1.787832, 0.7673755, 1.264674)
second_year$Towns2 <- similarity(second_year, 1.084897, 2.859916, 0.5892046, 4.530239) 

### 3rd Year Similarities
third_year <- scale_data %>% filter(YearsInLeague == 3)
third_year$Kyrie3 <- similarity(third_year, 0.6445288, 2.06828, 0.5001192, 1.482136)
third_year$Curry3 <- similarity(third_year, -1.156979, -0.3176168, -0.3194668, 0.03238469)
third_year$Towns3 <- similarity(third_year, 1.084897, 2.27286, 0.05469201, 4.725955)

### 4th Year Similarities
fourth_year <- scale_data %>% filter(YearsInLeague == 4)
fourth_year$Kyrie4 <- similarity(fourth_year, 0.8046628, 2.344542, 0.4466679, 3.113107)
fourth_year$Curry4 <- similarity(fourth_year, 0.9247634, 2.643825, 0.01905784, 2.968132)
fourth_year$Towns4 <- similarity(fourth_year, 0.8847299, 2.606467, 0.2328629, 4.921672)

### 5th Year Similarities
fifth_year <- scale_data %>% filter(YearsInLeague == 5)
fifth_year$Kyrie5 <- similarity(fifth_year, -0.07607449, 0.9236773, -0.1234789, 0.8116262)
fifth_year$Curry5 <- similarity(fifth_year, 0.9247634, 3.049846, -0.6223573, 3.91047)

### 6th Year Similarities
sixth_year <- scale_data %>% filter(YearsInLeague == 6)
sixth_year$Kyrie6 <- similarity(sixth_year, 0.6845623, 2.400422, 0.9455463,2.181641)
sixth_year$Curry6 <- similarity(sixth_year, 1.00483, 2.961945, -1.228138,5.179003)

### 7th Year Similarities
seventh_year <- scale_data %>% filter(YearsInLeague == 7)
seventh_year$Kyrie7 <- similarity(seventh_year, 0.2041601, 1.666863, -0.1234789, 3.258082)
seventh_year$Curry7 <- similarity(seventh_year, 0.9647969, 3.455344, -0.6579914, 8.933859)

### 8th Year Similarities
eighth_year <- scale_data %>% filter(YearsInLeague == 8)
eighth_year$Kyrie8 <- similarity(eighth_year, 0.4843947, 2.100301, 0.2328629, 3.94309)
eighth_year$Curry8 <- similarity(eighth_year, 0.9647969, 2.846312, -0.3016497, 5.410963)

### 9th Year Similarities
ninth_year <- scale_data %>% filter(YearsInLeague == 9)
ninth_year$Curry9 <- similarity(ninth_year, -0.1561415, 1.522454, 0.2328629, 3.649515)

### 10th Year Similarities
tenth_year <- scale_data %>% filter(YearsInLeague == 10)
tenth_year$Curry10 <- similarity(tenth_year, 0.5644618, 2.408375, 0.5892046, 4.138806)

### Function to predict all_nba for rest of career
future_all_nba <- function(vector_of_players, year){
  n <- length(vector_of_players)
  pred <- matrix(nrow = n, ncol =)
  for(i in 1:n) {
    filter <- predict_forcast %>% filter(player_name == vector_of_players[i]) %>% filter(YearsInLeague > year) %>% select(-c(1,6))
    prediction_prob <- predict(allnba_mod1,newdata=filter,type="response")
    prediction <- ifelse(prediction_prob>0.5,TRUE,FALSE)
    pred[i] <- sum(prediction)
  }
  avg <- mean(pred)
  var <- var(pred)/length(vector_of_players)
  all_nba_dist <- cbind(avg,var)
  colnames(all_nba_dist) <- c("predict", "var")
  print(all_nba_dist)
  
}

predict_forcast <- forcast %>% select(c(player_name,G, PtsProd, DRTG, WinsAR, YearsInLeague))

### Luka similar player by year
luka1 <- rookie %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Luka1)

similar_players_luka <- c("Magic Johnson", "Chris Paul", "Michael Jordan",
                          "Steve Francis", "Jason Kidd", "Dominique Wilkins")

### Prediction for Luka
luka <- future_all_nba(similar_players_luka,1)

### Towns Projections
towns1 <- rookie %>% filter(Positions == 4 | Positions == 5) %>% arrange(Towns1)
towns2 <- second_year %>% filter(Positions == 4 | Positions == 5) %>% arrange(Towns2)
towns3 <- third_year %>% filter(Positions == 4 | Positions == 5) %>% arrange(Towns3)
towns4 <- fourth_year %>% filter(Positions == 4 | Positions == 5) %>% arrange(Towns4)

similar_players_towns <- c("Hakeem Olajuwon", "Patrick Ewing", "Shaquille O'Neal",
                           "Alonzo Mourning", "David Robinson", "Dirk Nowitzki")
### Prediction for Towns
towns <- future_all_nba(similar_players_towns, 4)

### Kyrie similar players by year
kyrie1 <- rookie %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie1)
kyrie2 <- second_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie2)
kyrie3 <- third_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie3)
kyrie4 <- fourth_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie4)
kyrie5 <- fifth_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie5)
kyrie6 <- sixth_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie6)
kyrie7 <- seventh_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie7)
kyrie8 <- eighth_year %>% filter(Positions == 1 | Positions == 2) %>% arrange(Kyrie8)

similar_players_kyrie <- c("Chauncey Billups", "Steve Nash", "Clyde Drexler",
                           "Gary Payton", "Tony Parker", "John Stockton")
### Predictions for Kyrie
kyrie <- future_all_nba(similar_players_kyrie, 8)

### Curry similar players by year
curry1 <- rookie %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry1)
curry2 <- second_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry2)
curry3 <- third_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry3)
curry4 <- fourth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry4)
curry5 <- fifth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry5)
curry6 <- sixth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry6)
curry7 <- seventh_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry7)
curry8 <- eighth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry8)
curry9 <- ninth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry9)
curry10 <- tenth_year %>% filter(Positions == 1 | Positions == 1.5 | Positions == 2) %>% arrange(Curry10)

similar_players_curry <- c("Reggie Miller", "Magic Johnson", "Dwyane Wade",
                           "Kobe Bryant", "Chauncey Billups", "Steve Nash")

### Prediction for Curry
curry <- future_all_nba(similar_players_curry, 10)

### Likelihood each player is the highest
### Luka
### six prediction from the six similar players (a part of the function)
luka <- c(10,7,10,2,1,8)
curry <- c(0,2,0,6,1,3)
kyrie <- c(2,4,2,3,1,2)
towns <- c(9,7,8,1,5,9)
likelihood_of_greatest<- function(greatest, p2, p3, p4) {
  x <- t.test(greatest, p2, alternative = "greater", var.equal = FALSE)                     
  y <- t.test(greatest, p3, alternative = "greater", var.equal = FALSE)
  z <- t.test(greatest, p4, alternative = "greater", var.equal = FALSE)
  prob <- (1-x$p.value)*(1-y$p.value)*(1-z$p.value)
  print(prob)
}
### Likelihood of Luka
likelihood_of_greatest(luka, kyrie, towns, curry)

### Likelihood of Kyrie
likelihood_of_greatest(kyrie, luka, towns, curry)

### Likelihood of Curry
likelihood_of_greatest(curry, luka, towns, kyrie)

### Likelihhod of Towns
likelihood_of_greatest(towns, kyrie, luka, curry)
