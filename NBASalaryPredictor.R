################################
# CAPSTONE - NBASalaryPredictor - Brandon Rufino
################################

################################
# Install packages
################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos ="http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

################################
# Prepare Data
################################

salary_table <- 
  read.csv("input/salary/NBA_season1718_salary.csv")
stats <- read.csv("input/nba-players-stats/Seasons_Stats.csv")
head(salary_table)
head(stats)

################################
# Data Preprocessing
################################

# must mutate some columns to get MPG, PPG, APG, RPG, TOPG, BPG, SPG
stats_1617 <- 
  stats %>% filter(Year >= 2017) %>% 
  select(Year:G, MP, PER, FG:PTS) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  mutate(MPG = MP/G, PPG = PTS/G, APG = AST/G, 
         RPG = TRB/G, TOPG = TOV/G, BPG = BLK/G, 
         SPG = STL/G) 
head(stats_1617)

#merge by player and name column
stats1617_salary1718 <- merge(stats_1617, salary_table, by.x = "Player", by.y = "Player")
names(stats1617_salary1718)[40] <- "salary_1718"
#remove tm.y column
stats1617_salary1718 <- stats1617_salary1718[-39]
head(stats1617_salary1718)

################################
# Data Exploration
################################

#create correlation plot of players salary with their MPG, PPG, APG, RPG, TOPG, BPG, SPG, Age, and PER
corrplot(cor(stats1617_salary1718 %>% 
               select(salary_1718, MPG:SPG, 
                      Age, PER, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")

stats_salary_cor <- 
  stats1617_salary1718 %>% 
  select(salary_1718, PPG, MPG, TOPG, RPG, PER, SPG, APG)
ggpairs(stats_salary_cor)
cor(stats_salary_cor)[,"salary_1718"]

#name the team column Team
names(stats1617_salary1718)[5] <- "Team"
# #plot salary vs ppg with team as different groups
stats1617_salary1718 %>% 
  ggplot(aes(x = salary_1718, y = PPG, color=Team, label=Player)) + 
  geom_point() +geom_text(check_overlap = TRUE)

#create regression variable
stats_salary_regression <- 
  stats1617_salary1718 %>% select(salary_1718, MPG:SPG)

#find the average MPG and TOPG
avg.minutes <- mean(stats_salary_regression$MPG)
avg.turnover <- mean(stats_salary_regression$TOPG)

#create a trusted column if players get above average minutes
stats1617_salary1718$trusted <- as.factor(ifelse(stats1617_salary1718$MPG >= avg.minutes, "Yes", "No"))

#create a aggressiveness column if players turn over the ball more then the average
stats1617_salary1718$agressiveness <- as.factor(ifelse(stats1617_salary1718$TOPG >= avg.turnover, "Yes", "No"))

################################
# Models
################################


#considers points per game in function
salary_prediction_model1 <- function(m, points){
  pre_new <- predict(m, data.frame(PPG = points))
  msg <- paste("PPG:", points, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}
#create model
model1<- lm(salary_1718~PPG, data=stats1617_salary1718)
#predict salary
predict1<-salary_prediction_model1(model1, 25.0)


#considers points per game in function
salary_prediction_model2 <- function(m, minutes, points, assists, rebounds, turnovers, blocks, steals){
  pre_new <- predict(m, data.frame(PPG = points, MPG=minutes, APG=assists,
                                   RPG=rebounds, TOPG=turnovers, BPG=blocks,
                                   SPG=steals))
  msg <- paste("PPG:", points, "RPG:", rebounds, "MPG:", minutes, "APG:", assists, "TOPG:", turnovers, "BPG:", blocks, "SPG:", steals, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}
#create model
model2<- lm(salary_1718~., data=stats_salary_regression)
#predict salary
predict2<-salary_prediction_model2(model2, 36.9, 25.0, 3.8, 8.6, 2.9, 0.7, 0.9)


#considers points per game in function
salary_prediction_model3 <- function(m, trusted, agressiveness){
  pre_new <- predict(m, data.frame(trusted = trusted, agressiveness = agressiveness))
  msg <- paste("Trusted:", trusted, "Agressive:", agressiveness, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}
model3<-lm(formula = salary_1718 ~ trusted * agressiveness, data=stats1617_salary1718)
predict3<-salary_prediction_model3(model3, "Yes", "Yes")