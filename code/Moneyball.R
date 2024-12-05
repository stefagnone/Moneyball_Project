# Assignment: Moneyball Capstone Project 

# Set path to retrieve files
getwd()
setwd("/Users/veronicahurtado/Desktop/Hult/Visualizing and Analyzing Data with R/Team Assignment")

# Loading necessary libraries
library(dplyr)

# Retrieving csv files.
batting <- read.csv("Batting.csv")  
salaries <- read.csv("Salaries.csv")  

# Checking first rows of data base.
head(batting)  
head(salaries)

# Exploring databases.
summary(batting)
summary(salaries)

# Checking data types of each table.
str(batting)
str(salaries)

# Checking for null values in each database.
any(is.na(batting))
any(is.na(salaries))

# Checking which columns and rows in batting are the ones with missing values.
colSums(is.na(batting)) 

# Due to the high amount of NULL values, handling them in a reliable manner is not possible. 
# Therefor, they will be ignored from the analysis.

# The following metrics were chosen to make the optimal decision about the substitution of the players:
# 1. Batting Average (BA): Measures how often a player gets a hit.
batting$BA <- batting$H / batting$AB

# 2. On-Base Percentage (OBP): Measures how often a player gets on base.
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF) 

# 3. Number of singles (X1B): This metric is fundamental for analyzing a player's hitting ability, focusing on their capacity to get on base with basic hits
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# 4. Slugging Percentage (SLG): Measures the total number of bases a player records per at-bat.
batting$SLG <- (batting$X1B + 2 * batting$X2B + 3 * batting$X3B + 4 * batting$HR) / batting$AB 

# 5. On-Base Plus Slugging (OPS): Combines OBP and SLG to measure a player's overall offensive performance.
batting$OPS <- batting$OBP + batting$SLG


# Only players with available information on the salary table will be taken into consideration for the analysis, since this information is needed to determine the new players. 

# For budgeting purposes, two assumptions were made:
# 1. The new players chosen will be offered a new salary, higher than the one they are been paid right now.
# To calculate on average how much money to offer the new players, we calculated the average percentage of the salary increase
# of players that changed teams. This was made to ensure a valuable offer to the players and heighten the chances of them accepting the offer.

# Filter the dataset for the years 1991 to 2001
filtered_salaries <- salaries %>%
  filter(yearID >= 1991 & yearID <= 2001)

# This assumption was made as an approximate estimation of the salary increase. 
# Oakland's team could decide to take a different range of years for the estimate and simply replace them to achive an increase that suits the budget for 2001.

# Calculate year-to-year salary changes and detect team changes for all players
salary_team_changes <- filtered_salaries %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>% 
  mutate(previous_team = lag(teamID), # Adds a variable that matches the player's previous team
         team_change = teamID != previous_team, # Checks if the previous team is the same as the current one
         salary_increase = salary - lag(salary), # Calculates salary increase
         pct_increase = (salary_increase / lag(salary))) %>%
  filter(team_change) # Filters by players with team changes 

# Calculate the average percentage increase for these players
average_pct_increase_team_change <- mean(salary_team_changes$pct_increase, na.rm = TRUE)

# Display the boxplot that shows the distribution of Salary Increase Rate
boxplot(salary_team_changes$pct_increase, main="Distribution of Salary Increase Rate", ylab="Salary Increase Rate(%)", ylim=c(-2, 3))
text(x = 1.35, y = -0.15, labels = paste("Mean:", round(average_pct_increase_team_change, 2)), pos = 3, col = "red")

# Print the result
print(average_pct_increase_team_change)

# Players selected will be those that can be offered 41,77% more than what they are earning right now.


# 2. The second assumption is that the budget for this offer will be the the salary the leaving player had, plus the average yearly increase of salary those players had in the last 10 years.
# Filter for Oakland Athletics (teamID "OAK") from 1991 to 2001
oakland_salaries <- salaries %>%
  filter(teamID == "OAK" & yearID >= 1991 & yearID <= 2001)

# Calculate year-to-year salary changes for Oakland players
oakland_salary_changes <- oakland_salaries %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(salary_increase = salary - lag(salary),
         pct_increase = (salary_increase / lag(salary)) ) %>%
  filter(!is.na(pct_increase)) # Exclude instances with NA (the first year for each player)

# Calculate the average percentage increase across all Oakland players in the period
average_pct_increase_oakland <- mean(oakland_salary_changes$pct_increase, na.rm = TRUE)

# Display the boxplot that shows the distribution of Salary Increase Rate across all Oakland
boxplot(oakland_salary_changes$pct_increase, main="Distribution of Salary Increase Rate across all Oakland", ylab="Salary Increase Rate(%)", ylim=c(-2, 3))
text(x = 1.35, y = 0, labels = paste("Mean:", round(average_pct_increase_oakland, 2)), pos = 3, col = "red")

# Print the result
print(average_pct_increase_oakland)

# In conclusion, for each free position, the budget will be the salary the player had, times the average percentage of salary increase.
# And the current salary for each new player should be the budget for each salary, divided by the average salary increase of players that changed teams between the years 1991, and 2001

# To identify how much were the leaving players earning, is necessary to merge both tables
# To select the players, only players with available salary information will be taken into consideration.
full_data <- merge(batting, salaries, by=c("playerID", "yearID")) 

old_players <-c('giambja01', 'damonjo01', 'isrinja01')
# Players that need to be substituted
old_players_data <- full_data[full_data$playerID %in% old_players, ]

# Filter for the old players' salaries in the year 2001 in alphabetical order
old_players_salaries_2001 <- salaries %>%
  filter(playerID %in% old_players & yearID == 2001) %>%
  select(playerID, salary) %>%
  arrange(playerID)


# The two budget constraints are included in the following vector. Each player's salary incremented by the average increase in Oakland's salaries, 
# divided by the average increase in salary for players that changed leagues. This should return the maximum budget for each position of the new players.
budget_for_new_player <- old_players_salaries_2001$salary * (1 + average_pct_increase_oakland) /average_pct_increase_oakland


# Leaving players replacements

# Johnny Damon - Outfielder
damonjo01_replacement <- full_data %>% filter(teamID.x != "OAK" & yearID == 2001 & salary <= budget_for_new_player[1] & SB >=  27 & OBP >= 0.32352941 & OPS >= 0.6868834 & SLG >= 0.3633540 )

# Ichiro Suzuki (suzukic01) would be an excellent replacement for Johnny Damon. Ichiro offers a higher batting average and significantly more stolen bases, indicating superior base-running ability, 
# while also providing a higher OPS, suggesting better overall offensive skills. 
# Ichiro's profile suggests he can effectively replicate or exceed Damon's contributions in terms of getting on base, scoring runs, and providing speed on the bases.

# Jason Giambi - First Base 
giambja01_replacement <- full_data %>% filter(teamID.x != "OAK" & yearID == 2001 & salary <= budget_for_new_player[2] & RBI >= 120 & OBP >= 0.47690015 & SLG > 0.6596154 & OPS >= 1.1365155 & HR >= 38 )

# Barry Bonds (bondsba01) stands out as the best player for the first baseman position based on the provided metrics, especially due to his exceptionally high OPS, which is significantly above the others, 
# indicating superior overall offensive performance. 
# He leads in home runs, on-base percentage, and slugging percentage, demonstrating both his ability to get on base and his power-hitting capabilities. 

# Jason Isringhausen - Closer. For this position more statistics related to pitching are needed, but the database provided contained only bating information.
isrinja01_replacement <- full_data %>% filter(teamID.x != "OAK" & yearID == 2001 & salary <= budget_for_new_player[3] & AB == 0 & R == 0 & H == 0 & X2B == 0 & X3B == 0 & IBB == 0 & HR == 0 & BB == 0 & G >= 65 & G_batting <= 2)

# For the closer position, metrics regarding the players pitching are required. Since only batting data was provided, the results were filtered so the majority of the batting statistics were 0, 
# to ensure a pitcher was selected. The other two available metrics were games played and games batting, which were used to filtered to match the profile of the leaving player with the options remaining.
# Two choose between the two options that match Jason Isringhausen profile with the available information, an additional assumption had to be made: The higher the player gets paid, the better his performance
# is in the current league. This may not be true on practice, but do to the lack of pitching data is a necessary assumption. 
# Taking this into consideration, the most adequate candidate to substituted Jason Isringhausen is Eddie Guardado (guarded01).

# Vector with replacements
new_players <- c('suzukic01','bondsba01','guarded01')

# FINAL REPLACEMENTS
# 1. For Johnny Damon: Ichiro Suzuki (suzukic01)
# 2. For Jason Giambi: Barry Bonds (bondsba01)
# 3. For Jason Isringhausen: Eddie Guardado (guarded01)





# REFERENCES

# 1.	Batting Average (AVG):
# • Major League Baseball. (n.d.). Batting average (AVG). MLB.com. 
# https://www.mlb.com/glossary/standard-stats/batting-average


# 2.	On-Base Percentage (OBP):
# • Major League Baseball. (n.d.). On-base percentage (OBP). MLB.com. 
# https://www.mlb.com/glossary/standard-stats/on-base-percentage


#3.	Slugging Percentage (SLG):
# https://www.mlb.com/glossary/standard-stats/slugging-percentage


# 4.	On-Base Plus Slugging (OPS):
# • Major League Baseball. (n.d.). On-base plus slugging (OPS). MLB.com. 
# https://www.mlb.com/glossary/standard-stats/on-base-plus-slugging




