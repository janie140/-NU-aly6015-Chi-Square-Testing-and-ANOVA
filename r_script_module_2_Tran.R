# Trang Tran, ALY6015, Module 2 Practice, Apr 26

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
p_load(tidyverse)
p_load(ggpubr)

###############################################################################
################# Section 11-1/ Chi-squared Goodness of Fit #################
################# Use the traditional method
######## P6. Blood Types
# set the expected relative frequency distribution
exp_rel_freq_dist <- c(0.2, 0.28, 0.36, 0.16)

# set the observed frequency distribution
obs_freq_dist <- c(12, 8, 24, 6)

alpha = 0.1  # get alpha
df <- length(exp_rel_freq_dist) - 1  # get degrees of freedom

# get critical value
critical_value <- qchisq(alpha, df, lower.tail = FALSE)

# calculate the expected frequency distribution
exp_freq_dist <- sum(obs_freq_dist) * exp_rel_freq_dist

# calculate the test value
test_value <- 0
for (i in 1:length(exp_rel_freq_dist)) {
  test_value <- 
    test_value + (obs_freq_dist[i] - exp_freq_dist[i])^2 / exp_freq_dist[i]
}
writeLines(paste('\nchi squared goodness of fit test value:', test_value))

######## P8. On-Time Performance by Airlines
# set the expected relative frequency distribution
exp_rel_freq_dist <- c(0.708, 0.082, 0.09, 0.12)

# set the observed frequency distribution
obs_freq_dist <- c(125, 10, 25, 40)

alpha = 0.05  # get alpha
df <- length(exp_rel_freq_dist) - 1  # get degrees of freedom

# get critical value
critical_value <- qchisq(alpha, df, lower.tail = FALSE)

# calculate the expected frequency distribution
exp_freq_dist <- sum(obs_freq_dist) * exp_rel_freq_dist

# calculate the test value
test_value <- 0
for (i in 1:length(exp_rel_freq_dist)) {
  test_value <- 
    test_value + (obs_freq_dist[i] - exp_freq_dist[i])^2 / exp_freq_dist[i]
}
writeLines(paste('\nchi squared goodness of fit test value:', test_value))

###############################################################################
################# Section 11-2/ Chi-squared Independence Test #################
################# Use the traditional method
######## P8. Ethnicity and Movie Admissions
# create the contingency table
# enter the data
col_1 <- c(724, 370)  # column 1 of contingency table - top to bottom
col_2 <- c(335, 292)  # column 2 of contingency table - top to bottom
col_3 <- c(174, 152)  # column 3 of contingency table - top to bottom
col_4 <- c(107, 140)  # column 4 of contingency table - top to bottom

# form the data as a contingency table
obs_cont_table <- as.table(matrix(c(col_1, col_2, col_3, col_4), nrow = 2, ncol = 4))

# name the rows and columns of the contingency table
rownames(obs_cont_table) <- c('2013', '2014')  # top to bottom
colnames(obs_cont_table) <- c('Caucasian', 'Hispanic', 'African_American', 'Other')  # left to right

# calculate the expected contingency table
# define an empty matrix
exp_cont_matrix <- matrix(data = NA, nrow = dim(obs_cont_table)[1], 
                          ncol = dim(obs_cont_table)[2])

# fill the empty matrix to get the expected contingency matrix
grand_total <- sum(sum(obs_cont_table))
for (i in 1:dim(obs_cont_table)[1]) {
  for (j in 1:dim(obs_cont_table)[2]) {
    row_sum <- sum(obs_cont_table[i, ])
    col_sum <- sum(obs_cont_table[, j])
    exp_cont_matrix[i, j] = row_sum * col_sum / grand_total
  }
}
# convert the expected contingency matrix to the expected contingency table 
# and add row and column names
exp_cont_table <- as.table(exp_cont_matrix)
rownames(exp_cont_table) <- c('2013', '2014')  # top to bottom
colnames(exp_cont_table) <- c('Caucasian', 'Hispanic', 'African_American', 'Other')# left to right

writeLines(paste('\nexpected contingency table:\n'))
print(exp_cont_table)

# define / calculate various parameters required by the traditional method
alpha = 0.05  # get alpha

# get degrees of freedom
num_rows <- dim(obs_cont_table)[1]
num_cols <- dim(obs_cont_table)[2]
df <- (num_rows - 1) * (num_cols - 1)

# get critical value and report it
critical_value <- qchisq(alpha, df, lower.tail = FALSE)

# calculate the test value
test_value <- 0
for (i in 1:dim(obs_cont_table)[1]) {
  for (j in 1:dim(obs_cont_table)[2]) {
    test_value <- 
      test_value + (obs_cont_table[i, j] - exp_cont_table[i, j])^2 / 
      exp_cont_table[i, j]
  }
}
writeLines(paste('\nchi squared independence test test value:', test_value))

######## P10. Women in the Military
# create the contingency table
# enter the data
col_1 <- c(10791, 7816, 932, 11819 )  # column 1 of contingency table - top to bottom
col_2 <- c(62491, 42750, 9525, 54344)  # column 2 of contingency table - top to bottom

# form the data as a contingency table
obs_cont_table <- as.table(matrix(c(col_1, col_2), nrow = 4, ncol = 2))

# name the rows and columns of the contingency table
rownames(obs_cont_table) <- c('Army', 'Navy', 'Marine_Corps', 'Air_Force') # top to bottom
colnames(obs_cont_table) <- c('Officers', 'Enlisted')  # left to right

# calculate the expected contingency table
# define an empty matrix
exp_cont_matrix <- matrix(data = NA, nrow = dim(obs_cont_table)[1], 
                          ncol = dim(obs_cont_table)[2])

# fill the empty matrix to get the expected contingency matrix
grand_total <- sum(sum(obs_cont_table))
for (i in 1:dim(obs_cont_table)[1]) {
  for (j in 1:dim(obs_cont_table)[2]) {
    row_sum <- sum(obs_cont_table[i, ])
    col_sum <- sum(obs_cont_table[, j])
    exp_cont_matrix[i, j] = row_sum * col_sum / grand_total
  }
}
# convert the expected contingency matrix to the expected contingency table 
# and add row and column names
exp_cont_table <- as.table(exp_cont_matrix)
rownames(exp_cont_table) <- c('Army', 'Navy', 'Marine_Corps', 'Air_Force')# top to bottom
colnames(exp_cont_table) <- c('Officers', 'Enlisted') # left to right

writeLines(paste('\nexpected contingency table:\n'))
print(exp_cont_table)

# define / calculate various parameters required by the traditional method
alpha = 0.05  # get alpha

# get degrees of freedom
num_rows <- dim(obs_cont_table)[1]
num_cols <- dim(obs_cont_table)[2]
df <- (num_rows - 1) * (num_cols - 1)

# get critical value and report it
critical_value <- qchisq(alpha, df, lower.tail = FALSE)

# calculate the test value
test_value <- 0
for (i in 1:dim(obs_cont_table)[1]) {
  for (j in 1:dim(obs_cont_table)[2]) {
    test_value <- 
      test_value + (obs_cont_table[i, j] - exp_cont_table[i, j])^2 / 
      exp_cont_table[i, j]
  }
}
writeLines(paste('\nchi squared independence test test value:', test_value))

###############################################################################
################# Section 12-1/ One-way ANOVA Test #################
################# Use the traditional method
######## P8. Sodium Contents of Foods
# create data frame for condiments
condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200),
                         'food' = rep('condiments', 7), 
                         stringsAsFactors = FALSE)

# create data frame for cereals
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140),
                      'food' = rep('cereals', 7), stringsAsFactors = FALSE)

# create data frame for desserts
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160),
                       'food' = rep('desserts', 8), stringsAsFactors = FALSE)

# combine the data frames
sodium = rbind(condiments, cereals, desserts)

writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
for (food in unique(sodium$food)) {
  print(food)
  print(shapiro.test(sodium[sodium$food == food, 'sodium']))
}
writeLines('Check out the means of the groups')
print(aggregate(sodium$sodium, by = list(sodium$food), FUN = mean))

writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(sodium$sodium, by = list(sodium$food), FUN = sd))

writeLines('Form One-Way ANOVA Hypothesis:')
writeLines('\nH0: mean1 = mean2 = mean3')
writeLines('H1: at least one of the means is different then the others')
writeLines('claim: H1')

# perform test
alpha <- 0.05
writeLines(paste('\none-way anova alpha:', alpha))
anova <- aov(sodium ~ food, data = sodium)

writeLines('\n***************************')
writeLines('results of one-way anova analysis:\n')
print(summary(anova))

# visualize the results
boxplot(sodium ~ food, data = sodium)

# save summary to an object
anova_summary <- summary(anova)

###############################################################################
################# Section 12-2/ One-way ANOVA Test #################
################# Use Scheffé or Tukey test if H0 is rejected
######## P10. Sales for Leading Companies
# create data frame for Cereal
Cereal <- data.frame('sales' = c(578, 320, 264, 249, 237),
                         'company' = rep('Cereal', 5), 
                         stringsAsFactors = FALSE)

# create data frame for Chocolate Candy
Chocolate_Candy <- data.frame('sales' = c(311, 106, 109, 125, 173),
                      'company' = rep('Chocolate_Candy', 5),
                      stringsAsFactors = FALSE)

# create data frame for Coffee
Coffee <- data.frame('sales' = c(261, 185, 302, 689),
                     'company' = rep('Coffee', 4),
                     stringsAsFactors = FALSE)

# combine the data frames
sales = rbind(Cereal, Chocolate_Candy, Coffee)

writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
for (company in unique(sales$company)) {
  print(company)
  print(shapiro.test(sales[sales$company == company, 'sales']))
}
writeLines('Check out the means of the groups')
print(aggregate(sales$sales, by = list(sales$company), FUN = mean))

writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(sales$sales, by = list(sales$company), FUN = sd))

writeLines('Form One-Way ANOVA Hypothesis:')
writeLines('\nH0: mean1 = mean2 = mean3')
writeLines('H1: at least one of the means is different then the others')
writeLines('claim: H1')

# perform test
alpha <- 0.01
writeLines(paste('\none-way anova alpha:', alpha))
anova <- aov(sales ~ company, data = sales)

writeLines('\n***************************')
writeLines('results of one-way anova analysis:\n')
print(summary(anova))

# visualize the results
boxplot(sales ~ company, data = sales)

# save summary to an object
anova_summary <- summary(anova)

######## P12. Per-Pupil Expenditures
# create data frame for Eastern third
Eastern_third <- data.frame('expenditures' = c(4946, 5953, 6202, 7243, 6113),
                     'section' = rep('Eastern_third', 5), 
                     stringsAsFactors = FALSE)

# create data frame for Middle third
Middle_third <- data.frame('expenditures' = c(6149, 7451, 6000, 6479),
                              'section' = rep('Middle_third', 4),
                              stringsAsFactors = FALSE)

# create data frame for Western third
Western_third <- data.frame('expenditures' = c(5282, 8605, 6528, 6911),
                     'section' = rep('Western_third', 4),
                     stringsAsFactors = FALSE)

# combine the data frames
expenditures = rbind(Eastern_third, Middle_third, Western_third)

writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
for (section in unique(expenditures$section)) {
  print(section)
  print(shapiro.test(expenditures[expenditures$section == section, 'expenditures']))
}
writeLines('Check out the means of the groups')
print(aggregate(expenditures$expenditures, by = list(expenditures$section), FUN = mean))

writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(expenditures$expenditures, by = list(expenditures$section), FUN = sd))

writeLines('Form One-Way ANOVA Hypothesis:')
writeLines('\nH0: mean1 = mean2 = mean3')
writeLines('H1: at least one of the means is different than the others')
writeLines('claim: H1')

# perform test
alpha <- 0.05
writeLines(paste('\none-way anova alpha:', alpha))
anova <- aov(expenditures ~ section, data = expenditures)

writeLines('\n***************************')
writeLines('results of one-way anova analysis:\n')
print(summary(anova))

# visualize the results
boxplot(expenditures ~ section, data = expenditures)

# save summary to an object
anova_summary <- summary(anova)

###############################################################################
################# Section 12-3/ Two-way ANOVA Test #################
######## P10. Increasing Plant Growth
# load the dataset
plantdf <- read.csv("plant_growth.csv")
str(plantdf)

# Convert Plant_food as a factor and recode the levels
plantdf$Plant_food <- factor(plantdf$Plant_food, 
                       levels = c('A', 'B'),
                       labels = c("f1", "f2"))

# Convert Grow_light as a factor and recode the levels
plantdf$Grow_light <- factor(plantdf$Grow_light, 
                             levels = c('1', '2'),
                             labels = c("g1", "g2"))

# Check two-way annova assumptions
################################################################################
writeLines('\n***************************')
writeLines('Check two-way annova assumptions:')

writeLines('\n***************************')
writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
food_trtmt <- c()
light_trtmt <- c()
p_value <- c()
for (Plant_food in unique(plantdf$Plant_food)) {
  for (Grow_light in unique(plantdf$Grow_light)) {
    writeLines('***********')
    
    print(Plant_food)
    print(Grow_light)
    
    shapiro_test <- shapiro.test(plantdf[plantdf$Plant_food == Plant_food & 
                                           plantdf$Grow_light == Grow_light, 'Growth'])
    print(shapiro_test)
    
    food_trtmt <- c(food_trtmt, Plant_food)
    light_trtmt <- c(light_trtmt, Grow_light)
    p_value <- c(p_value, shapiro_test$p.value)
  }
}
temp_df <- data.frame(food_trtmt, light_trtmt, p_value)

writeLines('\n***************************')
writeLines('Check out the means of the groups')
print(aggregate(plantdf$Growth, by = list(plantdf$Plant_food, plantdf$Grow_light), 
                FUN = mean))

writeLines('\n***************************')
writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(plantdf$Growth, by = list(plantdf$Plant_food, plantdf$Grow_light), 
                FUN = sd))

# compute two-way ANOVA
################################################################################
writeLines('\n****************************************************************')
writeLines('compute two-way anova:')
res.aov3 <- aov(Growth ~ Plant_food * Grow_light, data = plantdf)
print(summary(res.aov3))
################################################################################
# Plant_food is significant - which pair of means are different in Plant_food
################################################################################
writeLines('\n****************************************************************')
writeLines('which pair of means are different in Plant_food:')
print(TukeyHSD(res.aov3, which = "Plant_food"))

################################################################################
# visualize the data
# Box plot with multiple groups
ggboxplot(plantdf, x = "Plant_food", y = "Growth", color = "Grow_light",
          palette = c("#00AFBB", "#E7B800"))

# Two-way interaction plot
ggline(plantdf, x = "Plant_food", y = "Growth", color = "Grow_light",
       add = c("mean_se"),
       palette = c("#00AFBB", "#E7B800"))

################################################################################
################# ON MY OWN #################
######## P1. Baseball Dataset
# load the dataset
bb <- read.csv("baseball-1.csv")
p_load(corrplot)
p_load(skimr)

##### EDA
colnames(bb)
head(bb)
skim(bb) #data types, missing values, min, max, mean, sd, hist
anyDuplicated(bb) #check for duplications

# Check for missing values
sapply(bb, function(x) sum(is.na(x)))

#remove missing columns (>20%)
bb <- select(bb, -RankSeason, -RankPlayoffs, -OOBP, -OSLG)

# convert Playoffs, RankSeason, RankPlayoffs into factors
bb$Playoffs <- as.factor(bb$Playoffs)

#Correlation matrix
# Corrplot of numeric variables: #na.rm = TRUE
corrs <- round(cor(bb[, unlist(lapply(bb, is.numeric))], use = "complete.obs"), 2)
corrplot(corrs, method="circle", type="upper", tl.col = "black", tl.srt = 45)

##### Perform a Chi-Square Goodness-of-Fit test
## Traditional Method
# Extract decade from year
bb$Decade <- bb$Year - (bb$Year%% 10)
# Create a wins table by summing the wins by decade
wins <- bb |> group_by(Decade) |> summarize(wins = sum(W)) |> as_tibble()

# set the expected relative frequency distribution
exp_rel_freq_dist <- c(rep(1/6, 6))

# set the observed frequency distribution
obs_freq_dist <- c(13267, 17934, 18926, 17972, 24286, 7289)

alpha = 0.05  # get alpha
df <- length(exp_rel_freq_dist) - 1  # get degrees of freedom

# get critical value
critical_value <- qchisq(alpha, df, lower.tail = FALSE)

# calculate the expected frequency distribution
exp_freq_dist <- sum(obs_freq_dist) * exp_rel_freq_dist

# calculate the test value
test_value <- 0
for (i in 1:length(exp_rel_freq_dist)) {
  test_value <- 
    test_value + (obs_freq_dist[i] - exp_freq_dist[i])^2 / exp_freq_dist[i]
}
writeLines(paste('\nchi squared goodness of fit test value:', test_value))

## when using p-value method
results <- chisq.test(x = obs_freq_dist, p = exp_rel_freq_dist)  # run test

######## P2. Crop Dataset
# load the dataset
crop <- read.csv("crop_data-1.csv")
str(crop)
crop <- select(crop, -block)
unique(crop$density)
unique(crop$fertilizer)

# Convert density as a factor and recode the levels
crop$density <- factor(crop$density, 
                             levels = c('1', '2'),
                             labels = c("d1", "d2"))

# Convert fertilizer as a factor and recode the levels
crop$fertilizer <- factor(crop$fertilizer, 
                             levels = c('1', '2', '3'),
                             labels = c("f1", "f2", 'f3'))

# Check two-way annova assumptions
################################################################################
writeLines('\n***************************')
writeLines('Check two-way annova assumptions:')

writeLines('\n***************************')
writeLines('Test for normality - Shapiro-Wilk normality test (H0: normal)\n')
density_trtmt <- c()
fertilizer_trtmt <- c()
p_value <- c()
for (density in unique(crop$density)) {
  for (fertilizer in unique(crop$fertilizer)) {
    writeLines('***********')
    
    print(density)
    print(fertilizer)
    
    shapiro_test <- shapiro.test(crop[crop$fertilizer == fertilizer & 
                                        crop$density == density, 'yield'])
    print(shapiro_test)
    
    density_trtmt <- c(density_trtmt, density)
    fertilizer_trtmt <- c(fertilizer_trtmt, fertilizer)
    p_value <- c(p_value, shapiro_test$p.value)
  }
}
temp_df <- data.frame(density_trtmt, fertilizer_trtmt, p_value)

writeLines('\n***************************')
writeLines('Check out the means of the groups')
print(aggregate(crop$yield, by = list(crop$density, crop$fertilizer), 
                FUN = mean))

writeLines('\n***************************')
writeLines('Check out the standard deviations (sqrt(variance)) of the groups')
print(aggregate(crop$yield, by = list(crop$density, crop$fertilizer), 
                FUN = sd))

# compute two-way ANOVA
################################################################################
writeLines('\n****************************************************************')
writeLines('compute two-way anova:')
res.aov3 <- aov(yield ~ density * fertilizer, data = crop)
print(summary(res.aov3))

################################################################################
writeLines('\n****************************************************************')
writeLines(paste('which pair of means are different in the density/fertilizer', 
                 'interaction:'))
print(TukeyHSD(res.aov3))


