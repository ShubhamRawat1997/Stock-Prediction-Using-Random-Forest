# Load necessary libraries
suppressPackageStartupMessages(library(tidymodels))
suppressPackageStartupMessages(library(visdat))
suppressPackageStartupMessages(library(modeldata))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(rsample))
suppressPackageStartupMessages(library(splitstackshape))
suppressPackageStartupMessages(library(hrbrthemes))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(TTR))
suppressPackageStartupMessages(library(Metrics))
suppressPackageStartupMessages(library(gmodels))

#install.packages("gmodels")

setwd("E:/Rstudio/Final Project")

#dev.off()

# Set the theme for ggplot2
theme_set(theme_bw(base_size=12))

# Load the data from a CSV file
SD<- as.data.frame(read.csv("401_Dataset.csv"))
summary(SD)


# Split the data into features (X) and target (y)
missingvalues <- sapply(SD,function(x) sum(is.na(x)))
print(missingvalues)

#----------handle missing values
SD <- na.omit(SD)
# Ensure the Date column is in the correct format
SD$DATE <- as.Date(SD$DATE, format = "%d-%m-%Y")

# Checking the varible importance
#library(car)
vif_model <- lm(Stock ~ M2Velocity + UnempRate + CPI + PPI + FCI + FedFunds + Bpermit + Dollar + DollarPercChange, data = SD)
vif(vif_model)

# scaling the data

SD_scaled <- as.data.frame(scale(SD[,-1], center = TRUE, scale = TRUE))
head(SD_scaled)

SD_scaled$DATE <- SD$DATE
head(SD_scaled)

SD_cleaned <- SD_scaled

###############################Step 2: EDA (Exploratory Data Analysis)

# Plot Stock over time
ggplot(SD_scaled, aes(x = DATE, y = Stock)) +
  geom_line(color = "blue") +
  labs(title = "Stock Indices Over Time", x = "Date", y = "Stock Indices")


# Correlation matrix for columns
cor_matrix <- cor(SD_scaled %>% select(where(is.numeric)), use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 60)

#Par Plot
variables <- c("Stock", "M2Velocity", "CPI", "PPI", "UnempRate", "FedFunds")
ggpairs(SD_scaled[, variables], title = "Pair plot")


# Exploring the relationship between CPI, PPI, and Stock
ggplot(SD_scaled, aes(x = CPI, y = Stock)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "CPI vs Stock Returns", x = "CPI", y = "Stock")

ggplot(SD_scaled, aes(x = PPI, y = Stock)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "PPI vs Stock Returns", x = "PPI", y = "Stock")

ggplot(SD_scaled, aes(x = M2Velocity, y = Stock)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "M2Velocity vs Stock Returns", x = "M2Velocity", y = "Stock")

ggplot(SD_scaled, aes(x = FedFunds, y = Stock)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "FedFunds vs Stock Returns", x = "FedFunds", y = "Stock")

ggplot(SD_scaled, aes(x = PPI, y = CPI)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "PPI vs CPI", x = "PPI", y = "CPI")

ggplot(SD_scaled, aes(x = M2Velocity, y = FedFunds)) +
  geom_line(color = "Black") +
  geom_smooth(method = "lm") +
  labs(title = "CPI vs Stock Returns", x = "M2Velocity", y = "FedFunds")



# Multi-line plot
melted_data_1 <- melt(SD_scaled, id.vars = "DATE", measure.vars = c("CPI","PPI", "Stock"))
ggplot(melted_data_1, aes(x = DATE, y = value, color = variable)) +
  geom_line() +
  labs(title = "Trends Over Time", x = "Date", y = "Value")

melted_data_2 <- melt(SD_scaled, id.vars = "DATE", measure.vars = c("FedFunds","M2Velocity", "Stock"))
ggplot(melted_data_2, aes(x = DATE, y = value, color = variable)) +
  geom_line() +
  labs(title = "Trends Over Time", x = "Date", y = "Value")


############################Step 3: Feature Engineering#####################################
#Lagging Indicators: Create lag features for unemployement rate, CPI, PPI, and FCI to analyze delayed effects.

SD_New <- SD_cleaned %>%
  mutate(
    Stock_Lag = lag(Stock, n = 6),
    CPI_Lag = lag(CPI, n = 6),
    PPI_Lag = lag(PPI, n = 6),
    M2V_Lag = lag(M2Velocity, n = 6)
  )


#b) Moving Averages
#Calculate moving averages for Stock prices (e.g., 5-day and 10-day moving averages).
# Add 5-day and 10-day moving averages
library(TTR)

SD_New$Stock_MA6 <- SMA(SD_cleaned$Stock, n = 6)
SD_New$Stock_MA12 <- SMA(SD_cleaned$Stock, n = 12)


#c) Create Labels for Classification
#We classify stock conditions based on percentage change in Stock price:
  
# Add percentage change
SD_New$Stock_Change <- (SD_New$Stock - SD_New$Stock_Lag) / SD_New$Stock_Lag * 100
hist(SD_New$Stock_Change, main = "Stock Change rate Distribution", xlab = "Residuals", col = "LightGreen")


# Classify into labels
SD_New$Market_Condition <- cut(
  SD_New$Stock_Change,
  breaks = c(-Inf, -20, 0, 20, 40, Inf),
  labels = c("Strong Bearish", "Bearish", "Neutral", "Bullish", "Strong Bullish")
)
  
# Remove rows with NA values
SD_New <- na.omit(SD_New)
#St_SD_New <- as.data.frame(scale(SD_New[,-c(1,21)]))
St_SD_New <- as.data.frame(scale(SD_New[,-c(13,18,19,21)]))
#St_SD_New1 <- as.data.frame(scale(SD_New[,-c(1,21)]))

St_SD_New$DATE <- SD_New$DATE
St_SD_New$Market_Condition <- SD_New$Market_Condition
St_SD_New <- droplevels(St_SD_New)

##################################Step 4: Model Building - Random Forest
#4.1 Split Data into Training and Testing Sets

library(caret)
set.seed(123)
train_index <- createDataPartition(St_SD_New$Stock, p = 0.8, list = FALSE)
train_data <- St_SD_New[train_index,]
test_data <- St_SD_New[-train_index,]


melted_data <- melt(St_SD_New, id.vars = "DATE", measure.vars = c("CPI","PPI", "M2Velocity", "Stock"))
ggplot(melted_data, aes(x = DATE, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Trends Over Time", x = "Years", y = "Value")

melted_data <- melt(St_SD_New, id.vars = "DATE", measure.vars = c("Bpermit", "FedFunds"))
ggplot(melted_data, aes(x = DATE, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Trends Over Time", x = "Years", y = "Value")

melted_data <- melt(St_SD_New, id.vars = "DATE", measure.vars = c("Stock", "M2Velocity"))
ggplot(melted_data, aes(x = DATE, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Trends Over Time", x = "Years", y = "Value")

melted_data <- melt(St_SD_New, id.vars = "DATE", measure.vars = c( "FCI", "Stock"))
ggplot(melted_data, aes(x = DATE, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  labs(title = "Trends Over Time", x = "Years", y = "Value")


################################Regression###############################

#4.2 Build a Random Forest Model
library(randomForest)
# Random Forest Regression
set.seed(123)
rf_model <- randomForest(Stock ~ DATE+PPI+CPI+M2Velocity, 
                         data = train_data, ntree = 200, importance = TRUE)

# Model summary
print(rf_model)
rf_model$importance
vip::vip(rf_model) + theme_bw()


# Predictions
predictions <- predict(rf_model, test_data)
pred_data <- as.data.frame(predictions)

OVP <- test_data %>%
  mutate(
    Pred = pred_data$predictions
  )
OVP


ggplot(OVP, aes(x = Stock_Lag, y = Pred)) +
geom_line(color = "Red") +
geom_point(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA) +
labs(title = "Observed vs Predicted", x = "CPI", y = "Stock")

#
residuals <- test_data$Stock - predict(rf_model, test_data)
residuals
hist(residuals, main = "Residuals Distribution", xlab = "Residuals", col = "skyblue")



# Evaluate performance
#install.packages("Metrics")
library(Metrics)
rmse(test_data$Stock, predictions)

######################Hyper parameter tuning#################

mtry_values = c(1,2,3,4,5,6)
results <- data.frame()
for(mtry in mtry_values){
  set.seed(007)
  rf_model_regHP <- train(Stock ~ M2V_Lag+ 
                              FedFunds +
                            FCI+
                            Bpermit+
                            Dollar+
                            Gold,
                            data = train_data,  
                            metric = "RMSE",
                            method = "rf",
                            trControl = trainControl(method = "cv", number = 5),
                            tuneGrid = expand.grid(.mtry = mtry)
  )

  # Extract metrics
  rmse <- rf_model_regHP$results$RMSE
  r_squared <- rf_model_regHP$results$Rsquared
  mae <- rf_model_regHP$results$MAE
  
  # Store the results
  results <- rbind(results, data.frame(mtry = mtry, RMSE = rmse, Rsquared = r_squared, MAE = mae))
  
  print(rf_model_regHP) # Print the model summary for each mtry  
}

# Print the combined results
print(results)

# Find the best mtry based on the lowest RMSE
best_mtry <- results$mtry[which.min(results$RMSE)]
cat("Best mtry:", best_mtry, "\n")

final_rf_model <- train(Stock ~ M2V_Lag+ 
                          FedFunds +
                          FCI+
                          Bpermit+
                          Dollar+
                          Gold,
                        data = test_data,
                        metric = "RMSE", # Use RMSE for regression
                        method = "rf",
                        trControl = trainControl(method = "cv", number = 5),
                        tuneGrid = expand.grid(.mtry = best_mtry))

print(final_rf_model)
vip::vip(final_rf_model) + theme_bw()
#Plot the results

library(ggplot2)

# Assuming 'results' is your data frame with mtry, RMSE, and MAE

# Plot RMSE
rmse_plot <- ggplot(results, aes(x = mtry, y = RMSE)) +
  geom_line() +
  geom_point() +
  labs(title = "RMSE vs. mtry", x = "mtry", y = "RMSE") +
  theme_bw()

# Plot MAE
mae_plot <- ggplot(results, aes(x = mtry, y = MAE)) +
  geom_line() +
  geom_point() +
  labs(title = "MAE vs. mtry", x = "mtry", y = "MAE") +
  theme_bw()

# Plot R^2
rsq_plot <- ggplot(results, aes(x = mtry, y = Rsquared)) +
  geom_line() +
  geom_point() +
  labs(title = "MAE vs. mtry", x = "mtry", y = "MAE") +
  theme_bw()


# Display the plots (separately)
print(rmse_plot)
print(mae_plot)
print(rsq_plot)

#Arrange them side by side
library(patchwork)

#rmse_plot + mae_plot

#Arrange them above and below each other
rmse_plot / mae_plot




##########################Classification###################
#Classification Model
train_data$Market_Condition <- droplevels(train_data$Market_Condition)

rf_model_class <- randomForest(Market_Condition ~ M2V_Lag + 
                                 FedFunds +  
                                 FCI+
                                 FedFunds+
                                 Bpermit+
                                 Dollar+
                                 Gold,
                               data = train_data, 
                               ntree = 1000,
                               importance = TRUE)

summary(rf_model_class)

# Feature Importance
vip::vip(rf_model_class) + theme_bw()


# Predict on test data
pred_class <- predict(rf_model_class, test_data)

# Confusion Matrix
conf_matrix = confusionMatrix(pred_class, test_data$Market_Condition)
conf_matrix

test_data$Invest_Decision <- ifelse(
  pred_class %in% c("Bearish", "Bullish"),
  "Dont Invest", "Invest"
)

test_data$Decision <- ifelse(test_data$Invest_Decision =="Invest", 1, 0)

# View results
head(test_data[, c("DATE", "Market_Condition", "Decision")])

CrossTable(test_data$Invest_Decision, test_data$Decision, prop.chisq= FALSE, prop.c= FALSE, prop.r=FALSE)

#########hyper parameter tuning
test_data$Market_Condition <- droplevels(test_data$Market_Condition)
mtry_values = c(1,2,3,4,5,6)
for(mtry in mtry_values){
  set.seed(007)
  rf_model_classHP <- train(Market_Condition ~ M2V_Lag + 
                              FedFunds +  
                              FCI+
                              FedFunds+
                              Bpermit+
                              Dollar+
                              Gold,
    data = test_data,  
    metric = "Accuracy",
    method = "rf",
    trControl = trainControl(method = "cv", number = 5),
    tuneGrid = expand.grid(.mtry = mtry)
  )
  print(rf_model_classHP)
}

summary(rf_model_classHP)
rf_model_classHP$results$Accuracy

# Feature Importance
vip::vip(rf_model_classHP) + theme_bw()
  

# Predict on test data
pred_class <- predict(rf_model_classHP, test_data)
pred_class

OVP1 <- test_data %>%
  mutate(
    Pred = pred_cla
  )
OVP1


ggplot(OVP, aes(x = Stock_Lag, y = Pred)) +
  geom_line(color = "Red") +
  geom_point(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA) +
  labs(title = "Observed vs Predicted", x = "Observed", y = "Predicted")

  
# Confusion Matrix
conf_matrix = confusionMatrix(pred_class, test_data$Market_Condition)
conf_matrix

test_data$Decision <- ifelse(test_data$Invest_Decision =="Invest", 1, 0)

 
test_data$Invest_Decision <- ifelse(
pred_class %in% c("Bearish", "Bullish"),
"Don't Invest", "Invest"
)


results_class <- as.data.frame(CrossTable(test_data$Invest_Decision, test_data$Decision, prop.chisq= FALSE, prop.c= FALSE, prop.r=FALSE))
results_class


