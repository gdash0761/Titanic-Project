# 1. IMPORT AND PREVIEW (0-6 MINUTES) -----------------------------------
data_dir <- paste0("C:/Users/Stanley/OneDrive/Documents/DSS 445/DSS 445 Datasets")
data_file <- file.path(data_dir, "titanic.csv")
if (!file.exists(data_file)) {
  stop("Save titanic.csv in data_dir, or edit data_dir.")
}
titanic <- read.csv(data_file,
                    na.strings = ("NA"))




head(titanic)
str(titanic)
summary(titanic)
nrow(titanic)
colSums(is.na(titanic))

#1. Creating a Dummy Variable
titanic$male <- as.numeric(titanic$Sex == "male")
table(titanic$Sex, titanic$male)


#2. data and distribution of quantitative data
aggregate(Survived ~ Sex, data = titanic, FUN = mean)
boxplot(Age ~ Survived, data = titanic,
        main = "Age by Survival",
        xlab = "Approved (0 = No, 1 = Yes)", ylab = "Age",
        col = c("red", "lightgreen"))

#3 and 4. Create a Model
model_logistic <- glm(Survived ~ Age + titanic$male + Pclass +
                        SibSp + Parch,
                      data = titanic, family = binomial(link = "logit"))
summary(model_logistic)

model_logistic1 <- glm(Survived ~ Age + male + Pclass +
                        SibSp,
                      data = titanic, family = binomial(link = "logit"))
summary(model_logistic1)

#5. Interpreting Coefficients

100 * (exp(coef(model_logistic)["Age"]) - 1)
100 * (exp(coef(model_logistic)["Sexmale"]) - 1)
100 * (exp(coef(model_logistic)["Pclass"]) - 1)
100 * (exp(coef(model_logistic)["SibSp"]) - 1)

#6. Confusion Matrix
titanic$PredictedProbability <- NA

titanic$PredictedProbability[as.numeric(names(predict(model_logistic, type = "response")))] <- predict(model_logistic, type = "response")
titanic$PredictedClass <- ifelse(
  is.na(titanic$PredictedProbability),
  NA,
  ifelse(titanic$PredictedProbability >= 0.5, 1, 0)
)
head(titanic[, c("Survived", "PredictedProbability", "PredictedClass")])
results <- table(Actual = factor(titanic$Survived, levels = c(0, 1)),
                 Predicted = factor(titanic$PredictedClass, levels = c(0, 1)))
results
mean(titanic$PredictedClass==titanic$Survived)
mean(titanic$Survived == 1)

#7 Jack Hypothetical

Jack <- data.frame(Age = 23, male = 1, Pclass = 3,
                        SibSp = 0)
predict(model_logistic1, newdata = Jack, type = "response")


# 8. Look up a passenger and compare predicted vs. actual survival
passenger_id <- 78  # Replace 1 with your PassengerId

# Look up the passenger by ID in the original dataset
my_passenger <- titanic[titanic$PassengerId == passenger_id, ]

if (nrow(my_passenger) == 0) {
  stop("PassengerId not found. Enter an ID from titanic$PassengerId.")
}

# Show the passenger's full row
print(my_passenger)

# Predict the probability of survival using the final model
my_probability <- predict(model_final, newdata = my_passenger,
                          type = "response")

# Use the ROC cutoff to turn the probability into a prediction
my_prediction <- ifelse(my_probability >= best_cutoff)


                        