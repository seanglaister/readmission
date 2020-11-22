### Run a ML Model for Mean Imputed Features
library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations

#### STEP 1: Load Total Feature Space and pids ####
patient_features <- read.csv("all_patients_feature_space_11_13_20.csv.csv")
patient_features$X <- NULL
pids <- read.csv("samplepids_11_20_20.csv")
pids$X <- NULL

#### STEP 2: Remove patients who are not in the control group or readmitted group ("pids") ####
patient_features <- patient_features[which(patient_features$patientunitstayid %in% pids$patientunitstayid), ]

#### STEP 3: Remove features with <40% available. Then mean impute the rest ####
# remove ethnicity
patient_features$ethnicity <- NULL

# remove features with <40% available
patient_features <- patient_features[lapply(patient_features, function(x) sum(is.na(x)) / length(x)) < 0.4]

# mean impute features
for (i in 2:length(patient_features)){
  temp_mean <- mean(patient_features[,i], na.rm = TRUE)
  patient_features[is.na(patient_features[,i]), i] <- temp_mean
}

#### STEP 4; Get the patinet_features in the same order as pids ####
patient_features <- merge(pids, patient_features, by = "patientunitstayid")
# y <- patient_features$class
# patient_features$patientunitstayid <- NULL
# 
# patient_features$class <- NULL
# x <- patient_features




#### STEP 5: Run GLM model taking into account class imbalance ####
library(caret)

# patient_features <- patient_features %>% select(-ApacheDx)
patient_features <- patient_features[complete.cases(patient_features), ]

# Make case and control groups in order to balance class numbers
case <- patient_features[which(patient_features$class == "readmit"), ]
control <- patient_features[which(patient_features$class == "control"), ]
# control_sample <- sample(seq(1,1,nrow(control)), nrow(case) * 2)
control_sample <- control[sample(nrow(control), nrow(case)), ]
new_patient_features <- rbind(case, control_sample)


# patient_features <- patient_features %>% dplyr::select("label", all_of(VIF_features))
df <- new_patient_features
df$class <- as.factor(df$class)
# df <- df[,-1]
df$class <- as.character(df$class)
df$class[which(df$class == "readmit")] <- "class_1"
df$class[which(df$class == "control")] <- "class_0"
df$class <- as.factor(df$class)
intrain <- caret::createDataPartition(y = df$class, p = 0.80, list = FALSE)
training <- df[intrain,]
# table(training$label)
testing <- df[-intrain,]
# table(testing$label)
control = trainControl(method="repeatedcv",
                       number=10,
                       repeats=1,
                       summaryFunction = twoClassSummary,
                       classProbs = T,
                       savePredictions = T)

# If applicable: Create class weights 
label_balance <- prop.table(table(training$class))
model_weights <- ifelse(training$class == "readmit",
                        label_balance[[2]],
                        label_balance[[1]])

# Make a glm model
modelglm <- caret::train(class~., data = training, method="glm", metric = 'ROC',trControl=control, weights = model_weights)

val_results <- resamples(list(xg = modelglm,
                              rf = modelglm,
                              glm = modelglm,
                              ens = modelglm
))

summary(val_results)

model_prediction <- predict(modelglm, newdata = testing, type = "raw")
test_confusion <- confusionMatrix(model_prediction, as.factor(testing$class))
glm2class <- twoClassSummary(model_prediction_prob, lev = levels(model_prediction_prob$obs))
glmpr <- prSummary(model_prediction_prob, lev = levels(model_prediction_prob$obs))
