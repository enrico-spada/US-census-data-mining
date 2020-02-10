library(caret)
library(dplyr)
library(pROC)
library(gbm)
library(purrr)

#Function to call .py data prep pipeline
from_csv_to_dataset <- function(file_path) {
        metadata <- read.csv(file = "../docs/metadata/census_income_metadata-prepared.v00.csv",
                             header = TRUE,
                             sep = ","
        )
        
        metadata[metadata$r_types == "integer", "r_types"] <-  "numeric"
        
        
        prepared_data <- read.csv(
                file = file_path,
                header = TRUE,
                sep = ",",
                strip.white = TRUE,
                colClasses = as.character(metadata$r_types),
                #trim whitespace
                na.strings = "?"
        )
        variables_selected <- c("age", "education", "marital_stat", "race", "sex", "target")
        df_selected <- prepared_data[ , variables_selected]
        levels(df_selected$target) <- c("below", "above")
        df_selected$target <- factor(df_selected$target, levels=rev(levels(df_selected$target)))
        
        return(df_selected)
}

#Build custom AUC function to extract AUC from the caret model object
test_roc <- function(model, data) {
        roc(data$target,
            predict(model, data, type = "prob")[, "above"])
}


source_python("./2b_prep/0_data_prep.py")
data_prep_to_csv(source_path = "../data/1_input_data/input_data_train_94.csv", type = "preprocessing", out_name = "training_data_94")
data_prep_to_csv(source_path = "../data/1_input_data/input_data_train_95.csv", type = "preprocessing", out_name = "training_data_95")
data_prep_to_csv(source_path = "../data/1_input_data/input_data_test_94.csv", type = "preprocessing", out_name = "test_data_94")
data_prep_to_csv(source_path = "../data/1_input_data/input_data_test_95.csv", type = "preprocessing", out_name = "test_data_95")



set.seed(4892)

df_train_selected <- from_csv_to_dataset("../data/2_eda_prep/training_data_94.csv")


#------------------------#
#TRAIN ORIGINAL MODEL
control <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 2,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     verboseIter = TRUE)

grid_hyperp <- expand.grid(
        n.trees = c(60, 90),
        interaction.depth = c(1, 2),
        shrinkage = 0.1,
        n.minobsinnode = 10
)

orig_fit <- train(target ~ .,
                  data = df_train_selected,
                  method = "gbm",
                  tuneGrid = grid_hyperp,
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = control)

saveRDS(orig_fit, "../models/orig_fit.rds")

orig_fit %>%
        test_roc(data = df_train_selected) %>%
        auc()
## Area under the curve: 0.9575
confusionMatrix(predict(orig_fit,df_train_selected[ , !colnames(df_train_selected) %in% ("target")]),df_train_selected$target)
print(orig_fit)
plot(orig_fit)
vImpGbm=varImp(orig_fit) #Variable importance
plot(vImpGbm)





#------------------------#
#TRAIN WEIGHTED CLASSES
#Create model weights (summation equal 1)
model_weights <- ifelse(df_train_selected$target == "above",
                        (1/table(df_train_selected$target)[1]) * 0.5,
                        (1/table(df_train_selected$target)[2]) * 0.5)


#Use the same seed to ensure same cross-validation splits
control$seeds <- orig_fit$control$seeds
weighted_fit <- train(target ~ .,
                      data = df_train_selected,
                      method = "gbm",
                      tuneGrid = grid_hyperp,
                      verbose = TRUE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = control)

saveRDS(weighted_fit, "../models/weighted_fit.rds")


weighted_fit %>%
        test_roc(data = df_train_selected) %>%
        auc()


weighted_fit %>%
        test_roc(data = df_train_selected) %>%
        auc()
## Area under the curve: 0.9575
confusionMatrix(predict(weighted_fit,df_train_selected[ , !colnames(df_train_selected) %in% ("target")]),df_train_selected$target)
print(weighted_fit)
plot(weighted_fit)
vImpGbm=varImp(weighted_fit) #Variable importance
plot(vImpGbm)





#------------------------#
#SCORE MODELS
df_test_selected <- from_csv_to_dataset("../data/2_eda_prep/test_data_94.csv")

confusionMatrix(predict(orig_fit,df_test_selected[ , !colnames(df_test_selected) %in% ("target")]),df_test_selected$target)
confusionMatrix(predict(weighted_fit,df_test_selected[ , !colnames(df_test_selected) %in% ("target")]),df_test_selected$target)


results_list_roc <- list(NA)
num_mod <- 1


model_list <- list(original = orig_fit,
                   weighted = weighted_fit)

model_list_roc <- model_list %>%
        map(test_roc, data = df_test_selected)

model_list_roc %>%
        map(auc)


for(the_roc in model_list_roc){
        
        results_list_roc[[num_mod]] <- 
                data_frame(tpr = the_roc$sensitivities,
                           fpr = 1 - the_roc$specificities,
                           model = names(model_list)[num_mod])
        
        num_mod <- num_mod + 1
        
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
        geom_line(aes(color = model), size = 1) +
        scale_color_manual(values = custom_col) +
        geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
        theme_bw(base_size = 18)

