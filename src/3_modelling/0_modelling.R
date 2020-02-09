library(caret)

metadata <- read.csv(file = "../docs/metadata/census_income_metadata-prepared.v00.csv",
                     header = TRUE,
                     sep = ","
)

metadata[metadata$r_types == "integer", "r_types"] <-  "numeric"


prepared_data <- read.csv(
        file = "../data/2_eda_prep/df_prepared.csv",
        header = TRUE,
        sep = ",",
        strip.white = TRUE,
        colClasses = as.character(metadata$r_types),
        #trim whitespace
        na.strings = "?"
)


x <- prepared_data[ , !colnames(prepared_data) %in% c("full_or_part_time_employment_stat", "target", "year") ]
y <- prepared_data[ , "target"]
prepared_data_2 <- prepared_data[ , !colnames(prepared_data) %in% c("full_or_part_time_employment_stat", "year") ]


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        verboseIter = TRUE)

x1 <- prepared_data[ , colnames(prepared_data) %in% metadata[metadata$r_types == "numeric", "description"]]
x <- x1[ , (!colnames(x1) %in% c("full_or_part_time_employment_stat", "target", "year")) ]
prepared_data_2 <- prepared_data[ , (colnames(prepared_data) %in% colnames(x))]
prepared_data_2$target <- prepared_data$target


#allowParallel=TRUE

#Metric compare model is Accuracy
metric <- "Kappa"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(target~., 
                    data=prepared_data_2, 
                    method='ranger', 
                    metric='Accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control,
                    verbose = TRUE)
print(rf_default)