#FUNCTION TO ADD IN EXTERNAL MODULE
df_deduplications <- function(df, out = c("dup", "dedup"), sum_over = NULL) {
        #df: data frame
        #out: define if function returns duplicated (dup) records or deduplicated (dedup)
        #sum_over: name of the field to sum over
        
        #Check df
        if(is.data.frame(df) == FALSE){
                stop("df parameter must be data.frame")
        }
        
        #Check sum_over
        if(sum_over %in% colnames(df) == FALSE){
                stop("sum_over parameter must be in col.names of df")
        } else {
                sum_over_check <- "passed"
        }
        
        if(out == "dup") {
                output <- df[duplicated(df), ]
        } else if (out == "dedup") {
                output <- df[!duplicated(df), ]
        }
        
        if (sum_over_check == "passed") {
                print(
                        paste(
                                "There are",
                                dim(output)[1],
                                "records,\n summing to",
                                sum(output[[sum_over]]),
                                "of", sum_over, "field."
                        )
                )
        } else {
                #error was already handled in check section
                print(
                        paste(
                                "There are",
                                dim(output)[1],
                                "duplicated records."
                        )
                )
        }
        
        return(output)
}

{
        test_data  <- read.csv(file = "../data/raw/census_income_test.csv",
                               header = FALSE,
                               sep = ",",
                               col.names = col_names
        )
        
        test_data_deduplicated <- df_deduplications(test_data, out = "dedup", sum_over = "MARSUPWT")
        
        
        saveRDS(input_data_deduplicated, file = "../data/test.rds")
}


#------------------------#
#METADATA
#Read metadata file containing the column names. The code column will be used.
metadata <- read.csv(file = "../docs/metadata/census_income_metadata-prepared.v00.csv",
                    header = TRUE,
                    sep = ","
                    )

#Pull code to use as column names
col_names <- metadata$description

#------------------------#
#RAW DATA
#Read raw training dataset
raw_data <- read.csv(
        file = "../data/raw/census_income_learn.csv",
        header = FALSE,
        sep = ",",
        col.names = col_names,
        strip.white = TRUE,
        #trim whitespace
        na.strings = "?"
)

#RAW DATA: CHANGE DTYPES
#List of columns for transform to categorical
cols_measurable_to_categorical <- c(
        "own_business_or_self_employed",
        "veterans_benefits",
        "own_business_or_self_employed",
        "detailed_occupation_recode",
        "detailed_industry_recode"
)

#Change dtypes of columns in cols_measurable_to_categorical vector
raw_data_changed_dtypes <- raw_data %>%
        mutate_at(cols_measurable_to_categorical, as.factor)

#check dtypes are now correct
sapply(raw_data_changed_dtypes, class)

#RAW DATA: FILTER BY YEAR (1994)
#Consider only 1994 data
input_data <- raw_data[raw_data$year == 94, ]

#------------------------#
#EDA: DUPLICATES

#Investigate duplicated records
input_data_duplicated <- df_deduplications(input_data, out = "dup", sum_over = "instance_weight")

#Remove duplicated records
input_data_deduplicated <- df_deduplications(input_data, out = "dedup", sum_over = "instance_weight")


#------------------------#
#EDA: INSTANCE_WEIGHT

#Pearson Correlation matrix between count() and sum(instance_weight for each variable)
library(dplyr)
correlation_by_variable <- list()
for (var in colnames(input_data_deduplicated)){
        summarized_values = input_data_deduplicated %>% group_by_at(vars(var)) %>% transmute(n_records = n(), weight = sum(instance_weight))
        correlation_by_variable[[var]] <- cor(summarized_values[2], summarized_values[3])
        print(paste("Correlation between count() and sum(instance_weight) for", var, "is:", correlation_by_variable[[var]]))
}
write.csv(data.frame(unlist(correlation_by_variable), row.names = names (correlation_by_variable )),
          file.path("../notebooks/output/", "p-correlation-instance-weight-all-vars.csv")
)




#------------------------#
#EDA: SUMMARY STATS

#CATEGORICAL
#logical vector to include only categorical variables
categorical <- !unlist(lapply(input_data_deduplicated, is.numeric))

#change position of target in first column for better visualiation        
df_factors_for_summary = data.frame( 
        input_data_deduplicated$target,
        input_data_deduplicated[Reduce("&",
                                       data.frame(categorical, !names(input_data_deduplicated) == "target")
        )]
)

#sort factor levels by descending frequency for better visualization
df_factors_for_summary_sorted_desc <- df_factors_for_summary_sorted <- as.data.frame(lapply(df_factors_for_summary[colnames(df_factors_for_summary)], function(x) reorder(x, x, length)))
for (var in colnames(df_factors_for_summary_sorted)) {
        df_factors_for_summary_sorted_desc[[var]] <-  factor(df_factors_for_summary_sorted[[var]],
                                             levels=rev(levels(df_factors_for_summary_sorted[[var]])))
}

#calculate summary statistics table
categorical_desc_stat <- dfSummary(df_factors_for_summary_sorted_desc,
                 plain.ascii = FALSE, style = "grid", 
                 graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "./tmp"
                 )
view(categorical_desc_stat)


#---------#

#MEASURABLE
#logical vector to include only categorical variables
measurable <- unlist(lapply(input_data_deduplicated, is.numeric))

#calculate summary statistics table
measurable_desc_stat <- dfSummary(input_data_deduplicated[measurable],
                 plain.ascii = FALSE, style = "grid", 
                 graph.magnif = 0.75, valid.col = FALSE, tmp.img.dir = "./tmp"
)

view(measurable_desc_stat)




df <- subset(input_data, select = -c(year))
