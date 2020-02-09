library(dplyr)

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
        file = "../data/0_setup/census_income_learn.csv",
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

saveRDS(raw_data_changed_dtypes, "../data/1_input_data/raw_data_changed_dtypes.rds")

#check dtypes are now correct
print(sapply(raw_data_changed_dtypes, class))


#TARGET VARIABLE: REMAP TO 0, 1
#levels(raw_data_changed_dtypes$target)
#levels(raw_data_changed_dtypes$target) <- c(0, 1)


#RAW DATA: FILTER BY YEAR (1994)
#Consider only 1994 data
input_data_94 <- raw_data_changed_dtypes[raw_data_changed_dtypes$year == 94, ]


#saveRDS(input_data_94, "../data/1_input_data/input_data_94.rds")
write.csv(input_data_94, "../data/1_input_data/input_data_94.csv", row.names = FALSE)


#METADATA: SAVE DATA TYPES
write.csv(as.data.frame(sapply(input_data_94, class)), "../docs/metadata/R_data_types.csv")
