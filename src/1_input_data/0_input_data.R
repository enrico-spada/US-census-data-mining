library(dplyr)

input_data_to_csv <- function(file_path, out_name, type = c("train", "test"), year) {
        #------------------------#
        #METADATA
        #Read metadata file containing the column names. The code column will be used.
        metadata <- read.csv(file = "../docs/metadata/census_income_metadata-prepared.v00.csv",
                             header = TRUE,
                             sep = ","
        )
        
        #Pull code to use as column names
        col_names <- metadata$description
        
                raw_data <- read.csv(
                file = file_path,
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
        
        
        #RAW DATA: FILTER BY YEAR 
        input_data <- raw_data_changed_dtypes[raw_data_changed_dtypes$year == year, ]
        
        write.csv(input_data, paste0("../data/1_input_data/input_data_",type, "_", year, ".csv"), row.names = FALSE)
        
        #METADATA: SAVE DATA TYPES
        write.csv(as.data.frame(sapply(input_data, class)), "../docs/metadata/R_data_types.csv")
        
        
        
        
}



#------------------------#
#INPUT DATA
input_data_to_csv(file_path = "../data/0_setup/census_income_learn.csv",type = "train",  year = 94)
input_data_to_csv(file_path = "../data/0_setup/census_income_learn.csv", type = "train",year = 95)
input_data_to_csv(file_path = "../data/0_setup/census_income_test.csv", type = "test",year = 94)
input_data_to_csv(file_path = "../data/0_setup/census_income_test.csv", type = "test",year = 95)
