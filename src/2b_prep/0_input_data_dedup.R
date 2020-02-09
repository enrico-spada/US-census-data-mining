source("./my_functions/my_functions.R")

#------------------------#
#INPUT_DATA: DUP/DEDUP
if(!(deparse(substitute(input_data_94)) %in% ls())) {
        input_data_94 = readRDS("../data/1_input_data/input_data_94.rds")
        print("Loaded data.")
} else {
        print("Data already loaded.")
}

#Investigate duplicated records
input_data_94_duplicated <- df_deduplications(input_data_94, out = "dup", sum_over = "instance_weight")

#Remove duplicated records
input_data_94_deduplicated <- df_deduplications(input_data_94, out = "dedup", sum_over = "instance_weight")

saveRDS(input_data_94_deduplicated, "../data/1_input_data/input_data_94_deduplicated.rds")

write.csv(input_data_94_deduplicated, "../data/1_input_data/input_data_94_deduplicated.csv",
          row.names = FALSE)
