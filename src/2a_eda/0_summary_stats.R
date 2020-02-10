#--------PACKAGES--------#
library(dplyr)
library(summarytools)
library(tidyverse)
library(lsr)
library(caret)

         
#---------DATA-----------#
if (!(deparse(substitute(prepared_data_94)) %in% ls())) {
        prepared_data_94 = readRDS("../data/2_eda_prep/prepared_data_94")
        print("Loaded data.")
} else {
        print("Data already loaded.")
}

#------------------------#
#EDA: INSTANCE_WEIGHT
#Pearson Correlation matrix between count() and sum(instance_weight for each variable)
correlation_by_variable <- list()
for (var in colnames(prepared_data_94)) {
        summarized_values = prepared_data_94 %>% group_by_at(vars(var)) %>% transmute(n_records = n(),
                                                                                                weight = sum(instance_weight))
        correlation_by_variable[[var]] <-
                cor(summarized_values[2], summarized_values[3])
        print(
                paste(
                        "P Correlation between count() and sum(instance_weight) for",
                        var,
                        "is:",
                        correlation_by_variable[[var]]
                )
        )
}
write.csv(
        data.frame(
                unlist(correlation_by_variable),
                row.names = names (correlation_by_variable)
        ),
        file.path(
                "../notebooks/output/",
                "p-correlation-instance-weight-all-vars.csv"
        )
        
)




#------------------------#
#EDA: SUMMARY STATS

#CATEGORICAL
#logical vector to include only categorical variables
categorical <-
        !unlist(lapply(prepared_data_94, is.numeric))

#change position of target in first column for better visualiation
df_factors_for_summary <- data.frame(target = prepared_data_94$target,
                                     prepared_data_94[Reduce("&",
                                                                       data.frame(categorical,!names(prepared_data_94) == "target"))])

#sort factor levels by descending frequency for better visualization
df_factors_for_summary_sorted_desc <-
        df_factors_for_summary_sorted <-
        as.data.frame(lapply(df_factors_for_summary[colnames(df_factors_for_summary)], function(x)
                reorder(x, x, length)))
for (var in colnames(df_factors_for_summary_sorted)) {
        df_factors_for_summary_sorted_desc[[var]] <-
                factor(df_factors_for_summary_sorted[[var]],
                       levels = rev(levels(df_factors_for_summary_sorted[[var]])))
}

#calculate summary statistics table
categorical_desc_stat <-
        dfSummary(
                df_factors_for_summary_sorted_desc,
                plain.ascii = FALSE,
                style = "grid",
                graph.magnif = 0.75,
                valid.col = FALSE,
                tmp.img.dir = "./tmp"
        )
view(categorical_desc_stat)


#---------#
#MEASURABLE
#logical vector to include only categorical variables
measurable <- unlist(lapply(prepared_data_94, is.numeric))


#calculate summary statistics table
measurable_desc_stat <-
        dfSummary(
                prepared_data_94[measurable],
                plain.ascii = FALSE,
                style = "grid",
                graph.magnif = 0.75,
                valid.col = FALSE,
                tmp.img.dir = "./tmp"
        )

view(measurable_desc_stat)



#--------WORK IN PROGRESS----------------#
#EDA: CORRELATION WITH TARGET


#CATEGORICAL
categorical_correlation <- function(x, y, df) {
        tbl = df %>% select(x, y) %>% table()
        chisq_pval = round(chisq.test(tbl)$p.value, 4)
        cramV = round(cramersV(tbl), 4)
        data.frame(x, y, chisq_pval, cramV)
}


#Chi-squared for all variable
v_explanatory_vars = colnames(df_factors_for_summary[!colnames(df_factors_for_summary) == "target"])
df_combination_var_target <-
        merge(v_explanatory_vars, "target")
df_categorical_variables_corr_to_target <-
        map2_df(df_combination_var_target$x, df_combination_var_target$y, categorical_correlation, df = df_factors_for_summary)


#Chi-squared for every categorical level
v_target <- prepared_data_94["target"]

df_categorical_levels_corr_to_target <- as.data.frame(x = c(), y = c(), chisq_pval = c(), cramV = c())
for (var_name in v_explanatory_vars) {
        df_var <- prepared_data_94[var_name]
        df_var_dummy <- dummyVars("~ .", data = df_var)
        df_target_var_dummy <-
                data.frame(df_target, predict(df_var_dummy, newdata = df_var))
        # create unique combinations of column names
        # sorting will help getting a better plot (upper triangular)
        df_comb <- data.frame(t(combn(sort(
                names(df_target_var_dummy)
        ), 2)), stringsAsFactors = F) %>%
                filter(X2 == "target")
        df_categorical_level_corr_to_target_temp <-
                map2_df(df_comb$X1, df_comb$X2, f, df = df_target_var_dummy)
        df_categorical_levels_corr_to_target = rbind(df_categorical_levels_corr_to_target, df_categorical_level_corr_to_target_temp)
}


#Calculate stats for every categorical level
df_categorical_levels_keys <-
        colsplit(df_categorical_levels_corr_to_target$x,
                 "\\.",
                 c("variable", "level"))

df_categorical_levels_keys[df_categorical_levels_keys$variable == "fill_inc_questionnaire_for_veteran", "variable"]  <-
        "fill_inc_questionnaire_for_veteran.s_admin"

df_categorical_levels_keys[df_categorical_levels_keys$variable == "fill_inc_questionnaire_for_veteran.s_admin", "level"] <-
        levels(data.frame(
                str_split_fixed(df_categorical_levels_keys[df_categorical_levels_keys$variable == "fill_inc_questionnaire_for_veteran.s_admin", "level"], "\\.", 2)
        )$X2)

df_categorical_levels_corr_to_target_keys <-
        cbind(df_categorical_levels_corr_to_target,
              df_categorical_levels_keys)

df_factors_for_summary$target_num =  as.numeric(as.character.factor(df_factors_for_summary$target))
df_var_levels_stats <-
        data.frame(
                level = c(),
                freq = c(),
                target = c(),
                target_ratio = c(),
                freq_rel = c(),
                variable = c()
        )
for (var in v_explanatory_vars) {
        df_var_levels_stats_temp <- df_factors_for_summary %>%
                group_by_at(vars(var)) %>%
                summarise(freq = n(), target = sum(target_num)) %>%
                mutate(target_ratio = target / freq,
                       freq_rel = n() / sum(freq))
        colnames(df_var_levels_stats_temp)[1] <- "level"
        df_var_levels_stats_temp$variable <- var
        df_var_levels_stats <- rbind(df_var_levels_stats, df_var_levels_stats_temp)
}


#Combine all three data.frames
categorical_correlation_result <- merge(merge(
        df_categorical_variables_corr_to_target,
        df_categorical_levels_corr_to_target_keys, by.x = "x", by.y = "variable" , all = TRUE),
        df_var_levels_stats, by.x = c("x", "level"), by.y = c("variable", "level"), all = TRUE)

df_categorical_variables_corr_to_target
df_categorical_levels_corr_to_target_keys
df_var_levels_stats


categorical_correlation_result <-
        data.frame(variable = v_explanatory_vars) %>%
        
        left_join(df_categorical_levels_corr_to_target_keys,
                  by = c("variable" = "variable")) %>%
        left_join(df_categorical_variables_corr_to_target,
                  by = c("variable" = "x")) %>%
        left_join(df_var_levels_stats,
                  by = c("variable" = "variable", "level" = "level"))