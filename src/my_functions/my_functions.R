#FUNCTION TO ADD IN EXTERNAL MODULE
df_deduplications <- function(df, out = c("dup", "dedup"), sum_over = NULL) {
        #INPUT
                #df: data frame
                #out: define if function returns duplicated (dup) records or deduplicated (dedup)
                #sum_over: name of the field to sum over
         
        #OUTPUT
                #output = data.frame
        
        #CHECK
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
