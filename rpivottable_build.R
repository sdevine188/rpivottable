# http://stackoverflow.com/questions/33214397/download-rpivottable-ouput-in-shiny
# https://github.com/smartinsightsfromdata/rpivotTable

library(rpivotTable)
library(shiny)
library(stringr)

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")", 
                  "aggregatorName = Sum", "rendererName = Table")

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = ", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = Count", "rendererName = Table")

# function to create arguments for pivottable
create_pivot_arguments <- function(pivot_string) {
        
        # create cols arg
        pivot_cols_index <- which(grepl("cols\\[", pivot_string))
        pivot_cols_values <- c() 
        for(i in 1:length(pivot_string[pivot_cols_index])) {
                value <- unlist(str_split(pivot_string[pivot_cols_index][i], "= "))[2]
                if(value == "") {
                        c(pivot_cols_values, NULL)
                } else {
                        pivot_cols_values <- c(pivot_cols_values, value)
                }
        }
        
        # create rows arg
        pivot_rows_index <- which(grepl("rows\\[", pivot_string))
        pivot_rows_values <- c() 
        for(i in 1:length(pivot_string[pivot_rows_index])) {
                value <- unlist(str_split(pivot_string[pivot_rows_index][i], "= "))[2]
                if(value == "") {
                        c(pivot_rows_values, NULL)
                } else {
                        pivot_rows_values <- c(pivot_rows_values, value)
                }
        }
        
        # create vals arg
        pivot_vals_index <- which(grepl("vals\\[", pivot_string))
        pivot_vals <- c() 
        for(i in 1:length(pivot_string[pivot_vals_index])) {
                value <- unlist(str_split(pivot_string[pivot_vals_index][i], "= "))[2]
                if(value == "") {
                        c(pivot_vals, NULL)
                } else {
                        pivot_vals <- c(pivot_vals, value)
                }
        }
        
        # create exclusions arg 
        pivot_exclusions_index <- which(grepl("exclusions\\$", pivot_string))
        pivot_exclusions <- list()
        for(i in 1:length(pivot_string[pivot_exclusions_index])) {
                variable <- unlist(str_split(pivot_string[pivot_exclusions_index][i], "="))[1]
                variable <- str_replace_all(variable, "exclusions\\$", "")
                variable <- str_trim(variable, "right")
                
                values <- unlist(str_split(pivot_string[pivot_exclusions_index][i], "="))[2]
                values <- str_replace_all(values, " list\\(\"", "")
                values <- str_split(values, "\\\",")
                values <- lapply(values, function(x) str_replace_all(x, " \\\"", ""))
                values <- lapply(values, function(x) str_replace_all(x, "\\\"\\)", ""))
                
                names(values) <- variable
                pivot_exclusions <- c(pivot_exclusions, values)
        }
        
        # create aggregatorName arg
        pivot_aggregator_index <- which(grepl("aggregatorName", pivot_string))
        pivot_aggregatorName <- unlist(str_split(pivot_string[pivot_aggregator_index], "= "))[2]
        
        # create rendererName arg
        pivot_renderer_index <- which(grepl("rendererName", pivot_string))
        pivot_rendererName <- unlist(str_split(pivot_string[pivot_renderer_index], "= "))[2]
        
        # create pivot_arguments
        pivot_arguments <- list("cols" = pivot_cols_values, "rows" = pivot_rows_values, "vals" = pivot_vals, "exclusions" = pivot_exclusions, "aggregatorName" = pivot_aggregatorName, 
                                "rendererName" = pivot_rendererName)
        pivot_arguments
}

pivot_args <- create_pivot_arguments(pivot_string)

# function to create exclusions list
create_exclusions_list <- function(exclusions) {
        is.even <- function(x) x %% 2 == 0
        exclusions_list <- list()
        for(i in 1:length(exclusions)) {
                values <- unlist(exclusions[[i]])
                values_list <- list()
                for(v in 1:length(values)) {
                        values_list <- c(values_list, list(values[v]))
                }
                exclusion <- setNames(list(values_list), names(exclusions[i]))
                exclusions_list <- c(exclusions_list, exclusion)
        }
        exclusions_list
}

exclusions <- create_exclusions_list(pivot_args$exclusions)

# build pivottable
rpivotTable(shiny_small, rows = pivot_args$rows, cols = pivot_args$cols, vals = pivot_args$vals, exclusions = exclusions2, 
            aggregatorName = pivot_args$aggregatorName, rendererName = pivot_args$rendererName)
