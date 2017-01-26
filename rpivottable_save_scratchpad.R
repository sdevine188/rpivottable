# http://stackoverflow.com/questions/33214397/download-rpivottable-ouput-in-shiny
# https://github.com/smartinsightsfromdata/rpivotTable

library(rpivotTable)
library(shiny)
library(stringr)
library(dplyr)
library(reshape2)
library(lazyeval)
library(readr)

# data(HairEyeColor)
# rpivotTable(data = HairEyeColor, rows = "Hair",cols="Eye", vals = "Freq", aggregatorName = "Sum", rendererName = "Table", width="100%", height="400px")
setwd("C:/Users/Stephen/Desktop/R/rpivottable")
data <- read_csv("shiny_small.csv")
shiny_small <- data %>% select(Appl.State.Abbr, Region.Name, EDA.Funding, FY, Local.Applicant.Match)
# rpivotTable(shiny_small)

# pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")", 
#                   "aggregatorName = Sum", "rendererName = Table") 

list_to_string <- function(obj, listname) {
        if (is.null(names(obj))) {
                paste(listname, "[[", seq_along(obj), "]] = ", obj,
                      sep = "", collapse = "\n")
        } else {
                paste(listname, "$", names(obj), " = ", obj,
                      sep = "", collapse = "\n")
        }
}

server <- function(input, output) {
        
        value_test <- reactive({
                cnames <- list("cols","rows","vals", "exclusions","aggregatorName", "rendererName")
                # Apply a function to all keys, to get corresponding values
                allvalues <- lapply(cnames, function(name) {
                        item <- input$myPivotData[[name]]
                        if (is.list(item)) {
                                list_to_string(item, name)
                        } else {
                                paste(name, item, sep = " = ")
                        }
                })
                # allvalues_string <- paste(allvalues, collapse = "\n")
                # unlist(allvalues_string)
                
                paste(allvalues, collapse = "\n")
        })
        
        
        output$pivotRefresh <- renderText({
                
                cnames <- list("cols","rows","vals", "exclusions", "aggregatorName", "rendererName")
                # Apply a function to all keys, to get corresponding values
                allvalues <- lapply(cnames, function(name) {
                        item <- input$myPivotData[[name]]
                        if (is.list(item)) {
                                list_to_string(item, name)
                        } else {
                                paste(name, item, sep=" = ")
                        }
                })
                paste(allvalues, collapse = "\n")
        })
        
        output$mypivot = renderRpivotTable({
                rpivotTable(data=shiny_small, onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }"))
        })
        
        output$test <- renderPrint({
                # test <- input$myPivotData
                test <- value_test()
                # test
                test <- unlist(str_split(test, "\n"))
                # test[1:3]
                test
        })
        
        # create pivot download file
        download_file <- reactive({
                value_test <- value_test()
                pivot_string <- unlist(str_split(value_test, "\n"))
                
                # transform data based on pivot_string
                
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
                
                # handle exclusions
                create_exclusion_dots_for_filter <- function(exclusions) {
                        exclusion_list <- list()
                        for(i in 1:length(exclusions)) {
                                for(v in 1:length(exclusions[[i]])) {
                                        value <- list(exclusions[[i]][v])
                                        exclusion <- setNames(value, names(exclusions[i]))
                                        exclusion_list <- c(exclusion_list, exclusion)
                                }
                        }
                        exclusions <- lapply(1:length(exclusion_list), function(i){
                                value <- exclusion_list[[i]]
                                col_name <- names(exclusion_list)[i]
                                interp(~y != x, .values = list(y = as.name(col_name), x = value))
                        })
                        exclusions
                }
                
                # transform data
                create_pivot_table <- function(pivot_args, data){
                        var_groups <- c(pivot_args$rows, pivot_args$cols)
                        exclusions <- create_exclusion_dots_for_filter(pivot_args$exclusions)
                        vals <- pivot_args$vals 
                        dcast_column_plus <- if(!(is.null(pivot_args$cols))) {
                                "+"
                        }
                        dcast_formula <- as.formula(paste(paste(paste(pivot_args$rows, collapse = " + "), " ~ "), paste(pivot_args$cols, dcast_column_plus, " variable", collapse = " + ")))
                        if(pivot_args$aggregatorName == "Count") {
                                return(data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% summarize(Count = n()) %>% melt(id.vars = var_groups) %>% 
                                        dcast(dcast_formula))
                        }
                        if(pivot_args$aggregatorName == "Count Unique Values") {
                                pivot_table <- data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
                                        summarize_(Count_Unique_Values = interp(~length(unique(vals)), vals = as.name(vals))) %>% 
                                        melt(id.vars = var_groups) %>% dcast(dcast_formula)
                                
                                return(pivot_table %>% mutate(Totals = pivot_table %>% rowwise() %>% 
                                        select(contains("Count_Unique_Values")) %>% rowSums(., na.rm = TRUE)))
                        }
                        if(pivot_args$aggregatorName == "List Unique Values") {
                                dcast_column_plus <- if(!(is.null(pivot_args$cols))) {
                                        "+"
                                }
                                dcast_formula <- as.formula(paste("row_id ~ ", paste(pivot_args$rows, collapse = " + "), paste(dcast_column_plus, pivot_args$cols, collapse = " + ")))
                                
                                unique_values_df <- data %>% filter_(.dots = exclusions) %>% select_(.dots = var_groups, vals) %>% 
                                        distinct(.) %>% melt(id.vars = var_groups) %>% arrange_(.dots = var_groups) %>% 
                                        mutate(row_id = seq_along(.[ , 1])) %>% dcast(dcast_formula)
                                
                                unique_values_list <- apply(unique_values_df, 2, unique)[-1]
                                str_unique_values_list <- lapply(unique_values_list, function(x) {str_c(str_replace_na(sort(x)), collapse = " ")})
                                
                                # create unique row identifier using row combinations
                                # this does account for multiple rows variables
                                unique_col_names <- sapply(names(str_unique_values_list), function(x) { str_locate_all(x, "_") })
                                col_name_length <- lapply(unique_col_names, function(x) { length(x) / 2 })
                                max_col_name_length <- max(unlist(col_name_length, use.names = FALSE))
                                num_col_vars <- length(pivot_args$rows) + length(pivot_args$cols)
                                split_col_names <- lapply(names(str_unique_values_list), function(x) { unlist(str_split(x, "_"))})
                                if(max_col_name_length == num_col_vars - 1) {
                                        row_var_count <- length(pivot_args$rows)
                                        unique_rows <- lapply(split_col_names, function(x) { str_c(unlist(x)[1:row_var_count], collapse = "_")})
                                        unique_rows <- unlist(unique(unique_rows), use.names = FALSE)
                                        unique_row_df <- data.frame()
                                        
                                        # find unique column names to include as placeholders if row has NA and so column name defaults to NA
                                        unique_combined_cols <- c()
                                        for(i in 1:length(unique_rows)) {
                                                # get unique values strings for each unique row
                                                row_name <- str_c(unique_rows[i], "_", sep = "")
                                                row_matches <- which(str_sub(names(str_unique_values_list), start = 1, end = nchar(row_name)) == row_name)
                                                combined_cols <- str_replace(names(str_unique_values_list)[row_matches], row_name, "")
                                                unique_combined_cols <- c(unique_combined_cols, combined_cols)
                                        }
                                        unique_combined_cols <- sort(unique(unique_combined_cols))       
                                        
                                        
                                        # create rows based on unique rows        
                                        for(i in 1:length(unique_rows)) {
                                                # get unique values strings for each unique row
                                                row_name <- str_c(unique_rows[i], "_", sep = "")
                                                row_matches <- which(str_sub(names(str_unique_values_list), start = 1, end = nchar(row_name)) == row_name)
                                                row_values <- c(unlist(str_unique_values_list[row_matches], use.names = TRUE))
                                                
                                                # create unique row combining all columns
                                                # not sure why rbind works here instead of cbind, but it creates a df with 1 row and multiple columns
                                                unique_row_rbind <- data.frame(rbind(row_values), row.names = "")
                                                
                                                # add any col names that are missing from row_values due to NAs
                                                unique_col_names <- str_replace(names(row_values), row_name, "")
                                                missing_col_values <- unique_combined_cols[which(!(unique_combined_cols %in% unique_col_names))]
                                                missing_col_index <- which(!(unique_combined_cols %in% unique_col_names))
                                                if(length(missing_col_index) != 0) {
                                                        for(col in missing_col_index) {
                                                                if(col == 1) {
                                                                        unique_row_rbind_left <- unique_row_rbind[col]
                                                                        unique_row_rbind_left[ , col] <- NA
                                                                        unique_row_rbind_right <- unique_row_rbind
                                                                        unique_row_rbind <- cbind(unique_row_rbind_left, unique_row_rbind_right)
                                                                }
                                                                if(col != 1) {
                                                                        unique_row_rbind_left <- unique_row_rbind[1:(col - 1)]
                                                                        unique_row_rbind_right <- unique_row_rbind[-(1:(col - 1))]
                                                                        unique_row_rbind_left[ , col] <- NA
                                                                        unique_row_rbind <- cbind(unique_row_rbind_left, unique_row_rbind_right)
                                                                }
                                                        }
                                                }
                                                
                                                # merge unique row to df
                                                names(unique_row_rbind) <- unique_combined_cols
                                                unique_row_df <- rbind(unique_row_df, unique_row_rbind)
                                        }
                                        
                                        # add row names to final df
                                        name_col <- data.frame(unique_rows)
                                        unique_row_df <- cbind(name_col, unique_row_df)
                                        
                                        # add totals col to final df
                                        unique_row_df$Totals <- NA
                                        for(row in 1:length(unique_rows)) {
                                                total_unique_row_values <- c()
                                                row_name <- str_c(unique_rows[row], "_", sep = "")
                                                row_matches <- which(str_sub(names(unique_values_df), start = 1, end = nchar(row_name)) == row_name)
                                                row_values <- unique_values_df[ , row_matches]
                                                if(!(is.null(ncol(row_values)))) {
                                                        row_id <- seq_along(row_values[ , 1])
                                                }
                                                if(is.null(ncol(row_values))) {
                                                        row_id <- seq_along(row_values)
                                                }
                                                row_values <- cbind(row_id, row_values)
                                                unique_row_list <- apply(row_values, 2, unique)[-1]
                                                for(list in 1:length(unique_row_list)) {
                                                        unique_row_values <- unique_row_list[[list]]
                                                        unique_row_values <- unique(unique_row_values)
                                                        total_unique_row_values <- c(total_unique_row_values, unique_row_values)
                                                }
                                                total_unique_row_values <- unique(total_unique_row_values)
                                                total_unique_row_values <- str_c(str_replace_na(total_unique_row_values), collapse = " ")
                                                unique_row_df$Totals[row] <- total_unique_row_values
                                        }
                                        
                                        # add totals row to final df
                                        # convert column types to character to avoid an error generated when trying to add unique_val string to factor columns
                                        names_unique_row_df <- names(unique_row_df)
                                        unique_row_df <- data.frame(sapply(1:ncol(unique_row_df), function(x) 
                                        { unique_row_df[ , x] <- as.character(unique_row_df[ , x]) }), stringsAsFactors = FALSE)
                                        names(unique_row_df) <- names_unique_row_df
                                        
                                        total_row_placeholder <- rep(NA, ncol(unique_row_df))
                                        unique_row_df <- rbind(unique_row_df, total_row_placeholder)
                                        rownames(unique_row_df) <- NULL
                                        for(col in 1:length(unique_combined_cols)) {
                                                total_unique_col_values <- c()
                                                col_name <- str_c("_", unique_combined_cols[col], sep = "")
                                                col_matches <- which(str_sub(names(unique_values_df), start = str_c("-", nchar(col_name))) == col_name)
                                                col_values <- unique_values_df[ , col_matches]
                                                if(!(is.null(ncol(col_values)))) {
                                                        row_id <- seq_along(col_values[ , 1])
                                                }
                                                if(is.null(ncol(col_values))) {
                                                        row_id <- seq_along(col_values)
                                                }
                                                col_values <- cbind(row_id, col_values)
                                                unique_col_list <- apply(col_values, 2, unique)[-1]
                                                for(list in 1:length(unique_col_list)) {
                                                        unique_col_values <- unique_col_list[[list]]
                                                        unique_col_values <- unique(unique_col_values)
                                                        total_unique_col_values <- c(total_unique_col_values, unique_col_values) # this looks wrong?? or just final Total?
                                                }
                                                total_unique_col_values <- unique(total_unique_col_values)
                                                total_unique_col_values <- str_c(str_replace_na(total_unique_col_values), collapse = " ")
                                                unique_row_df[ nrow(unique_row_df), col] <- total_unique_col_values
                                        }
                                        
                                        # add unique values grand total at intersection of column totals and row totals 
                                        unique_row_df[nrow(unique_row_df), ncol(unique_row_df)] <- str_c(str_replace_na(unique_row_df$Totals), collapse = " ")
                                }
                                unique_row_df
                        }
                        
                        # if(pivot_args$aggregatorName == "List Unique Values") {
                        #         data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
                        #                 summarize_(Sum = interp(~sum(vals, na.omit = TRUE), vals = as.name(vals))) %>% melt(id.vars = var_groups) %>% 
                        #                 dcast(dcast_formula)
                        # }

                }
                
                pivot_table <- create_pivot_table(pivot_args, shiny_small)
                
        })
        
        # download pivot data
        output$pivot_data <- downloadHandler(
                filename = function() {
                        str_c("pivot_data", date, ".csv") 
                },
                content = function(file) {
                        write.csv(download_file(), file, row.names = FALSE)
                }
        )
        
}

ui <- shinyUI(fluidPage(
        fluidRow(column(6,   verbatimTextOutput("pivotRefresh")),
                 column(6, rpivotTableOutput("mypivot"))),
        fluidRow(column(6, verbatimTextOutput("test")),
                column(3, align = "center",
                       downloadButton("pivot_data", "Download Data")
                ))
)
)

shinyApp(ui = ui, server = server) 