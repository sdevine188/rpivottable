library(reshape2)
library(dplyr)
library(stringr)
library(lazyeval)

# df %>%
#         select(-matches(drp)) %>%
#         group_by_(key) %>%
#         summarise_(sum_val = interp(~sum(var, na.rm = TRUE), var = as.name(val)))

# https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
# http://stackoverflow.com/questions/26724124/standard-evaluation-in-dplyr-summarise-on-variable-given-as-a-character-string
# http://www.carlboettiger.info/2015/02/06/fun-standardizing-non-standard-evaluation.html
# http://stackoverflow.com/questions/31843638/how-to-do-rowsums-over-many-columns-in-dplyr-or-tidyr
# http://stackoverflow.com/questions/25571547/select-unique-values-with-select-function-in-dplyr-library

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = Sum", "rendererName = Table")

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = Count Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = ", "rows[[1]] = Region.Name", "vals[[1]] = Appl.State.Abbr", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = List Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = ", "rows[[1]] = Appl.State.Abbr", "vals[[1]] = FY", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = List Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = ", "rows[[1]] = Appl.State.Abbr", "rows[[2]] = Region.Name", "vals[[1]] = FY", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = List Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "vals[[1]] = Appl.State.Abbr", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = List Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = List Unique Values", "rendererName = Table")

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = ", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = Count", "rendererName = Table")

# pivot <- shiny_small %>%
#         filter(FY > 2014) %>%
#         group_by(Region.Name, Appl.State.Abbr, FY) %>%
#         summarize(
#                 funding = sum(EDA.Funding)
#         ) %>%
#         melt(id.vars = c("Region.Name", "Appl.State.Abbr", "FY")) %>%
#         dcast(Region.Name + Appl.State.Abbr ~ FY + variable)

# x <- data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
#         summarize_(Totals = interp(~str_c(unique(.$vals), sep = " "), vals = as.name(vals))) %>% 
#         melt(id.vars = var_groups) %>% dcast(dcast_formula)
# 
# shiny_small %>% select(Appl.State.Abbr) %>% unique(.)
# shiny_small %>% select(Region.Name, Appl.State.Abbr) %>% group_by(Region.Name) %>% distinct(Appl.State.Abbr)
# 
# shiny_small %>% group_by(Region.Name) %>% summarize(totals = length(unique(Appl.State.Abbr)))
# shiny_small %>% group_by(Region.Name) %>% summarize(totals = str_c("test", "test2", sep = " "))
# shiny_small %>% group_by(Region.Name) %>% summarize(totals = str_c(distinct(Appl.State.Abbr), sep = " "))


# count unique values
x <- data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
        summarize(count_unique_values = length(unique(EDA.Funding))) %>% 
        melt(id.vars = var_groups) %>% dcast(dcast_formula) %>% 
        mutate(total = sum(select(data, matches(".count_unique_values.")), na.rm = TRUE))

# list unique values
# this works
dcast_column_plus <- if(!(is.null(pivot_args$cols))) {
        "+"
}
dcast_formula <- as.formula(paste("row_id ~ ", paste(pivot_args$rows, collapse = " + "), paste(dcast_column_plus, pivot_args$cols, collapse = " + ")))

unique_values_df <- data %>% filter_(.dots = exclusions) %>% select_(.dots = var_groups, vals) %>% 
        distinct(.) %>% melt(id.vars = var_groups) %>% arrange_(.dots = var_groups) %>% 
        mutate(row_id = seq_along(.[ , 1])) %>% dcast(dcast_formula)
        
unique_values_list <- apply(unique_values_df, 2, unique)[-1]
str_unique_values_list <- lapply(unique_values_list, function(x) {str_c(str_replace_na(sort(x)), collapse = " ")})
# unique_values_col <- c()

# split col names on underscore, confirm number of terms equals dcast formula, if not, match terms to var_groups from dcast formula
# then c() all the unique row elements together to form columns, then rbind all these rows, and rename columns

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




write_csv(unique_row_df, "unique_row_df.csv")


        
# for(i in 1:length(str_unique_values_list)) {
#         unique_values <- unlist(str_unique_values_list[i], use.names = FALSE)
#         unique_values_col <- c(unique_values_col, unique_values)
# }
# unique_values_col <- data.frame(Totals = unique_values_col)
# 
# pivot_table <- data %>% filter_(.dots = exclusions) %>% select_(.dots = var_groups) %>% distinct_(.) %>% 
#         arrange_(.dots = var_groups) %>% cbind(., unique_values_col)
        

        
      
# this works
y <- data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
        summarize(count_unique_values = length(unique(EDA.Funding))) %>% 
        melt(id.vars = var_groups) %>% dcast(dcast_formula)

y <- y %>% mutate(totals = y %>% rowwise() %>% 
        select(contains("count_unique_values")) %>% rowSums(., na.rm = TRUE))


pivot_count <- function(pivot_args, data){
      var_groups <- c(pivot_args$rows, pivot_args$cols)
      
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
              dots <- lapply(1:length(exclusion_list), function(i){
                      value <- exclusion_list[[i]]
                      col_name <- names(exclusion_list)[i]
                      interp(~y != x, .values = list(y = as.name(col_name), x = value))
              })
              dots
      }
      dots <- create_exclusion_dots_for_filter(pivot_args$exclusions)
      dcast_formula <- as.formula(paste(paste(paste(pivot_args$rows, collapse = " + "), " ~ "), paste(pivot_args$cols, " + variable", collapse = " + ")))
      data %>% filter_(.dots = dots) %>% group_by_(.dots = var_groups) %>% summarize(count = n()) %>% melt(id.vars = var_groups) %>% 
              dcast(dcast_formula)
}

pivot_table <- pivot_count(pivot_args, shiny_small)


x <- data %>% filter_(.dots = dots) %>% group_by_(.dots = var_groups) %>% summarize(count = n()) %>% melt(id.vars = var_groups) %>% 
        dcast(dcast_formula)
dim(x)
head(x)
filter(x, Region.Name == "Atlanta", Appl.State.Abbr == "MS")


pivot_args$exclusions
exclusions <- list(Appl.State.Abbr = "AK", Appl.State.Abbr = "AL", Appl.State.Abbr = "AR", FY = "1995")
var1 <- "Appl.State.Abbr"
val1 <- "AK"
var2 <- "Appl.State.Abbr"
val2 <- "AL"

# this works??
ex_list <- list()
for(i in 1:length(pivot_args$exclusions)) {
        is.even <- function(x) x %% 2 == 0
        if(is.even(i)) {
                ex_list[[i - 1]] <- pivot_args$exclusions[i]
                print(ex_list[[i - 1]])
                names(ex_list)[i - 1] <- pivot_args$exclusions[i - 1]
        }
}

ex_list <- list()
ex_list[[1]] <- "test1"
ex_list[[2]] <- "test2"
names(ex_list)[1] <- "name1"

y <- c("cyl", "gear")
dots = sapply(y, . %>% {as.formula(paste0('~', .))})
interp(~ mean(var), var = as.name("mpg"))
interp(~ x + y, .values = list(x = 10))
pivot_args$exclusions
shiny_small %>% filter_(var %in% val) %>% head(.)
shiny_small %>% filter_(interp(quote(var %in% val), var = as.name("var"), val = as.name("val"))) %>% head(.)
shiny_small %>% filter_(filter_criteria) %>% head(.)
shiny_small %>% filter(Appl.State.Abbr == "AK") %>% head(.)
z <- shiny_small %>% filter_(filter_criteria)
z <- shiny_small %>% filter_(.dots = dots)
z1 <- shiny_small %>% filter(Appl.State.Abbr != "AK", Appl.State.Abbr != "AL", Appl.State.Abbr != "AR", FY != "1995")
dim(z)
dim(z1)
dim(shiny_small)
length(unique(z$Appl.State.Abbr))
which(is.na(z$Appl.State.Abbr))
length(unique(z$FY))
length(unique(z1$Appl.State.Abbr))
which(is.na(z1$Appl.State.Abbr))
length(unique(z1$FY))
length(unique(shiny_small$Appl.State.Abbr))
length(unique(shiny_small$FY))
sort(unique(shiny_small$Appl.State.Abbr))
sort(unique(z$Appl.State.Abbr))
unique(shiny_small$Appl.State.Abbr)[which(!(unique(shiny_small$Appl.State.Abbr) %in% z$Appl.State.Abbr))]
unique(shiny_small$FY)[which(!(unique(shiny_small$FY) %in% z$FY))]

unique(z$Appl.State.Abbr)[which(!(unique(z$Appl.State.Abbr) %in% z1$Appl.State.Abbr))]
unique(z$FY)[which(!(unique(z$FY) %in% z1$FY))]

unique(z$Appl.State.Abbr)[which(!(unique(z$Appl.State.Abbr) %in% z1$Appl.State.Abbr))]
unique(z$FY)[which(!(unique(z$FY) %in% z1$FY))]

which(!(unique(shiny_small$FY) %in% z$FY))
which(!(unique(shiny_small$FY) %in% z$FY))


dim(z)
dim(shiny_small)



exclusions <- list(Appl.State.Abbr = "AK", Appl.State.Abbr = "AL", Appl.State.Abbr = "AR", FY = "1995")
dots <- lapply(names(exclusions), function(col_name){
        value <- exclusions[[col_name]]
        interp(~y != x, 
               .values = list(y = as.name(col_name), x = value))
})

# this works
dots <- lapply(1:length(exclusions), function(i){
        value <- exclusions[[i]]
        col_name <- names(exclusions)[i]
        interp(~y != x, 
               .values = list(y = as.name(col_name), x = value))
})

# this works 3
pivot_args$exclusions
value <- list(pivot_args$exclusions[[1]][1])

create_exclusion_dots_for_filter <- function(exclusions) {
        exclusion_list <- list()
        for(i in 1:length(exclusions)) {
                for(v in 1:length(exclusions[[i]])) {
                        value <- list(exclusions[[i]][v])
                        exclusion <- setNames(value, names(exclusions[i]))
                        exclusion_list <- c(exclusion_list, exclusion)
                }
        }
        
        dots <- lapply(1:length(exclusion_list), function(i){
                value <- exclusion_list[[i]]
                col_name <- names(exclusion_list)[i]
                interp(~y != x, 
                       .values = list(y = as.name(col_name), x = value))
        })
        
        dots
}

dots <- create_exclusion_dots_for_filter(pivot_args$exclusions)

# exclusion_list <- list()
# for(i in 1:length(pivot_args$exclusions)) {
#         for(v in 1:length(pivot_args$exclusions[[i]])) {
#                 value <- list(pivot_args$exclusions[[i]][v])
#                 exclusion <- setNames(value, names(pivot_args$exclusions[i]))
#                 exclusion_list <- c(exclusion_list, exclusion)
#         }
# }
# 
# dots <- lapply(1:length(exclusion_list), function(i){
#         value <- exclusion_list[[i]]
#         col_name <- names(exclusion_list)[i]
#         interp(~y != x, 
#                .values = list(y = as.name(col_name), x = value))
# })

# this works 2
length(pivot_args$exclusions[[1]])
names(pivot_args$exclusions[1])
pivot_args$exclusions[1]
dots <- lapply(1:length(pivot_args$exclusions), function(i){
                dots_term_collection <- c()
                for(v in 1:length(pivot_args$exclusions[[i]])) {
                        value <- pivot_args$exclusions[[i]][v]
                        col_name <- names(pivot_args$exclusions)[i]
                        print(interp(~y != x, .values = list(y = as.name(col_name), x = value)))
                        dots_term_collection <- c(dots_term_collection, interp(~y != x, .values = list(y = as.name(col_name), x = value)))
                }
                dots_term_collection
        })

dots <- for(i in 1:length(exclusions)){
                value <- exclusions[[i]]
                col_name <- names(exclusions)[i]
                interp(~y != x, 
                       .values = list(y = as.name(col_name), x = value))
        }



# this one should work
downloader::download("https://github.com/cboettig/2015/raw/fc0d9185659e7976927d0ec91981912537ac6018/assets/data/2015-02-06-taxa.csv", "taxa.csv")
all_taxa <- read.csv("taxa.csv")

query <- list(Family = 'Scaridae', SpecCode = 5537)
dots <- lapply(names(query), function(level){
        value <- query[[level]]
        interp(~y == x, 
               .values = list(y = as.name(level), x = value))
})

x3 <-  filter_(all_taxa, .dots = dots)
x3

x4 <- filter(all_taxa, Family != "Scaridae", SpecCode != "5537")
dim(x4)
dim(all_taxa)
query <- list(Family = 'Scaridae', SpecCode = 5537)
dots <- lapply(names(query), function(level){
        value <- query[[level]]
        interp(~y != x, 
               .values = list(y = as.name(level), x = value))
})

x5 <-  filter_(all_taxa, .dots = dots)
dim(x5)



###
filter_criteria <- str_c(var1, "==", val1)
filter_criteria <- str_c(filter_criteria, collapse = ", ")
interp(filter_criteria)
filter_criteria <- c("Appl.State.Abbr=='AK'", "Appl.State.Abbr=='AL'", "Appl.State.Abbr=='AR'")
c1 <- "Appl.State.Abbr=='AK'"
c2 <- "Appl.State.Abbr=='AL'"
c3 <- "Appl.State.Abbr=='AR'"
filter_criteria <- interp(~ which_column == "AK", which_column = as.name("Appl.State.Abbr"))
var1 <- "Appl.State.Abbr"
val1 <- c("'AK'", "'AL'", "'AR'")
val1 <- pivot_args$exclusions[2]

###




df <- data.frame(
        v1 = sample(5, 10, replace = TRUE),
        v2 = sample(5,10, replace = TRUE)
)
filter_criteria <- interp(~ which_column == 1, which_column = as.name("v1"))
df %>% filter_(filter_criteria)



df <- read.table(text = 
                         "id   sth1    tg1_num   sth2    tg2_num    others   
                 1     dave    2         ca      35         new
                 2     tom     5         tn      -3         old
                 3     jane    -3        al       0         new
                 4     leroy   0         az      25         old
                 5     jerry   4         mi      55        old", header=TRUE)
pattern <- "_num$"
ind <- grep(pattern, colnames(df))
target_columns <- colnames(df)[ind]

dots <- lapply(target_columns, function(cols){
        interp(~y >= 0, .values = list(y = as.name(cols)))
})

filter_(df, .dots = dots)  
