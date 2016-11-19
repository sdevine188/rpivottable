library(reshape2)
library(dplyr)
library(stringr)
library(lazyeval)

setwd("C:/Users/Stephen/Desktop/R/rpivottable")
data <- read_csv("shiny_small.csv")
shiny_small <- data %>% select(Appl.State.Abbr, Region.Name, EDA.Funding, FY, Local.Applicant.Match)

pivot_string <- c("cols[[1]] = FY", "rows[[1]] = Region.Name", "rows[[2]] = Appl.State.Abbr", "vals[[1]] = EDA.Funding", "exclusions$Appl.State.Abbr = list(\"AK\", \"AL\", \"AR\")", "exclusions$FY = list(\"1995\")",
                  "aggregatorName = Count", "rendererName = Table")

data %>% filter_(.dots = exclusions) %>% group_by_(.dots = var_groups) %>% 
        summarize_(Sum = interp(~sum(vals, na.omit = TRUE), vals = as.name(vals))) %>% melt(id.vars = var_groups) %>% 
        dcast(dcast_formula)