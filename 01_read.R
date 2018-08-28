# libraries
source("libraries.R")
# functions
source("functions.R")

# parameters
param_raw <- read.delim("data/parameters.txt", header=F, sep=" ", stringsAsFactors=F) 
param <- as.list(t(param_raw$V2)) %>% setNames(param_raw$V1)
param$interval <- interval(start=ymd_hm(param$datetime_start, tz=param$timezone), 
                           end=ymd_hm(param$datetime_end, tz=param$timezone))
# user names
path_users <- 'data/users_'%+%lubridate::date(param$datetime_start)%+%'.txt'
users <- readLines(path_users, warn=F)

# check if moneda-par users exist
users_iscorrect <- users_exist(users)
if (any(users_iscorrect==F)) {
  stop('NON-EXISTENT NAMES: '%+%paste0(users[!users_iscorrect],collapse=" "))
}

# list of htmls
html_l <- users %>% map(scrape_html_cfresh) %>% setNames(users)

# save useful objects
saveRDS(html_l, file="data/temp/htmllist_temp.RDS")
saveRDS(param, file="data/temp/paramlist_temp.RDS")

# remove not useful objects
rm(param_raw, html_l, param)

message("USERS' HTMLS SUCCESSFULLY READ")