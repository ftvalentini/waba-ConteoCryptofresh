# functions
source("functions.R")
# libraries
source("libraries.R")

# read saved parameters and html strings saved after script 01
html_l <- readRDS(file="data/temp/htmllist_temp.RDS")
param <- readRDS(file="data/temp/paramlist_temp.RDS")

# detail of transactions for each user
(
  users_tr_l <- map2(
    .x=html_l, .y=names(html_l), 
    function(x,y) user_transactions_data(html_strings=x, user_name=y,
                                         time_interval=param$interval, 
                                         token=param$token))
)
# summary of each users' activity
(
  users_summary_df <- users_tr_l %>% map_dfr(user_summary, .id="usuario")
)
# summary of all transactions
(
  total_summary <- users_summary_df %>% select(-usuario) %>% colSums()
)
# save results
if (param$save_excel=="TRUE") {
  # write list of users transactions in excel (one sheet per user)
  wb <- xlsx::createWorkbook(type="xlsx")
  sheets <- map(sort(names(users_tr_l)), function(x)
    xlsx::createSheet(wb, sheetName=x))
  walk2(users_tr_l[order(names(users_tr_l))], sheets, function(x,y)
        xlsx::addDataFrame(x, sheet=y, row.names=F))
  xlsx::saveWorkbook(wb, file="output/users_transactions.xlsx")
  # write csv of users summary
  write.csv(users_summary_df %>% arrange(usuario), 
            file="output/users_summary.csv", row.names=F)
  # write csv of users summary
  write.csv(t(total_summary), file="output/total_summary.csv", row.names=F)
}

# save RDS to explore results with R
saveRDS(users_tr_l, file="output/rds/users_transactions.RDS")
saveRDS(users_summary_df, file="output/rds/users_summary.RDS")
saveRDS(total_summary, file="output/rds/total_summary.RDS")

# check if any recipient not in 'users' txt 
all_recip_issuers <- users_tr_l %>% map(function(x) x$recipient.issuer) %>% 
  unlist(use.names=F) %>% unique %>% na.omit()
new_users <- base::setdiff(all_recip_issuers, names(html_l))
if (length(new_users)>0) {
  message('USERS NOT IN .txt INPUT FILE: '%+%new_users)
}

if (param$save_excel=="TRUE") message('OUTPUT FILES SAVED IN /output/')
message('.RDS FILES SAVED IN /output/rds/')



