source("libraries.R")

# paste text
"%+%" <- function(a,b) paste(a,b,sep="")

# check if list of cryptofresh moneda-par urls exists
users_exist <- function(user_names) {
  url_users <- 'http://cryptofresh.com/u/'%+%'moneda-par.'%+%user_names
  out <- url_users %>% map_lgl(RCurl::url.exists) %>% setNames(user_names)
  return(out)
}

# get html of a cryptofresh moneda-par account
scrape_html_cfresh <- function(user_name) {
  url_user <- 'http://cryptofresh.com/u/'%+%'moneda-par.'%+%user_name
  if (RCurl::url.exists(url_user)) {
    html <- readLines(url_user)
    return(html)
  } else {
    warning("user name does not exist")
  }
}

# parse cryptofresh html of a given moneda-par user
user_transactions_data <- function(html_strings, user_name, 
                                   time_interval=param$interval, token=param$token) {
  # regex
  pattern_token <- 'sent ([[:digit:]]*) <a href=\"/a/'%+%token%+%'\">'
  pattern_send_token <- '^((?!'%+%user_name%+%').)*$'
  pattern_get_token <- user_name
  pattern_datetime <- 'datetime([[:print:]]*)>([[:print:]]*)</time>'
  pattern_recipient <- '\\"\\>(?:moneda\\-par\\.)([[:print:]]+)</a>'
  pattern_issuer <- '\\"\\>(?:moneda\\-par\\.)([[:print:]]+)</a>'
  # index (all, send and get)
  i_token <- str_which(html_strings, pattern_token)
  i_send <- str_subset(html_strings, pattern_token) %>% 
    str_subset(pattern_send_token) %>%
    {which(html_strings %in% .)}
  i_get <- str_subset(html_strings, pattern_token) %>% 
    str_subset(pattern_get_token) %>%
    {which(html_strings %in% .)}
  # value of each transaction
  value_send <- if (length(i_send)==0) 0 else { 
    html_strings[i_send] %>% str_match(pattern_token) %>% "["(,2) %>% as.numeric()}
  value_get <- if (length(i_get)==0) 0 else { 
    html_strings[i_get] %>% str_match(pattern_token) %>% "["(,2) %>% as.numeric()}
  # datetime of each transaction
  datetime_send <- if (length(i_send)==0) dmy_hms(NA) else { 
    html_strings[i_send+2] %>% str_match(pattern_datetime) %>% "["(,3) %>%
      as_datetime() %>% with_tz(param$timezone)}
  datetime_get <- if (length(i_get)==0) dmy_hms(NA) else { 
    html_strings[i_get+2] %>% str_match(pattern_datetime) %>% "["(,3) %>%
      as_datetime() %>% with_tz(param$timezone)}
  # recipient/issuer of each transaction
  recipient_send <- if (length(i_send)==0) NA else { 
    html_strings[i_send] %>% str_match(pattern_recipient) %>% "["(,2)}
  issuer_get <- if (length(i_get)==0) NA else { 
    html_strings[i_get-1] %>% str_match(pattern_issuer) %>% "["(,2)}
  # output dataframe
  out <- data.frame(
    datetime=c(datetime_send, datetime_get),
    type=c(rep("send",length(value_send)), rep("get",length(value_get))), 
    value=c(value_send, value_get),
    'recipient/issuer'=c(recipient_send, issuer_get),
    stringsAsFactors=F
  )
  # filter within interval, order by -datetime
  out_filter <- if (all(is.na(out$datetime))) out else {
    out %>% dplyr::filter(datetime %within% time_interval) %>%
      arrange(-(as.numeric(datetime)))}
  
  return(out_filter)
}

# summarise activity of parsed html by 'user_transactions_data'
user_summary <- function(tr_data) {
  # sumary values of transactions
  values <- tr_data %>% mutate(type=recode(type, "send"="buy", "get"="sell")) %>% 
    group_by(type) %>% 
    summarise(n=n(), value=sum(value)) %>% 
    mutate(n=ifelse(value==0,0,n),
           id_temp=1) %>% 
    as.data.frame %>% 
    reshape(timevar="type",idvar="id_temp",direction="wide",sep="_") %>% 
    select(-id_temp)
  # fill empty dataframe with values to avoid non-existent columns
  out <- data.frame(n_buy=0, value_buy=0, n_sell=0, value_sell=0) %>% 
    bind_rows(values) %>% replace(is.na(.), 0) %>% summarise_all(sum) %>%
    mutate(balance=value_sell-value_buy)  
  return(out)
}
