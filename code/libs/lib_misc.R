

subdist_from_func <- function() {
  
  calls <- sys.calls() %>% as.character() %>% rev()
  
  if(grepl("st[xjt]_[0-9]{1,2}",calls[2])) call <- calls[2] else call <- calls[1] 
  
  gsub(".*_([0-9]{1,2})\\(.*\\)$","\\1",call)
}

isl_from_func <- function() {
  
  calls <- sys.calls() %>% as.character() %>% rev()
  
  if(grepl("st[xjt]_[0-9]{1,2}",calls[2])) call <- calls[2] else call <- calls[1] 
  
  gsub(".*_(st.)_([0-9]{1,2})\\(.*\\)$","\\1",call)
}


state_name <- function(state) {
  st_fips <- fips::state_fips(state)
  fips::fips_st_2020_txt %>% filter(fips == st_fips) %>% pull(state)
  
}

state_abbrev <- function(state) {
  st_fips <- fips::state_fips(state)
  fips::fips_st_2020_txt %>% filter(fips == st_fips) %>% pull(state_abb)
  
}

url_files <- function(url) {
  
  filenames  <-  RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  
  paste(strsplit(filenames, "\r*\n")[[1]], sep = "") %>% 
    grep("alt=.*href=",. , value = T) %>% 
    gsub(".*>(tl_rd.*zip)<.*","\\1", .)
  
}


url_folders <- function(url) {
  
  filenames  <-  RCurl::getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  strsplit(filenames, "\r*\n")[[1]] %>% 
    gsub("[\"]", "'", .) %>% 
    grep("[DIR]",. , fixed = TRUE, value = T) %>% 
    gsub(".*href=.*?[>](.*)?/</a>.*","\\1",.)
  
}
