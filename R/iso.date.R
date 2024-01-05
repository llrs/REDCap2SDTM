iso.date <- function(date) {
  
  pre_date <- format_iso_8601(parse_iso_8601(parse_date(date, approx = TRUE)))
  
  if ( length(str_split(date, "[:blank:]" , n = 2)[[1]]) == 1 ) {
    iso_date <- str_split(pre_date, "T")[[1]][1]
  }
  else  {
    iso_date <- pre_date
  }
  
  return(iso_date)  
}
