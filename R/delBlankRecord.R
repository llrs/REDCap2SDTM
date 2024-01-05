delBlankRecord <- function(df, exclude){
  cols <- names(df)
  cols_ <- cols[!(cols %in% exclude)]
  rlt <- c()
  for (i in 1:nrow(df)){
    flg <- FALSE
    for (j in 1:length(cols_)){
      bb <- !(df[i, cols_[j]] %in% c("", NA, NULL))
      flg <- flg + bb
    }
    if (flg == 0) {
      rlt <- append(rlt, -i)
    }
  }
  return(df[rlt, ])
}