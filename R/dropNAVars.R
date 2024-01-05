dropNAVars <- function(df){
  vars_ <- names(df)
  drop_cols <-c()
  for( i in 1:length(vars_) ){
     val <- df[,as.character(vars_[i])]
     naFlg <- sum(!is.na(val))
     if (naFlg == 0 ) {
       drop_cols <- append(drop_cols, -i)
     }
  }
  if (length(drop_cols) > 0){
    return(df[,drop_cols])
  }
  else{
    return(df)
  }
}


