getREDCap.CL <- function(CodeListText){
  REDCAP.CL.PR1 <- stringr::str_split(CodeListText, "\\|")
  REDCAP.CL <- list()
  k <- 1
  for (i in REDCAP.CL.PR1[[1]]){
    devided.str <- stringr::str_split(i, ",")[[1]]
    CODE <- stringr::str_trim(devided.str[1])
    TERM <- stringr::str_trim(devided.str[2])
    REDCAP.CL[[k]] <- c(REDCAP.CLI.CODE=CODE, REDCAP.CLI.TERM=TERM)
    k <- k+1
  }
  redcap.cl.df <-  as.data.frame( t(as.data.frame(REDCAP.CL)) )
  rownames(redcap.cl.df) <- NULL
  return(redcap.cl.df)
}
