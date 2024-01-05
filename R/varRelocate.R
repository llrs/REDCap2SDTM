varRelocate <- function(df){
domain <- df[1, "DOMAIN"]
var.md <- read.csv(system.file("extdata", "sdtm.variable.metadata.csv", package="REDCap2SDTM"), sep=",", stringsAsFactors = FALSE)
var.md <- subset(var.md, Domain == domain, select = c("Variable", "Order"))
orgOrder <- seq(1:length(names(df)))
Variable <- names(df)
df1 <- data.frame(orgOrder, Variable, stringsAsFactors = FALSE)
df2 <- merge(var.md, df1, by = "Variable", all = TRUE)
df2 <- df2[order(df2$Order),]
df <- df[,df2$orgOrder]
return(df)
}
