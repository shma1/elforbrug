
# pjm internal - cleaing 7day load data -----------------------------------


pjm_internal1 <- function(ht,area){
  ht=regmatches(ht, regexpr('Date.+', ht))
  ht <- as.vector(ht)
  ht <- scan(text=ht,what="", quiet = T)

  m <- NULL
  x <- 15
  for (i in 1:7){
    y <- x+26
    m <- cbind(m,ht[x:y])
    x <- y+1
  }
  m <- data.frame(m, stringsAsFactors = F)
  names(m) <-  m[1,] %>% unfactor() %>%  lubridate::mdy()
  m <- m[-c(1,2,15),] %>% mutate_all(.funs = as.numeric)

  m$HS <- str_pad(0:23,2,pad = "0")
  m <- gather(m,X, Load, names(m)[1]: names(m)[7] )
  m$HE <- paste(m$X,m$HS) %>% ymd_h(tz = "EST") %>% format("%Y-%m-%d %H:%M EST")
  m <- m[,c("HE","Load")]
  names(m) <- c("Hour_Starting", area)
  return(m)
}
