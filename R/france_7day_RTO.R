

#' 7day RTE-France forecast download
#'
#' @param ddate a date object with ymd format
#' @param hour.freq a logical object for getting data in hourly
#'
#' @return a dataframe with the scraped forecast data
#' @export
#'
#' @examples
#' fr_7forecast_dw(Sys.Date())
#' fr_7forecast_dw("2017-09-25", hour.freq=F )
fr_7forecast_dw=function(ddate, hour.freq=T){
  ### function to download forecast data
    ddate=lubridate::ymd(ddate)
    x=NULL
    url=paste("http://clients.rte-france.com/servlets/PrevConsoServlet?T=J&J=", format((ddate), "%d/%m/%Y"), sep="")
    download.file(url, "prevision_conso.zip")
    unzip("prevision_conso.zip")

    x= list(readLines(paste("previ_conso",
                            format(ddate, "%d-%m-%Y"), ".xls", sep = "" ) , warn = F))

  file.remove("prevision_conso.zip")
  file.remove(paste("previ_conso",
                    format(ddate, "%d-%m-%Y"), ".xls", sep = "" ))

  y=NULL
  for (i in 2:length(x[[1]]) ){
    y=rbind (y, data.frame(
      X= stringr::str_split(x[[1]][i], "\t")[[1]][1],
      HS= stringr::str_split(x[[1]][i], "\t")[[1]][2],
      LF= stringr::str_split(x[[1]][i], "\t")[[1]][3]), stringsAsFactors=F )

  }
  y=na.omit(y)
  y=y[-c(1:6),]
  y$HS=y$HS %>% substr(1,5)

  y$index=paste(y$X,y$HS)%>%lubridate::dmy_hm()
   y$X=NULL
   y$HS=NULL


  y$LF=y$LF%>%as.character()%>%as.integer()
  y <- data.frame(y)

  names(y)=c("L","X")
if (hour.freq==T){
  y <- dplyr::group_by(y,date(X), hour(X)) %>% dplyr::summarise(L=mean(L, na.rm = T)) %>% data.frame()

  y$X <- paste(y$date.X., stringr::str_pad(y$hour.X.,2,pad="0")) %>% lubridate::ymd_h(tz = "CET") %>% format("%Y-%m-%d %H:%M CET")
  y <- y[,c("X","L")]
  names(y) <- c("Hours_Starting" ,"Load_Forecast")
  return(y)
} else {
  names(y) <- c("Hours_Starting" ,"Load_Forecast")
  data.frame(y)%>%dplyr::select(Load_Forecast,Hours_Starting) %>%  return()
}

}

#fr_7forecast_dw(Sys.Date()-1,F)
