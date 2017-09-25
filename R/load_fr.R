


# Load file for a particular date -----------------------------------------------------


#' fr_load_dw
#'
#' This function returns the load (Electricity Demand) at a particular day in
#' France. Data Source is RTE
#'
#' @param dag a date object. this is the date for which the load data is
#'   downloaded
#' @param hour.freq a logical object. this is to get hourly frequency if true
#'
#' @return a data.frame that has date and Load (numeric) data
#' @import tidyverse
#' @import stringr
#' @import RCurl
#' @import rvest
#' @import lubridate
#' @importFrom magrittr "%>%"
#'
#' @export fr_load_dw
#'
#' @examples
#' fr_load_dw (Sys.Date(), hour.freq=T)
#' fr_load_dw ("2017-01-01") #date format is ymd
#'
fr_load_dw  <-  function (dag,hour.freq=T){
  dag <-  lubridate::ymd(dag)
  #dag <- as.Date(dag,format="%Y-%m-%d")
  url <- paste("http://www.rte-france.com/curves/eco2mixDl?date=", format(dag, "%d/%m/%Y"), sep="")
  files <- paste("eCO2mix_RTE_", format(dag, "%Y-%m-%d"), ".zip", sep = "")
  download.file(url,files)
  unzip(files)
  x <- list(readLines( paste("eCO2mix_RTE_", format(dag, "%Y-%m-%d"), ".xls", sep = "") ))



  y <- NULL
  for (i in 2:(length(x[[1]])-1) ){
    y <- rbind (y, data.frame(
      X = lubridate::ymd_hm( paste( stringr::str_split(x[[1]][i], "\t")[[1]][3],
                stringr::str_split(x[[1]][i], "\t")[[1]][4] )) ,
      L= as.integer(stringr::str_split(x[[1]][i], "\t")[[1]][5] ),
      stringsAsFactors = F) )
  }

  file.remove(files)
  file.remove(gsub(".zip",".xls", files))

  if (hour.freq==T){
  y <- dplyr::group_by(y,date(X), hour(X)) %>% dplyr::summarise(L=mean(L, na.rm = T)) %>% data.frame()

  y$X <- paste(y$date.X., stringr::str_pad(y$hour.X.,2,pad="0")) %>% lubridate::ymd_h(tz = "CET")
  y=y[,c("X","L")]
  names(y)=c("Hours_Starting" ,"Load")
  return(y)
  } else {
    names(y)=c("Hours_Starting" ,"Load")
    data.frame(y)%>%return()
  }

}


