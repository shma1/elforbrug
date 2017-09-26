


#' PJM seven day load forecast. Scraped from PJM website
#'
#' @return a data frame which has the current pjm 7 day load foreacst
#' @export
#'
#' @examples
#' pjm_7dag_forecast() # no argumets required.. this data is available only for today
pjm_7dag_forecast <- function () {
htt <- getURL("http://oasis.pjm.com/doc/projload.txt")
#Ref-- pjm internal - cleaing 7day load data-- pjm_internal_1.R
ht <- regmatches(htt, regexpr('MID.+AP', htt))
pjm <- pjm_internal1(ht,"MID_ATLANTIC")

ht <- regmatches(htt, regexpr('AP.+AEP', htt))
pjm <- merge(pjm, pjm_internal1(ht,"AP"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('AEP.+DAYTON', htt))
pjm <- merge(pjm, pjm_internal1(ht,"AEP"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('DAYTON.+COMED', htt))
pjm <- merge(pjm, pjm_internal1(ht,"DAYTON"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('COMED.+DUQUESNE', htt))
pjm <- merge(pjm, pjm_internal1(ht,"COMED"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('DUQUESNE.+ATSI', htt))
pjm <- merge(pjm, pjm_internal1(ht,"DUQUESNE"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('ATSI.+DEOK', htt))
pjm <- merge(pjm, pjm_internal1(ht,"ATSI"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('DEOK.+EKPC', htt))
pjm <- merge(pjm, pjm_internal1(ht,"DEOK"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('EKPC.+WESTERN', htt))
pjm <- merge(pjm, pjm_internal1(ht,"EKPC"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('WESTERN.+DOMINION', htt))
pjm <- merge(pjm, pjm_internal1(ht,"WESTERN"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('DOMINION.+SOUTHERN', htt))
pjm <- merge(pjm, pjm_internal1(ht,"DOMINION"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('SOUTHERN.+RTO', htt))
pjm <- merge(pjm, pjm_internal1(ht,"SOUTHERN"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('RTO.+PSE', htt))
pjm <- merge(pjm, pjm_internal1(ht,"RTO"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('PSE.+PECO', htt))
pjm <- merge(pjm, pjm_internal1(ht,"PSE"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('PECO.+PPL', htt))
pjm <- merge(pjm, pjm_internal1(ht,"PECO"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('PPL.+UGI', htt))
pjm <- merge(pjm, pjm_internal1(ht,"PPL"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('UGI.+BG', htt))
pjm <- merge(pjm, pjm_internal1(ht,"UGI"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('BG.+JCP', htt))
pjm <- merge(pjm, pjm_internal1(ht,"BG"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('JCP.+METED', htt))
pjm <- merge(pjm, pjm_internal1(ht,"JCP"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('METED.+PENELEC', htt))
pjm <- merge(pjm, pjm_internal1(ht,"METED"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('PENELEC.+PEPCO', htt))
pjm <- merge(pjm, pjm_internal1(ht,"PENELEC"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('PEPCO.+AE', htt))
pjm <- merge(pjm, pjm_internal1(ht,"PEPCO"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('AE/.+DP', htt))
pjm <- merge(pjm, pjm_internal1(ht,"AE"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('DP.+RECO', htt))
pjm <- merge(pjm, pjm_internal1(ht,"DP"), by="Hour_Starting")

ht <- regmatches(htt, regexpr('RECO.+', htt))
pjm <- merge(pjm, pjm_internal1(ht,"RECO"), by="Hour_Starting")

return(pjm)
}
