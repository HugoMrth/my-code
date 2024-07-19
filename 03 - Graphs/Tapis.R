#### TAPIS ####
dataTapis <- apply(data[, c("date_index", "date_fin_1st_TREF", "date_1st_reprise_Tref",
                                      "date_deb_1st_tswi_niv1", "date_deb_1st_tswi_niv2",
                                      "date_dc", "date_fin_suivi")], 2, 
                   function(x) {as.Date(x, format = "%Y-%m-%d")})
dataTapis <- apply(dataTapis, 2, function(x) {
  floor((x - dataTapis[, "date_index"]) / 7)
})
dataTapis[, "date_index"] <- 0

dataTapis <- dataTapis %>%
  as.data.frame() %>%
  group_by(date_index, date_fin_1st_TREF, date_1st_reprise_Tref, date_deb_1st_tswi_niv1, date_deb_1st_tswi_niv2, date_dc, date_fin_suivi) %>%
  summarise(n = n())


dataTapisSeq <- t(apply(dataTapis, 1, function(x) {
  #x <- dataTapis[1,]
  y <- x[8]
  x <- as.numeric(x)[-8]
  names(x) <- colnames(dataTapis)[-8]
  x <- diff(sort(x[!is.na(x)]))
  as.character(c(y,
    rep(c("date_index", names(x)[1:(length(x)-1)]), x),
    rep("date_dc", 52-sum(x))))
}))

dataTapisSeq[dataTapisSeq == "date_1st_reprise_Tref"] <- "TREF"
dataTapisSeq[dataTapisSeq == "date_dc"] <- "DC"
dataTapisSeq[dataTapisSeq == "date_deb_1st_tswi_niv1"] <- "TSWI"
dataTapisSeq[dataTapisSeq == "date_fin_1st_TREF"] <- "PSTT"
dataTapisSeq[dataTapisSeq == "date_deb_1st_tswi_niv2"] <- "TSWI"
dataTapisSeq[dataTapisSeq == "date_index"] <- "TREF"

rm(list = c("dataTapis"))

seq <- seqdef(data = dataTapisSeq[, 2:53],
              weights = as.numeric(dataTapisSeq[, 1]))
seqdplot(seq, border = NA,
         cpal = c("lightgray", "lightcoral" ,"cornflowerblue", "mediumseagreen"))
