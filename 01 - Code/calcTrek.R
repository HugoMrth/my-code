calcTrek <- function(KM, D, J = NULL, GR = NULL) {
  if(!is.null(GR)) {
    switch (GR,
            "GR10" = {
              KM = 922
              D = 55000
            },
            "TMB" = {
              KM = 180
              D = 10000
            }
    )
  }
  
  if (is.null(J)) {
    jmin <- ceiling(round(KM + D * 0.013333, 0)/65)
    jmax <- floor(round(KM + D * 0.013333, 0)/30)
    J <- jmin:jmax
  }
  
  data <- data.frame(
    KMtot = KM,
    Dtot = D,
    NJ = J
  ) %>%
    mutate(
      KMEFFtot = round(KM + Dtot * 0.013333, 0),
      KMjour = round(KMtot / NJ, 1), 
      Djour = round(Dtot / NJ, 0),
      KMEFFjour = round(KMEFFtot / NJ, 0)
    )%>%
    dplyr::select(NJ, KMjour, Djour, KMEFFjour)
  
  colnames(data) <- c("Nombre de jours", "Km", "D+", "Km/Effort")
  
  list(
    Trek = paste0(
      "Ton trek fait ", KM, "km pour ", D, "m de d�nivel� positif, soit ", 
      round(KM + D * 0.013333, 0), "km/effort. Par jours, �a fait :"
    ),
    StatParJour = data 
  )
}
