lump_species2 <- function(vegdata, lumpfile, use.subtaxa = FALSE){
  setDT(vegdata)[setDT(lumpfile), "Species" := LumpCode, on = c("Species" = "SppCode")]
  if (isFALSE(use.subtaxa)){
    vegdata$Species <-   gsub('[0-9]+', '', vegdata$Species)
  }
  vegdata <- vegdata[, sum(Cover), by=list(PlotNumber,Species, Layer)]
  vegdata <- vegdata %>% dplyr::rename(Cover = V1) %>% dplyr::select(PlotNumber,Species, Cover, Layer)
  
  return(vegdata)
}
