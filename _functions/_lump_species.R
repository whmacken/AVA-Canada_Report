###Optional application of lump species
# lumpfile = lump
# vegdata = veg.dat
lump_species <- function(vegdata, lumpfile, use.subtaxa = FALSE){
setDT(vegdata)[setDT(lumpfile), "Species" := LumpCode, on = c("Species" = "SppCode")]
 if (isFALSE(use.subtaxa)){
   vegdata$Species <-   gsub('[0-9]+', '', vegdata$Species)
 }
  vegdata2 <- vegdata[, sum(Cover), by=list(PlotNumber,Species, Lifeform)]
vegdata <- vegdata2 %>% dplyr::rename(Cover = V1) %>% 
  dplyr::select(PlotNumber,Species, Cover, Lifeform) %>% 
  group_by(PlotNumber,Species, Lifeform) %>% summarise(Cover = sum(Cover)) %>% ungroup()
  
return(vegdata)
}
