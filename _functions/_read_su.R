## combines all _SU tables in a database into a single SU table
return_su <- function(db, SU){
  correlation <- dbConnect(odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; DBQ=", db,";"))
  SU <- paste0(SU, "_SU")
  su <- dbReadTable(correlation, SU)
  dbDisconnect(correlation)
  su <- su %>% mutate(SiteUnit = gsub("/", "_", su$SiteUnit)) 
  su <- su %>% mutate(SiteUnit = gsub(" ", "", su$SiteUnit, fixed = TRUE)) 
  return(su)
}
