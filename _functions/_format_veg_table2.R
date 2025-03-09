##formats a vegetation summary table

format_veg_table2 <- function(vsum = vegSum, spp = species){
  #create new variable Species2 in vegSum from Species and Layer
  vsum <- vsum %>% mutate(Layer = as.character(Lifeform))
  vsum$Layer <-  case_match(vsum$Layer, "1" ~ "Tree",
                             "2" ~ "Tree",
                             "3" ~ "Shrub",
                             "4" ~ "Shrub",
                             "5" ~ "Herb",
                             "6" ~ "Herb" ,
                             "7" ~ "Herb",
                             "8" ~ "Herb",
                             "9" ~ "Moss",
                            "10" ~ "Moss",
                            "11" ~ "Moss",
                            "12" ~ "Herb")
  vsum$Species2 <- paste0(vsum$Species, "_", vsum$Layer)
  vsum[ , code := encode_veg_sum(MeanCov, Constancy), by = .(Species2, SiteUnit)]
  vsum <- vsum %>% merge(spp, by.x = 'Species', by.y = 'Code') 
  #merge(spp, by.x = 'Species', by.y = 'Code') |>
  #mutate(SiteUnit = str_replace(SiteUnit, "101", "109"))
  nPlots <- unique(vsum[ ,.(SiteUnit, nplots)])[order(nplots, decreasing = TRUE), ] %>% 
    arrange(SiteUnit)
  vsum <- data.table::dcast(vsum, 
                            Layer + ScientificName + EnglishName ~ SiteUnit, 
                            value.var = 'code',
                            fill = '')[order(Layer, ScientificName), ]
  #vsum[duplicated(ReportName, fromLast = TRUE), ReportName := '']
  vsum[ , c('Layer','ScientificName', nPlots$SiteUnit, 'EnglishName'), with = FALSE]
  data.table::setnames(vsum, old = c('Layer', 'ScientificName', 
                                     'EnglishName'), new = c('Layer', 'Scientific name', 'Common name'))
  vsum <- vsum %>% 
    mutate_all(str_replace_all,"-remove", "")
  # vsum$Layer <-  case_match(vsum$Layer, "1" ~ "A",
  #                            "2" ~ "A",
  #                            "3" ~ "B",
  #                            "4" ~ "B",
  #                            "5" ~ "C",
  #                            "6" ~ "C" ,
  #                            "7" ~ "C",
  #                            "8" ~ "C",
  #                            "9" ~ "D",
  #                           "10" ~ "D",
  #                           "11" ~ "D",
  #                           "12" ~ "C")
  vsum <- vsum[ order(match(vsum$Scientific, indic.order$ScientificName)), ]
  
  vsum2 <-   vsum %>% select(order(colnames(vsum))) %>%  
   select(Layer, `Scientific name`, everything()) %>% 
    relocate(`Common name`, .after = last_col()) %>%
    arrange(match(Layer, c("Tree", "Regen", "Shrub", "Herb", "Moss")), Layer)
  
  #colnames(vsum2) <- gsub(paste0(bgc.choose,"?"), "", colnames(vsum2))
  nPlotRow <- c('', 'n Plots', nPlots$nplots, '') |> 
    matrix(nrow = 1) |>
    data.frame() |> 
    stats::setNames(names(vsum2)) |> 
    data.table::as.data.table()
  vsum3 <- rbind(nPlotRow, vsum2)
}
