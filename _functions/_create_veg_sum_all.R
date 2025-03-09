create_veg_sum_all <- function(vdat, siteUnits, minconstancy = 60, noiseconstancy = 10, strata.by = "Lifeform", minimportance = 0) {
  if (strata.by == "Layer") {
    vdat <- lump_species2(vdat, lump = lump)
  } else if (strata.by == "Lifeform") {
    vdat <- vdat <- lump_species(vdat, lump = lump)
  }
  setDT(vdat)
  vdat <- merge(vdat, siteUnits, by = "PlotNumber")
  vdat <- vdat[PlotNumber %in% siteUnits$PlotNumber, ]
  # vdat <- vdat %>% filter(bgc %in% BGC)
 # vdat <- vdat[assocs %in% Assoc, ]
  ## remove trees in moss layer
  #vdat <-  vdat  %>% filter(!Species %in% tree_seedlings)
  #vdat <-  vdat  %>% filter(!(Species %in% trees & Layer == "Moss"))
  
  vdat <- vdat[, if (.N > 1) .SD, by = .(SiteUnit, Species)]
  vdat[, nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  if (strata.by == "Layer") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots),
      importance = (sum(Cover, na.rm = TRUE) / unique(nplots))^(1/2) * (.N / unique(nplots))
    ), by = .(SiteUnit, Species, Layer)]
  } else if (strata.by == "Lifeform") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots),
      importance = (sum(Cover, na.rm = TRUE) / unique(nplots))^(1/2) * (.N / unique(nplots))
    ), by = .(SiteUnit, Species, Lifeform)]
  }
  
  vdat[, maxcons := max(Constancy), by = .(Species)]
  vdat[, maximportance := max(importance), by = .(Species)]
  vdat <- vdat[maxcons > minconstancy, ]
  vdat <- vdat[Constancy > noiseconstancy, ]
  vdat <- vdat[importance > minimportance, ]
}
