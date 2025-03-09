create_veg_sum_assoc <- function(vdat, siteUnits, minconstancy = 60, noiseconstancy = 10, strata.by = "Lifeform", Assoc) {
  if (strata.by == "Layer") {
    vdat <- lump_species2(vdat, lump = lump)
  } else if (strata.by == "Lifeform") {
    vdat <- vdat <- lump_species(vdat, lump = lump)
  }
  setDT(vdat)
  vdat <- merge(vdat, siteUnits, by = "PlotNumber")
  vdat <- vdat[PlotNumber %in% siteUnits$PlotNumber, ]
  # vdat <- vdat %>% filter(bgc %in% BGC)
  vdat <- vdat[assocs %in% Assoc, ]
  
  vdat <- vdat[, if (.N > 1) .SD, by = .(SiteUnit, Species)]
  vdat[, nplots := length(unique(PlotNumber)), by = .(SiteUnit)]
  if (strata.by == "Layer") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots)
    ), by = .(SiteUnit, Species, Layer)]
  } else if (strata.by == "Lifeform") {
    vdat <- vdat[, .(
      MeanCov = sum(Cover, na.rm = TRUE) / unique(nplots), # should this just be mean, is NA assumed to be 0?
      Constancy = (.N / unique(nplots)) * 100,
      nplots = unique(nplots)
    ), by = .(SiteUnit, Species, Lifeform)]
  }
  vdat[, maxcons := max(Constancy), by = .(Species)]
  vdat <- vdat[maxcons > minconstancy, ]
  vdat <- vdat[Constancy > noiseconstancy, ]
}
