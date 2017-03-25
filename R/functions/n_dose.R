# ------------------------------------------------------------------------------
# file        : functions/n_dose.R
# author      : Xavier Laviron
# object      : Function to compute nitrogen dose from fertilizer dose
# ------------------------------------------------------------------------------

n_dose <- function(x, ref) {
  # x <- data_full[, c("Produit_Ferti", "Dose_Ferti", "Unité_dose")]

  # x must be a 3 columns dataframe with the fertilizer name in col 1,  the dose
  # in col 2 and the dose unit in col 3
  colnames(x) <- c("id_parc_tri", "product", "dose", "unit")

  ref$Fertilisant <- toupper(ref$Fertilisant)
  x$product       <- toupper(x$product)

  n_ha <- as.numeric(rep(NA, nrow(x)))

  for (i in 1:nrow(x)) {
    if (is.na(x$product[i]) || is.na(x$dose[i]) || is.na(x$unit[i])) {
      # If the product, the dose or the dose unit are not mentioned, we skip the
      # line
      next
    }

    if (x$unit[i] == "Unité/HA") {
      # 1 U/HA = 1 Kg/HA, so the dose need not to be computed
      n_ha[i] <- x$dose[i]
    }

    else if (x[i, "unit"] == "Kg/HA") {
      if (x$product[i] %in% ref$Fertilisant) {
        # Check if the product is in the reference table
        # mass is the nitrogen mass for 100 Kg of product, found in the
        # reference table
        mass <- ref$kg_n_100kg[ref$Fertilisant == x$product[i]]
        n_ha[i] <- x$dose[i] * mass / 100
      }
    }
  }

  # sum the doses for each plot
  result <- data.frame(character(), numeric())
  for (parc in unique(x$id_parc_tri)) {
    if (all(is.na(n_ha[x$id_parc_tri == parc])))
      dose <- NA
    else
      dose <- sum(n_ha[x$id_parc_tri == parc], na.rm = TRUE)

    result <-
      rbind.data.frame(result,
                       cbind.data.frame(parc, dose))
  }

  colnames(result) <- c("id_parc_tri", "n_ha")
  result$id_parc_tri <- as.character(result$id_parc_tri)

  return(result)
}
