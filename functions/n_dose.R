# ------------------------------------------------------------------------------
# file        : functions/n_dose.R
# author      : Xavier Laviron
# object      : Function to compute nitrogen dose from fertilizer dose
# ------------------------------------------------------------------------------

n_dose <- function(x, ref) {

  # x must be a 4 columns dataframe with the fertilizer name in col 2,  the dose
  # in col 3 and the dose unit in col 4. Col 1 is the unique identifier
  colnames(x) <- c("id_parc_tri", "product", "dose", "unit")

  # empty vector to store the dose
  n_ha <- as.numeric(rep(0, nrow(x)))

  for (i in 1:nrow(x)) {
    if (is.na(x$product[i]) || is.na(x$dose[i]) || is.na(x$unit[i])) {
      # If the product, the dose or the dose unit are not mentioned, we skip the
      # line because we cannot compute anything. If the dataframe is clean, this
      # should happen only on lines that do not mention fertilization
      next
    }

    if (x$unit[i] == "Unité/HA") {
      # 1 U/HA = 1 KgN/HA, so the dose need not to be computed
      n_ha[i] <- x$dose[i]
    }

    if (x$unit[i] == "L/HA") {
      # here we assume 1 L = 1 Kg, I don't know if this is right
      x$unit[i] <- "Kg/HA"
    }

    if (x$unit[i] == "Kg/HA") {
      if (x$product[i] %in% ref$Fertilisant) {
        # Check if the product is in the reference table
        # The reference gives the % of nitrogen
        percentN <- ref$doseN[ref$product == x$product[i]]

        if (length(percentN) > 1) {
          # there are more than one product with that name (eg 'fumierX'): we
          # take the mean dose (this can be changed):
          percentN <- mean(percentN)
        }
        #
        n_ha[i]  <- x$dose[i] * percentN / 100
      }
    }
  }

  # We take the sum of the dose for each 'id_parc_tri' so we have the full
  # nitrogen dose for the season
  result <- data.frame(character(), numeric())

  for (this_id in unique(x$id_parc_tri)) {
    if (all(n_ha[x$id_parc_tri == this_id] == 0)) {
      # if no nitrogen:
      this_dose <- 0
    } else {
      this_dose <- sum(n_ha[x$id_parc_tri == this_id], na.rm = TRUE)
    }

    result <-
      rbind.data.frame(result,
                       cbind.data.frame(this_id, this_dose))
  }

  colnames(result) <- c("id_parc_tri", "n_ha")
  result$id_parc_tri <- as.character(result$id_parc_tri)

  return(result)
}
