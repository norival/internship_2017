get_trait_value <- function(sp, ref, trait) {
  # little function to extract traits values from a reference table
  # When a species is not present in the reference, it tries to look for species
  # with the same genus and take their average values

  # check if species is present
  present <- sp %in% ref$sp
  
  # present
  if (present) {
    trait.val <- ref[ref$sp == sp, trait]
  } else {
    # not present
    ## look for genus
    genus <- strsplit(sp, " ")[[1]][1]
    ref.genus <- as.character(sapply(ref$sp, function(x) strsplit(x, " ")[[1]][1]))
    trait.val <- mean(ref[grepl(genus, ref.genus), trait], na.rm = TRUE)

    # if no species from the same genus are present, the mean returns NaN, just
    # set this to NA
    if (is.nan(trait.val)) trait.val <- NA
  }

  return(trait.val)
}
