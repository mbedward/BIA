## Prepare lookup table of BIA attributes from data provided by the Rural Fire Service

x <- read.csv("data-raw/BIA_LU.csv")

# Note: first column is just a sequence of integer values.

bia_lookup <- lapply(2:ncol(x), function(i) {
  nm <- colnames(x)[i]
  xvals <- x[,i]
  ok <- !is.na(xvals)

  if (any(ok)) {
    data.frame(attribute = nm, value = which(ok), label = xvals[ok])
  }
})

bia_lookup <- do.call(rbind, bia_lookup)

usethis::use_data(bia_lookup, overwrite = TRUE)

rm(bia_lookup, x)
