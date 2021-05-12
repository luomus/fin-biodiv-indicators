#' Species list
#'
#' Get species lists for indices
#'
#' @param index Which index to get species list from?
#' @param which Use MX "mx" codes or species codes "spcode".
#'
#' @export

species <- function(index, which = c("mx", "spcode")) {

  which <- match.arg(which)

  species <- list(
    wb = c(
      poemon = "MX.34535",
      regreg = "MX.33954",
      lopcri = "MX.34553",
      bonbon = "MX.26931",
      teturo = "MX.26928",
      tetrix = "MX.26926",
      cerfam = "MX.34616",
      drymar = "MX.30504",
      loxcur = "MX.36358",
      glapas = "MX.29011",
      pinenu = "MX.36351",
      pictri = "MX.30453",
      loxpyt = "MX.36356",
      surulu = "MX.29008",
      poecin = "MX.34542",
      perinf = "MX.37095"
    )
  )

  ans <- species[[index]]

  switch(
    which,
    mx = ans,
    spcode = names(ans)
  )

}

#' Check species
#'
#' Check if a species is available
#'
#' @param index Which index to check if species is available for.
#' @param sp Which species to check.
#'
#' @export

check_sp <- function(index, sp) {
  index <- check_index(index)
  sp <- tolower(sp)
  spp <- species(index, "spcode")
  stopifnot(sp %in% spp)
  sp
}
