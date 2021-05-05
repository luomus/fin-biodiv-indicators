#' Species list
#'
#' Get species lists for indices
#'
#' @param index Which index to get species list from?
#' @param which Use MX "mx" codes or species codes "spcode".
#'
#' @export

species <- function(index = "winter_birds", which = c("mx", "spcode")) {

  which <- match.arg(which)

  species <- list(
    winter_birds = c(
      POEMON = "MX.34535",
      REGREG = "MX.33954",
      LOPCRI = "MX.34553",
      BONBON = "MX.26931",
      TETURO = "MX.26928",
      TETRIX = "MX.26926",
      CERFAM = "MX.34616",
      DRYMAR = "MX.30504",
      LOXCUR = "MX.36358",
      GLAPAS = "MX.29011",
      PINENU = "MX.36351",
      PICTRI = "MX.30453",
      LOXPYT = "MX.36356",
      SURULU = "MX.29008",
      POECIN = "MX.34542",
      PERINF = "MX.37095"
    )
  )

  ans <- species[[index]]

  switch(
    which,
    mx = ans,
    spcode = names(ans)
  )

}
