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
    ),
    bf = c(
      aglio = "MX.60914",
      aglurt = "MX.60916",
      antcar = "MX.60767",
      aphhyp = "MX.60970",
      apocra = "MX.60772",
      aralev = "MX.60908",
      argpap = "MX.60876",
      ariart = "MX.60859",
      boleup = "MX.60889",
      bolsel = "MX.60890",
      breino = "MX.60884",
      calrub = "MX.60811",
      carsil = "MX.60745",
      celarg = "MX.60830",
      coegly = "MX.60965",
      coepam = "MX.60968",
      cyasem = "MX.60865",
      erelig = "MX.60979",
      eumeum = "MX.60863",
      eupmat = "MX.60931",
      fabadi = "MX.60878",
      gonrha = "MX.60794",
      lasmae = "MX.60957",
      laspet = "MX.60958",
      lepsin = "MX.60763",
      limpop = "MX.60944",
      lychip = "MX.60821",
      lycphl = "MX.60816",
      lycvir = "MX.60819",
      melath = "MX.60939",
      nymant = "MX.60920",
      ochsyl = "MX.60755",
      papmac = "MX.60730",
      paraeg = "MX.60953",
      pienap = "MX.60778",
      plearg = "MX.60854",
      pleida = "MX.60855",
      polcal = "MX.60925",
      polama = "MX.60866",
      polica = "MX.60867",
      pyrmal = "MX.60735",
      satpru = "MX.60809",
      speagl = "MX.60877",
      thebet = "MX.60799",
      thylin = "MX.60749"
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
