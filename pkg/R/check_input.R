#' Check input
#'
#' Check indicator inputs
#'
#' @param index Character. Update which index?
#' @param model Character. Which model to use?
#' @param taxon Character. Which taxon?
#'
#' @importFrom config get
#' @export

check_input <- function(index, model, taxon) {

  has_index <- index %in% vapply(config::get("indices"), getElement, "", "code")

  models <- names(config::get("model", index))

  has_model <- model %in% c(models, "default")

  taxa <- vapply(config::get("taxa", index), getElement, "", "code")

  etaxa <- vapply(config::get("extra_taxa", index), getElement, "", "code")

  has_taxon <- taxon %in% c(taxa, etaxa, "none")

  has_index && has_model && has_taxon

}
