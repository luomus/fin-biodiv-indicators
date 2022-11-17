#' Get indices
#'
#' Get indices from a configuration file.
#'
#' @param file Configuration file.
#'
#' @importFrom  yaml read_yaml
#' @export

get_indices <- function(file = Sys.getenv("R_CONFIG_FILE")) {

  indices <- yaml::read_yaml(file)

  indices[["default"]] <- NULL

  lapply(
    names(indices),
    function(x) list(code = x, name = indices[[x]][["name"]])
  )

}
