#' Update AzureR packages
#'
#' This will check to see if all AzureR packages (and optionally, their
#' dependencies) are up-to-date, and will install after an interactive
#' confirmation.
#'
#' @inheritParams AzureR_deps
#' @export
#' @examples
#' \dontrun{
#' AzureR_update()
#' }
AzureR_update <- function(recursive = FALSE, repos = getOption("repos")) {

  deps <- AzureR_deps(recursive, repos)
  behind <- deps[deps$behind==TRUE,]

  if (nrow(behind) == 0) {
    cat("All AzureR packages up-to-date")
    return(invisible())
  }

  cat("The following packages are out of date:")
  cat("\n")
  cat(behind[c("Package", "Installed", "ReposVer")])

  cat("\n")
  cat("Start a clean R session then run:")

  pkg_str <- paste0(deparse(behind$Package), collapse = "\n")
  cat("install.packages(", pkg_str, ")")

  invisible()
}

#' List all AzureR dependencies
#'
#' @param recursive If \code{TRUE}, will also list all dependencies of
#'   tidyverse packages.
#' @param repos The repositories to use to check for updates.
#'   Defaults to \code{getOptions("repos")}.
#' @export
AzureR_deps <- function(recursive = FALSE, repos = getOption("repos")) {
  pkgs <- utils::available.packages(repos = repos)
  deps <- tools::package_dependencies("AzureR", pkgs, recursive = recursive)

  pkg_deps <- unique(sort(unlist(deps)))

  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  df <- as.data.frame(utils::old.packages(pkg_deps)[, c("Package", "Installed", "ReposVer")], row.names = FALSE)

  df$behind <- base::package_version(df$ReposVer) > base::package_version(df$Installed)

  df
}
