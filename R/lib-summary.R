#' Provide number of R packages by Library
#'
#' @return  a data.frame of the number of packages in Program Files and Users library
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function() {
   pkgs <- utils::installed.packages()
   pkg_tbl <- table(pkgs[, "LibPath"])
   pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
   names(pkg_df) <- c("Library", "n_packages")
   pkg_df
}
