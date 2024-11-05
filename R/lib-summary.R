#' Provide number of R packages by Library
#'
#' @param sizes Should sizes of libraries be calculated. Default `FALSE` .
#'
#' @return  a data.frame of the number of packages in Program Files and Users library
#' @export
#'
#' @examples
#' lib_summary()
lib_summary <- function(sizes = FALSE) {
   if (!is.logical(sizes)){
      stop("sizes must be logical (TRUE or FALSE")
   }

   pkgs <- utils::installed.packages()
   pkg_tbl <- table(pkgs[, "LibPath"])
   pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)
   names(pkg_df) <- c("Library", "n_packages")

   if (isTRUE(sizes)){
      pkg_df$lib_size <- vapply(
         pkg_df$Library,
         function(x) {
            sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
         },
         FUN.VALUE = numeric(1)
      )
   }
   pkg_df
}
