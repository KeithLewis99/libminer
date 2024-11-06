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

   pkg_df <- lib()
   pkg_tbl <- table(pkg_df[, "LibPath"])
   pkg_df <- as.data.frame(pkg_tbl, stringsAsFactors = FALSE)

   names(pkg_df) <- c("Library", "n_packages")

   if (isTRUE(sizes)){
      pkg_df <- calculate_sizes(pkg_df)
   }
   pkg_df
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
lib <- function(){
   pkgs <- utils::installed.packages()
   as.data.frame(pkgs, stringsAsFactors = FALSE)
}

#' Title
#'
#' @param df a dataframe
#'
#' @return a df with a lib_size column
#' @export
#'
#' @examples
#' @noRd
calculate_sizes <- function(df){
   df$lib_size <- map_dbl(
      df$Library,
      \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
   )
   df
}
