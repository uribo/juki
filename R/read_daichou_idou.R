#' @noRd
pull_vars <- function(..., ncol = NULL) {
  if (is.null(ncol)) {
    ncol <-
      readxl::read_xlsx(...,
                        n_max = 1) |>
      ncol()
  }
  readxl::read_xlsx(...,
                    col_names = paste0("x", stringr::str_pad(seq.int(ncol),
                                                             width = 2,
                                                             pad = "0"))) |>
    purrr::flatten_chr()
}

#' @noRd
make_vars <- function(path, ncols, sep = "_") {
  vars_2a <-
    pull_vars(path, skip = 4, n_max = 1, ncol = ncols)
  vars_2b <-
    pull_vars(path, skip = 5, n_max = 1, ncol = ncols)
  vars_1 <-
    pull_vars(path,
              skip = 6,
              n_max = 1,
              ncol = ncols+7)
  vars_2 <-
    paste(
      vars_2a,
      vars_2b,
      sep = sep)
  vars_1_mod_loc <-
    vars_1 |>
    stringr::str_which(paste0("^", "\u4eba", "$"))
  vars_2 <-
    paste(
      vars_2,
      vars_1[vars_1_mod_loc],
      sep = sep
    )
  vars_1[vars_1_mod_loc] <- vars_2

  vars_1
}

#' Read and tidy internal migration data for a specified prefecture in Japan.
#'
#' @param path Path to the Excel file containing the population migration data.
#' @param pref_code a character string specifying the code for the prefecture of
#' interest. This code should be a two-digit string (e.g. "01" for Hokkaido)
#' @param .tidy option. A logical flag indicating whether the output should
#' be in a tidy format or not.
#' @examples
#' \dontrun{
#' read_idou_pref("b01036s.xlsx", pref_code = "36")
#' }
#' @rdname read_idou_pref
#' @export
read_idou_pref <- function(path, pref_code, .tidy = FALSE) {
  pref_code <-
    stringr::str_pad(pref_code, width = 2, pad = "0")
  rlang::arg_match(pref_code,
                   stringr::str_pad(seq.int(47),
                                    width = 2,
                                    pad = "0"))
  d <-
    daichou_idou_meta |>
    dplyr::filter(pref_code == {{ pref_code }})
  if (nrow(d) > 0) {
    df_raw <-
      readxl::read_xlsx(path,
                        sheet = 1,
                        skip = 7,
                        col_names = make_vars(path = path, ncols = d$ncol))
    df_mod <-
      df_raw |>
      purrr::set_names(names(df_raw) |>
                         stringr::str_remove("_\u4eba$"))
    if (.tidy == TRUE) {
      df_mod <-
        df_mod |>
        tidy_idou_pref()
    }
    df_mod
  }
}

#' @noRd
tidy_idou_pref <- function(df) {
  value <- NULL
  df |>
    tidyr::pivot_longer(cols = seq.int(8, ncol(df)),
                        names_to = c("\u79fb\u52d5\u5f8c\u306e\u4f4f\u6240\u5730",
                                     "\u9805\u76ee"),
                        names_pattern = "(.*)_(.*)") |>
    tidyr::pivot_wider(names_from = "\u9805\u76ee",
                       values_from = value)
}
