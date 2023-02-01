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

read_idou_pref <- function(path, pref_code) {
  d <-
    daichou_idou_meta |>
    dplyr::filter(pref_code == {{ pref_code }})
  if (nrow(d) > 0) {
    df_raw <-
      readxl::read_xlsx(path,
                        sheet = 1,
                        skip = 7,
                        col_names = make_vars(path = path, ncols = d$ncol))
    df_raw |>
      purrr::set_names(names(df_raw) |>
                         stringr::str_remove("_\u4eba$"))
  }
}
