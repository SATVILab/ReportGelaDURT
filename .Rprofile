if (interactive()) {
  alwaysloaded::run_std()

  # loaded libraries
  suppressWarnings(suppressMessages(invisible(library(quantreg))))

  # clinical data
  data_tidy_clin_infant <- DataTidyGelaDURT::data_tidy_clin_infant
  data_tidy_clin_adult <- DataTidyGelaDURT::data_tidy_clin_adult

  # labels
  short_to_long_pop <- c(
    "cd1b" = "CD1b",
    "cd4" = "CD4",
    "cd4_ifng" = "CD4 IFNg",
    "gd" = "TCRgd",
    "gem" = "GEM",
    "mait" = "MAIT",
    "nkt" = "NKT"
  )
  short_to_long_var_cont <- c(
    "gest_age" = "Gest. age",
    "head_circ" = "Head circ.",
    "length" = "Length",
    "visit_age" = "Visit age",
    "weight" = "Weight",
    "age_in_years" = "Age (years)",
    "hgbval" = "HGB",
    "height" = "Height (cm)",
    "weight" = "Weight (kg)",
    "ppd" = "PPD (mm)"
  )
  short_to_display_bcg <- c(
    "bcg" = "BCG",
    "no bcg" = "No BCG",
    "nobcg" = "No BCG",
    "before" = "Before",
    "after" = "After"
  )
  short_to_display_sex <- c(
    "female" = "Female",
    "male" = "Male",
    "unknown" = "Unknown"
  )

  bcg_to_col <- c(
    "no bcg" = "red",
    "bcg" = "dodgerblue"
  )
  trt_to_col <- c(
    "No BCG" = "red",
    "BCG" = "dodgerblue",
    "Before" = "red",
    "After" = "dodgerblue",
    "cd4_ifng" = "dodgerblue",
    "IFNg+CD4+" = "#fdae61"
  )
  short_to_display_age <- c("infant" = "Infant", "adult" = "Adult",
                            "Infant" = "Infant", "Adult" = "Adult")
  short_to_display_trt <- c(
    "bcg" = "BCG",
    "ifng" =  bquote(paste(plain(paste("IFN")), gamma, plain(paste("+"))))
    )

  # figure directories
  dir_fig_vec <- NULL
  dir_fig_manu_n <- here::here(
    "_book",
    "figures",
    "non_manuscript"
  )
  dir_fig_vec <- c(
    dir_fig_vec,
    c("manu_n" = dir_fig_manu_n)
  )
  dir_fig_manu_n_exp <- file.path(
    dir_fig_manu_n,
    "data_exp"
  )
  dir_fig_vec <- c(
    dir_fig_vec,
    c("manu_n_exp" = dir_fig_manu_n_exp)
  )
  dir_fig_manu <- here::here(
    "_book",
    "figures",
    "manuscript"
  )
  dir_fig_vec <- c(
    dir_fig_vec,
    c("manu" = dir_fig_manu)
  )
  dir_fig_vec_rel <- dir_fig_vec %>%
    stringr::str_remove(paste0(here::here(), "/")) %>%
    setNames(names(dir_fig_vec))
  purrr::walk(dir_fig_vec, function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

  r_file_vec <- list.files(file.path(here::here(), "R"),
                           full.names = TRUE)
  for(i in seq_along(r_file_vec)) source(r_file_vec[i])

}
