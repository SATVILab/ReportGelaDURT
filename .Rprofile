if (interactive()) {
  alwaysloaded::run_std()
  library(grDevices)

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
  age_to_shape <- c(
    "infant" = "triangle",
    "adult" = "diamond",
    "Infant" = "triangle",
    "Adult" = "diamond"
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



  col_vec_bcg_infant <- RColorBrewer::brewer.pal(
    9, "Blues")[c(7, 9)]
  col_vec_bcg_infant <- c(col_vec_bcg_infant[1],
                          "navyblue")
  col_vec_bcg_infant <- RColorBrewer::brewer.pal(
    9, "Blues")[c(4, 8)]
  col_vec_bcg_infant <- c(
    "cadetblue2",
    col_vec_bcg_infant[2]
  )

  # col_vec_bcg_infant <- c("#00ffff",
  #                        "#0000ff")
  # col_vec_bcg_infant <- c("#1E",
  #                         "#082642")
  # col_vec_bcg_infant <- c("dodgerblue", "dodgerblue4")
  bcg_to_col_infant <- setNames(
    col_vec_bcg_infant,
    c("no bcg", "bcg")
  )

  bcg_to_col_infant <- c(
    bcg_to_col_infant,
    setNames(
      bcg_to_col_infant,
      c("No BCG", "BCG")
    ),
    setNames(
      bcg_to_col_infant,
      c("infant_ctrl", "infant_trt")
    )
  )

  color_vec_adults <- c(
    "orange2",
    RColorBrewer::brewer.pal(
      9,
      "Reds"
    )[c(7:9)])

  grp_to_col_adult <- setNames(
    color_vec_adults,
    paste0("day_", c(0, 21, 35, 365))
  )

  trt_to_col <- c(
    bcg_to_col_infant,
    grp_to_col_adult,
    setNames(
      grp_to_col_adult[c(1, 2)],
      c("before", "after")
    ),
    setNames(
      grp_to_col_adult[c(1, 2)],
      c("Before", "After")
    ),
    setNames(
      grp_to_col_adult[c(1, 2)],
      c("adult_ctrl", "adult_trt")
    ),
    "cd4_ifng" = "red",
    "IFNg+CD4+" = "yellowgreen"
  )

  short_to_display_age <- c("infant" = "Infant", "adult" = "Adult",
                            "Infant" = "Infant", "Adult" = "Adult")
  short_to_display_trt <- c(
    "bcg" = "BCG",
    "ifng" =  bquote(paste(plain(paste("IFN")), gamma, plain(paste("+"))))
    )

  var_exp_to_shape <- c(
    "bcg" = "triangle filled",
    "day_365" = "circle filled",
    "adult_bcg" = "square filled"
  )

  var_exp_to_fill <- c(
    "bcg" = "#7570b3",
    "day_365" = "#1b9e77",
    "adult_bcg" = "#d95f02"
  )

  # figure directories
  dir_fig_vec <- NULL
  dir_fig_manu_n <- here::here(
    "_book",
    "figures",
    "manuscript"
  )
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

  r_file_vec <- list.files(here::here("R"),
                           full.names = TRUE)
  for(i in seq_along(r_file_vec)) source(r_file_vec[i])

  conf_vec_infant <- c(
    "none" = "",
    "pre_specified" = "+ sex + race"#,
    # "data_specified" = " + ethnicity + length + gest_age + weight"
  )

}

