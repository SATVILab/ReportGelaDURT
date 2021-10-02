get_fig_4_sub_plots <- function() {

  chr_to_num_days <- c(
    "0" = 0,
    "21" = 1,
    "35" = 2,
    "365" = 3
  )
  num_to_chr_days <- function(x) {
    purrr::map_chr(x, function(elem) {
      setNames(
        names(chr_to_num_days),
        chr_to_num_days
      )[as.character(elem)]
    })
  }

  num_to_chr_days_lab <- function(x) {
    purrr::map_chr(x, function(elem) {
      paste0(
        "Day ",
        setNames(
          names(chr_to_num_days),
          chr_to_num_days
          )[as.character(elem)]
      )
    })
  }
  purrr::map(pop_vec, function(pop) {
    results_tbl_pop <- results_tbl_fig4_std %>%
      dplyr::filter(pop == .env$pop)
    data_raw_pop <- data_mod_hladr_adult %>%
      dplyr::filter(pop == .env$pop)

    break_vec <- switch(
      pop,
      "mait" = c(0, 250, 500, 750, 1000),
      "nkt" = c(0, 250, 500, 750, 1000),
      "cd4" = (0:4) * 1e2,
      "cd1b" = c(500, 1000, 2000, 5000, 1e4),
      "gd" = c(0, 500, 1500, 3500, 6000)
    )

    p <- ggplot() +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "xy") +
      scale_y_continuous(
        trans = get_trans_obj(pop),
        breaks = break_vec
      )

    for (day_curr in unique(results_tbl_fig4_std$day)) {
      p <- p +
        ggforce::geom_sina(
          data = data_raw_pop %>%
            dplyr::filter(days == day_curr) %>%
            dplyr::mutate(days_num = chr_to_num_days[days]),
          aes(x = days_num, y = hladr, col = day_type),
          alpha = 0.5, shape = "circle",
          maxwidth = 0.75,
          scale = "width"
        )
    }
    p <- p +
      scale_colour_manual(
        values = c("pre_bcg" = "red",
                   "post_bcg" = "dodgerblue")
      ) +
      guides("colour" = "none")

    p <- p +
      geom_errorbar(
        data = results_tbl_pop %>%
          dplyr::filter(!day %in% c("any")) %>%
          dplyr::mutate(day_num = chr_to_num_days[day]),
        aes(x = day_num, ymin = lb, ymax = ub),
        width = 0.75
      ) +
      geom_point(
        data = results_tbl_pop %>%
          dplyr::filter(!day %in% c("any")) %>%
          dplyr::mutate(day_num = chr_to_num_days[day]),
        aes(x = day_num, y = est),
        shape = "circle"
      )

    range_axis_orig <- range(data_raw_pop$hladr, na.rm = TRUE)
    range_axis_orig_trans <- get_trans(pop)(range_axis_orig)

    length_axis_orig_trans <- diff(range_axis_orig_trans)
    y_pos_21_trans <- range_axis_orig_trans[2] + 0.05 * length_axis_orig_trans
    y_pos_all_trans <- range_axis_orig_trans[2] + 0.275 * length_axis_orig_trans
    var_exp_to_y_pos_trans <- c("21" = y_pos_21_trans, "any" = y_pos_all_trans)
    range_upper_bound_trans <- range_axis_orig_trans + 0.375 * length_axis_orig_trans
    range_upper_bound <- range_upper_bound_trans %>% get_trans_inv(pop)()





    p_tbl <- results_tbl_pop %>%
      dplyr::select(day, diff_p, diff_p_bonf) %>%
      dplyr::filter(day %in% c("21", "any")) %>%
      dplyr::mutate(
        xmin = 0,
        xmax = purrr::map_dbl(day, function(x) {
          switch(
            x,
            "21" = 1,
            "any" = 3
          )
          })
      ) %>%
      dplyr::mutate(
        p_txt = purrr::map_chr(diff_p, function(p) {
          paste0(
            "p ",
            ifelse(
              p < 0.0001,
              "< 0.0001",
              paste0(" = ", signif(p, 2)))
          )
        }),
        q_txt = purrr::map_chr(diff_p_bonf, function(p) {
          paste0(
            "(q ",
            ifelse(
              p < 0.0001,
              "< 0.0001",
              paste0(" = ", signif(p, 2))),

            ")"
          )
        })
      ) %>%
      dplyr::mutate(
        y = var_exp_to_y_pos_trans[day]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        row = seq_len(dplyr::n())
      )

    tip_len <- ifelse(pop == "cd1b", 0.03 * 2/3.5, 0.03)

    # if(pop == "cd1b" || pop == "gd") browser()

    extra_tick_top_trans <- p_tbl$y[p_tbl$day == "any"]
    extra_tick_bottom_trans <- extra_tick_top_trans - 0.0475 * length_axis_orig_trans
    extra_tick_top <- extra_tick_top_trans %>%
      get_trans_inv(pop)()
    extra_tick_bottom <- extra_tick_bottom_trans %>%
      get_trans_inv(pop)()

    tick_vec_extra <- c(
      extra_tick_top,
      extra_tick_bottom
    )
    extra_tick_tbl <- tibble::tibble(
      x = rep(c(1, 2), each = 2),
      y = rep(tick_vec_extra, 2),
      grp = rep(letters[1:2], each = 2)
    )

    p_test <- p +
      ggsignif::geom_signif(
        data = p_tbl,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = p_txt,
                      group = row),
        manual = TRUE, vjust = -0.2,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      ggsignif::geom_signif(
        data = p_tbl,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = q_txt,
                      group = row),
        manual = TRUE, vjust = -1.54,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      expand_limits(y = range_upper_bound) +
      geom_line(
        data = extra_tick_tbl,
        aes(x = x, y = y, grp = grp),
        inherit.aes = FALSE
    )

    # fine-tune

    p_grp <- p_test +
      scale_x_continuous(
        breaks = 0:3,
        labels = num_to_chr_days
      ) +
      labs(y = pop_to_lab_hladr_adult[[pop]],
           x = "Days")
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45,
                                       vjust = 0.675))

    # ======================
    # Standardised effect size
    # ======================

    plot_tbl_std <- results_tbl_pop %>%
      dplyr::filter(day == "21")

    lab_y <- pop_to_lab_hladr_diff[pop][[1]]

    p_diff <- ggplot(data = plot_tbl_std,
                     mapping = aes(x = day)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "y") +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_errorbar(
        aes(ymin = diff_lb_std, ymax = diff_ub_std)
      ) +
      geom_point(
        aes(y = diff_est_std), size = 1.8
      ) +
      expand_limits(y= c(-3, 3)) +
      scale_y_continuous(
        breaks = seq(-3, 3)
      ) +
      labs(x = "Days", y = lab_y)

    list("p_grp" = p_grp, "p_diff" = p_diff)
  })
}
