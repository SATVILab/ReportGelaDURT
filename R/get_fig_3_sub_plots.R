get_fig_3_sub_plots <- function() {
  conf <- names(conf_vec_infant)[length(names(conf_vec_infant))]
  results_tbl_fig3_conf_none <- results_tbl_fig3_std %>%
    dplyr::filter(conf == "none") %>%
    dplyr::select(pop, diff_p, diff_est_std_resid_large:diff_ub_std_resid_large,
                  ctrl_grp_est:trt_grp_ub) %>%
    dplyr::rename(
      ctrl_est = ctrl_grp_est,
      trt_est = trt_grp_est,
      ctrl_lb = ctrl_grp_lb,
      ctrl_ub = ctrl_grp_ub,
      trt_lb = trt_grp_lb,
      trt_ub = trt_grp_ub,
      diff_std_est = diff_est_std_resid_large,
      diff_std_lb = diff_lb_std_resid_large,
      diff_std_ub = diff_ub_std_resid_large
    ) %>%
    dplyr::mutate(
      age = "infant"
    ) %>%
    dplyr::mutate(
      diff_p_bonf = p.adjust(diff_p, method = "bonf", n = length(pop_vec) + 1)
    ) %>%
    dplyr::mutate(
      var_exp = "bcg"
    ) %>%
    dplyr::select(
      pop, age, var_exp, diff_p, diff_p_bonf, everything()
    )
  results_tbl_fig3_conf <- results_tbl_fig3_std %>%
    dplyr::filter(conf == "pre_specified") %>%
    dplyr::select(pop, diff_p, diff_est_std_resid_large:diff_ub_std_resid_large,
                  ctrl_grp_est:trt_grp_ub) %>%
    dplyr::rename(
      ctrl_est = ctrl_grp_est,
      trt_est = trt_grp_est,
      ctrl_lb = ctrl_grp_lb,
      ctrl_ub = ctrl_grp_ub,
      trt_lb = trt_grp_lb,
      trt_ub = trt_grp_ub,
      diff_std_est = diff_est_std_resid_large,
      diff_std_lb = diff_lb_std_resid_large,
      diff_std_ub = diff_ub_std_resid_large
    ) %>%
    dplyr::mutate(
      age = "infant"
    ) %>%
    dplyr::mutate(
      diff_p_bonf = p.adjust(diff_p, method = "bonf", n = length(pop_vec) + 1)
    ) %>%
    dplyr::mutate(
      var_exp = "bcg"
    ) %>%
    dplyr::select(
      pop, age, var_exp, diff_p, diff_p_bonf, everything()
    )
  results_tbl_fig3_cyt_conf_none <- results_tbl_fig3_cyt %>%
    dplyr::filter(conf == "none") %>%
    dplyr::mutate(
      diff_p = diff_p_wsc,
      diff_est = diff_est_wsc,
      diff_lb = diff_lb_wsc,
      diff_ub = diff_ub_wsc
    ) %>%
    dplyr::select(pop, diff_p, diff_est_std_resid_large:diff_ub_std_resid_large,
                  ctrl_grp_est:trt_grp_ub) %>%
    dplyr::rename(
      ctrl_est = ctrl_grp_est,
      trt_est = trt_grp_est,
      ctrl_lb = ctrl_grp_lb,
      ctrl_ub = ctrl_grp_ub,
      trt_lb = trt_grp_lb,
      trt_ub = trt_grp_ub,
      diff_std_est = diff_est_std_resid_large,
      diff_std_lb = diff_lb_std_resid_large,
      diff_std_ub = diff_ub_std_resid_large
    ) %>%
    dplyr::mutate(
      age = "infant"
    ) %>%
    dplyr::mutate(
      diff_p_bonf = p.adjust(diff_p, method = "bonf", n = length(pop_vec) + 1)
    ) %>%
    dplyr::mutate(
      var_exp = "ifng"
    ) %>%
    dplyr::select(
      pop, age, var_exp, diff_p, diff_p_bonf, everything()
    )
  results_tbl_fig3_cyt_conf <- results_tbl_fig3_cyt %>%
    dplyr::filter(conf == "pre_specified") %>%
    dplyr::mutate(
      diff_p = diff_p_wsc,
      diff_est = diff_est_wsc,
      diff_lb = diff_lb_wsc,
      diff_ub = diff_ub_wsc
    ) %>%
    dplyr::select(pop, diff_p, diff_est_std_resid_large:diff_ub_std_resid_large,
                  ctrl_grp_est:trt_grp_ub) %>%
    dplyr::rename(
      ctrl_est = ctrl_grp_est,
      trt_est = trt_grp_est,
      ctrl_lb = ctrl_grp_lb,
      ctrl_ub = ctrl_grp_ub,
      trt_lb = trt_grp_lb,
      trt_ub = trt_grp_ub,
      diff_std_est = diff_est_std_resid_large,
      diff_std_lb = diff_lb_std_resid_large,
      diff_std_ub = diff_ub_std_resid_large
    ) %>%
    dplyr::mutate(
      age = "infant"
    ) %>%
    dplyr::mutate(
      diff_p_bonf = p.adjust(diff_p, method = "bonf", n = length(pop_vec) + 1)
    ) %>%
    dplyr::mutate(
      var_exp = "ifng"
    ) %>%
    dplyr::select(
      pop, age, var_exp, diff_p, diff_p_bonf, everything()
    )

  results_tbl_fig3_conf <- results_tbl_fig3_conf %>%
    dplyr::bind_rows(results_tbl_fig3_cyt_conf)
  results_tbl_fig3_conf_none <- results_tbl_fig3_conf_none %>%
    dplyr::bind_rows(results_tbl_fig3_cyt_conf_none)

  pop_vec_conf <- unique(results_tbl_fig3_conf_none$pop)
  pop <- pop_vec_conf[1]

  purrr::map(pop_vec_conf, function(pop) {

    # Group plot
    # =====================

    # prep est and ci
    # --------------------

    plot_tbl_results_raw <- results_tbl_fig3_conf_none %>%
      dplyr::filter(pop == .env$pop)
    plot_tbl_results_tidy <- plot_tbl_results_raw %>%
      dplyr::select(var_exp, age, ctrl_est:trt_ub) %>%
      tidyr::pivot_longer(
        -c(var_exp, age),
        names_to = "type",
        values_to = "value"
      ) %>%
      tidyr::separate(
        col = type,
        into = c("trt", "stat"),
        sep = "_"
      ) %>%
      dplyr::mutate(
        trt = paste0(var_exp, "_", trt)
      ) %>%
      dplyr::filter(
        trt != "ifng_ctrl"
      ) %>%
      dplyr::mutate(
        trt = purrr::map_chr(trt, function(x) {
          switch(
            x,
            "bcg_ctrl" = "No BCG",
            "bcg_trt" = "BCG",
            "ifng_trt" = "IFNg+CD4+",
            stop(paste0(x, " not recognised"))
          )
        }),
        trt = factor(
          trt,
          levels = switch(
            pop,
            "cd4" = c(
              "No BCG",
              "BCG",
              "IFNg+CD4+"
              ),
            c(
              "No BCG",
              "BCG"
            )
        )
      )
      )%>%
      tidyr::pivot_wider(
        names_from = stat,
        values_from = value
      ) %>%
      dplyr::mutate(
        ub = pmax(ub, est, lb),
        est = pmax(est, lb),
        lb = pmin(ub, est, lb),
        est = pmin(ub, est)
      )

    # prep raw data
    # --------------------

    plot_tbl_raw_tidy <- data_mod_hladr_infant %>%
        dplyr::filter(pop == .env$pop) %>%
        dplyr::select(bcg, hladr) %>%
        dplyr::mutate(
          bcg = ifelse(bcg %in% c("nobcg", "no bcg"), "No BCG", "BCG")
        )

    if (pop == "cd4") {
      plot_tbl_raw_tidy <- plot_tbl_raw_tidy %>%
        dplyr::bind_rows(
          data_mod_hladr_infant_cyt %>%
            dplyr::select(bcg, hladr) %>%
            dplyr::mutate(bcg = "IFNg+CD4+")
        )
    }

    plot_tbl_raw_tidy <- plot_tbl_raw_tidy %>%
      dplyr::rename(trt = bcg) %>%
      dplyr::mutate(
        trt = factor(
          trt,
          levels = switch(
            pop,
            "cd4" = c(
              "No BCG",
              "BCG",
              "IFNg+CD4+"
            ),
            c(
              "No BCG",
              "BCG"
            )
          )
        )
      )

    # transformation
    # -------------------

    trans_y <- switch(
      pop,
      "nkt" = scales::trans_new(
        "fourth_root",
        transform = function(x) x^0.5,
        inverse = function(x) x^(2),
      ),
      trans_asinh
    )

    inv_fn <- trans_y$inverse
    trans_fn <- trans_y$transform

    # create numeric x-axis variable
    # -------------------

    chr_to_num_trt <- c(
      "No BCG" = 0,
      "BCG" = 1,
      "IFNg+CD4+" = 2
    )
    num_to_chr_trt <- setNames(
      names(chr_to_num_trt),
      chr_to_num_trt
    )

    plot_tbl_results_tidy <- plot_tbl_results_tidy %>%
      dplyr::mutate(
        trt_num = chr_to_num_trt[trt]
      )

    plot_tbl_raw_tidy <- plot_tbl_raw_tidy %>%
      dplyr::mutate(
        trt_num = chr_to_num_trt[trt]
      )

    plot_tbl_results_tidy <- plot_tbl_results_tidy %>%
      dplyr::mutate(
        trt_num = chr_to_num_trt[trt]
      )

    # prepare p-values
    # ---------------------

    # actual p-values
    p_tbl <- results_tbl_fig3_conf %>%
      dplyr::filter(pop == .env$pop) %>%
      dplyr::select(pop:diff_p_bonf) %>%
      dplyr::select(pop, var_exp, age, diff_p, diff_p_bonf)


    plot_tbl_p <- p_tbl %>%
      dplyr::mutate(
        xmin = purrr::map_int(var_exp, function(x) {
          switch(
            x,
            "bcg" = 0L,
            "ifng" = 1L,
          )
        }),
        xmax = purrr::map_int(var_exp, function(x) {
          switch(
            x,
            "bcg" = 1L,
            "ifng" = 2L,
          )
        }),
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
        }),
        trt_num = 1
      )

    range_axis_orig <- range(plot_tbl_raw_tidy$hladr)
    range_axis_orig_trans <- trans_fn(range_axis_orig)

    length_axis_orig_trans <- diff(range_axis_orig_trans)
    frac_bump_bcg <- ifelse(pop == "cd4", -0.1, 0.1)
    y_pos_bcg_trans <- range_axis_orig_trans[2] + frac_bump_bcg * length_axis_orig_trans
    y_pos_cyt_trans <- range_axis_orig_trans[2] + 0.15 * length_axis_orig_trans
    var_exp_to_y_pos_trans <- c("bcg" = y_pos_bcg_trans, "ifng" = y_pos_cyt_trans)
    frac_extend <- ifelse(pop == "cd4", 0.3, 0.225)
    range_upper_bound_trans <- range_axis_orig_trans + frac_extend * length_axis_orig_trans
    range_upper_bound <- range_upper_bound_trans %>% inv_fn()

    plot_tbl_p <- plot_tbl_p %>%
      dplyr::mutate(y = var_exp_to_y_pos_trans[var_exp]) %>%
      dplyr::mutate(row = seq_len(dplyr::n()))

    plot_tbl_p_bcg <- plot_tbl_p %>%
      dplyr::filter(var_exp == "bcg")
    plot_tbl_p_cyt <- plot_tbl_p %>%
      dplyr::filter(var_exp == "ifng")

    vjust_p_cyt <- -1.8
    vjust_p_bcg <- -1.8
    vjust_q_cyt <- -0.4
    vjust_q_bcg <- -0.4

    tip_len <- switch(
      pop,
      "gem" = 0.0125,
      0.03
    )

    # plot
    # --------------------

    # add comparison bars and p-values
    p_grp <- ggplot(plot_tbl_results_tidy,
                    aes(x = trt_num)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "y") +
      ggsignif::geom_signif(
        data = plot_tbl_p_cyt,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = p_txt,
                      group = row),
        manual = TRUE, vjust = vjust_p_cyt,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      ggsignif::geom_signif(
        data = plot_tbl_p_bcg,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = p_txt,
                      group = row),
        manual = TRUE, vjust = vjust_p_bcg,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      ggsignif::geom_signif(
        data = plot_tbl_p_cyt,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = q_txt,
                      group = row),
        manual = TRUE, vjust = vjust_q_cyt,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      ggsignif::geom_signif(
        data = plot_tbl_p_bcg,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = q_txt,
                      group = row),
        manual = TRUE, vjust = vjust_q_bcg,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      scale_colour_manual(
        values = trt_to_col
      )


    # add raw data
    trt_lvl_vec <- unique(plot_tbl_raw_tidy$trt)
    for(trt_lvl in trt_lvl_vec) {
      p_grp <- p_grp +
        ggforce::geom_sina(
          data = plot_tbl_raw_tidy %>%
            dplyr::filter(trt == trt_lvl),
          aes(x = trt_num, y = hladr, col = trt),
          alpha = 0.65,
          maxwidth = ifelse(pop == "cd4", 1, 2/3)
        )
    }

    # add lines between ifng and bcg if pop is cd4
    # not working because pids aren't saved with
    # raw data
    if (pop == "cd4" && FALSE) {

      p_grp <- p_grp +
        geom_line(
          data = plot_tbl_raw_tidy %>%
            dplyr::filter(trt != "No BCG"),
          mapping = aes(x = trt_num, y = hladr, group = pid),
          col = "gray50"
        )

      # add lines joining bcg and cd4 ifn responses
    }

    # add confidence intervals and estimates
    p_grp <- p_grp +
      geom_errorbar(
        data = plot_tbl_results_tidy,
        aes(x = trt_num, ymin = lb, ymax = ub),
        alpha = 0.8,
        width = 0.45 * ifelse(pop == "cd4", 1, 0.55)
      ) +
      geom_point(
        data = plot_tbl_results_tidy,
        aes(x = trt_num, y = est),
        col = "black",
        alpha = 1, size = 2
      )

    # fine-tune appearnace
    num_to_chr_trt_test <- c(
      `0` = "No BCG",
      `1` = "BCG",
      `2` = bquote(paste(plain(paste("IFN")), gamma, "+)")))
    label_x <- function(brk) {
      purrr::map(brk, function(x) {
        switch(
          as.character(x),
          "0" = "No BCG",
          "1" = "BCG",
          "2"= ""
        )
      })
    }
    p_grp <- p_grp +
      scale_x_continuous(breaks = 0:2,
                         labels = label_x) + #num_to_chr_trt_test) +
      labs(y = pop_to_lab_hladr[[pop]]) +
      guides(colour = "none", shape = "none") +
      theme(axis.text.x = element_text(angle = 45,
                                       vjust = 0.675),
            axis.title.x = element_blank())

    # adjust breaks and push up upper limit
    p_grp <- switch(
      pop,
      "mait" = p_grp +
        scale_y_continuous(
          breaks = c(50, 175, 500, 2000),
          trans = trans_y
        ),
      "nkt" = p_grp +
        scale_y_continuous(
          breaks = c(0, 75, 275, 750),
          trans = trans_y
        ),
      "cd1b" = p_grp +
        scale_y_continuous(
          breaks = c(100, 500, 5000, 30000),
          trans = trans_y
        ),
      "gd" = p_grp +
        scale_y_continuous(
          breaks = c(125, 250, 500, 900, 1500),
          trans = trans_y
        ),
      "cd4" = p_grp +
        scale_y_continuous(
          breaks = c(0, 2, 10, 50, 200),
          trans = trans_y
        ),
      p_grp
    ) +
      expand_limits(y = range_upper_bound[2])

    # Standardised effect size plot
    # =====================


    plot_tbl_results_tidy <- plot_tbl_results_raw %>%
      dplyr::select(var_exp, diff_std_est:diff_std_ub) %>%
      tidyr::pivot_longer(
        -var_exp,
        names_to = "type",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(
        names_from = type,
        values_from = value
      )

    lab_y <- pop_to_lab_hladr_diff[pop][[1]]

    p_diff <- ggplot(data = plot_tbl_results_tidy,
                     mapping = aes(x = var_exp)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "y") +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_errorbar(
        aes(ymin = diff_std_lb, ymax = diff_std_ub),
        width = 0.75 * ifelse(pop == "cd4", 1, 0.5)
      ) +
      geom_point(
        aes(y = diff_std_est), size = 1.8
      ) +
      expand_limits(y= c(-3, 3)) +
      scale_y_continuous(
        breaks = seq(-3, 3)
      ) +
      scale_x_discrete(
        labels = short_to_display_trt,
        limits = switch(
          pop,
          "cd4" = c("bcg", "ifng"),
          c("bcg")
        )
      ) +
      labs(x = "Explanatory variable", y = lab_y) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.675))

    list("p_grp" = p_grp, "p_diff" = p_diff)


  }) %>%
    setNames(pop_vec_conf)
}
