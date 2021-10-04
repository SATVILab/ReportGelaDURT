get_fig_6_sub_plots <- function() {
  chr_to_num_x_order <- setNames(
    seq_len(length(x_order_vec)),
    x_order_vec
  )
  num_to_chr_x_order <- function(x) {
    purrr::map_chr(x, function(elem) {
      setNames(
        names(chr_to_num_x_order),
        chr_to_num_x_order
      )[as.character(elem)]
    })
  }
  pop <- pop_vec[1]
  purrr::map(pop_vec, function(pop) {


    # ====================
    # Add raw data
    # ====================

    data_raw_pop <- data_tidy_fig6 %>%
      dplyr::filter(pop == .env$pop) %>%
      dplyr::select(pid, age, grp, pop, pop_sub,
                    x_order, x_order_num, freq)

    rt <- 0.25
    rs <- 1/rt
    trans_root <- scales::trans_new(
      transform = function(x) {
        purrr::map_dbl(x, function(elem) {
          if(is.na(elem)) return(elem)
          if(elem <= 0) return(elem)
          if(elem <= 50) return(elem^rt)
          if(elem <= 100) {
            return(2 * 50^rt - (abs(elem - 100))^rt)
          }
          2 * 50^rt + 1 * (elem - 100)
        })

      },
      inverse = function(x) {
        purrr::map_dbl(x, function(elem) {
          if(is.na(elem)) return(elem)
          if(elem <= 0) return(elem)
          if(elem <= 50^rt) return(elem^rs)
          if(elem <= 2 * 50^rt) {
            return(100 - (2 * 50^rt - elem)^rs)
          }
          100 + (elem - 2 * 50^rt) / 1
        })
      },
      name = "root"
    )

    test_tbl <- tibble::tibble(
      x = seq(0, 110, by= 10)
    ) %>%
      dplyr::mutate(
        trans = trans_root$transform(x)
      ) %>%
      dplyr::mutate(
        back_trans = trans_root$inverse(trans)
      )

    break_vec <- seq(
      trans_root$transform(0),
      trans_root$transform(100),
      length.out = 5
    ) %>%
      trans_root$inverse()

    break_vec <- c(0, 1, 10, 50, 90, 99, 100)

    break_lab_vec <- as.character(break_vec)


    p <- ggplot(mapping = aes(x = x_order_num)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "y") +
      scale_y_continuous(
        trans = trans_root,
        breaks = break_vec,
        labels = break_lab_vec
      ) +
      theme(
        axis.ticks.x = element_blank()
      ) +
      scale_x_continuous(
        expand = expansion(mult = 0, add = c(0.1, 0))
      )

    for (x_order in x_order_vec) {
      p <- p +
        ggforce::geom_sina(
          data = data_raw_pop %>%
            dplyr::filter(x_order == .env$x_order),
          aes(x = x_order_num, y = freq, col = grp),
          alpha = 0.5,
          maxwidth = 0.75,
          scale = "width",
          size= 1
        )
    }

    vline_tbl <- tibble::tibble(
      x = rep(4.5 + c(0, 4, 8), each = 2),
      y = rep(c(0, 100), 3),
      grp = rep(letters[1:3], each = 2)
    )
    p <- p +
      geom_line(
        data = vline_tbl,
        aes(x = x, y = y, grp = grp),
        linetype = "dotted",
        inherit.aes = FALSE,
        col = "gray50"
      )
    color_vec <- RColorBrewer::brewer.pal(
      8,
      "Greens"
    )[4:7]
    pop_sub_to_col <- setNames(
      color_vec,
      pop_sub_vec
    )
    p_raw <- p +
      scale_colour_manual(
        values = c(
          "no bcg" = "#a6cee3",
          "bcg" = "#1f78b4",
          "day_0" = "#b2df8a",
          "day_365" = "#33a02c"
        )
      ) +
      guides("colour" = "none") +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      scale_shape_manual(
        values = c(
          "infant" = "triangle",
          "adult" = "circle"
        )
      ) +
      expand_limits(y = c(0, 100)) +
      geom_hline(yintercept = c(0, 100), col = "gray85")

    # ====================
    # Add estimates and CIs
    # ====================

    results_tbl_est_ci_pop <- results_tbl_fig6_est_ci %>%
      dplyr::filter(pop == .env$pop) %>%
      dplyr::left_join(
        pop_sub_grp_to_x_order_inc_num,
        by = c("pop_sub", "grp")
      )

    p_est <- p_raw +
      geom_errorbar(
        data = results_tbl_est_ci_pop,
        aes(ymin = lb, ymax = ub),
        width = 0.5
      ) +
      geom_point(
        data = results_tbl_est_ci_pop,
        aes(y = est),
        size = 0.75
      )

    # ====================
    # Add p-values
    # ====================

    # if(pop != "") browser()

    pop_sub <- pop_sub_vec[1]
    p_tbl <- purrr::map_df(pop_sub_vec, function(pop_sub) {
      data_raw_pop_pop_sub <- data_raw_pop %>%
        dplyr::filter(pop_sub == .env$pop_sub)
      range_axis_orig <- c(0, max(data_raw_pop_pop_sub$freq, na.rm = TRUE))
      range_axis_orig_trans <- trans_root$transform(range_axis_orig)

      length_axis_orig_trans <- diff(range_axis_orig_trans)
      y_pos_bcg_trans <- range_axis_orig_trans[2] + 0.05 * length_axis_orig_trans
      y_pos_age_trans <- range_axis_orig_trans[2] + 0.275 * length_axis_orig_trans
      var_exp_to_y_pos_trans <- c("day_365" = y_pos_bcg_trans,
                                  "bcg" = y_pos_bcg_trans,
                                  "adult_bcg" = y_pos_age_trans)

      results_tbl_fig6_diff %>%
        dplyr::filter(pop == .env$pop) %>%
        dplyr::filter(pop_sub == .env$pop_sub) %>%
        dplyr::select(age, pop_sub, var_exp,
                      diff_p, diff_p_bonf) %>%
        dplyr::filter(!var_exp == "adult_no_bcg") %>%
        dplyr::mutate(
          xmin = purrr::map_dbl(var_exp, function(x) {
            switch(
              x,
              "bcg" = 0,
              "day_365" = 2,
              "adult_bcg" = 1
            )
          }),
          xmax = purrr::map_dbl(var_exp, function(x) {
            switch(
              x,
              "bcg" = 1,
              "day_365" = 3,
              "adult_bcg" = 3
            )
          })
        ) %>%
        dplyr::mutate(
          xmin = xmin + purrr::map_dbl(pop_sub, function(x) {
            switch(
              x,
              "CD45RA+CCR7+" = 1,
              "CD45RA-CCR7+" = 5,
              "CD45RA-CCR7-" = 9,
              "CD45RA+CCR7-" = 13
            )
          }),
          xmax = xmax + purrr::map_dbl(pop_sub, function(x) {
            switch(
              x,
              "CD45RA+CCR7+" = 1,
              "CD45RA-CCR7+" = 5,
              "CD45RA-CCR7-" = 9,
              "CD45RA+CCR7-" = 13
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
          y = var_exp_to_y_pos_trans[var_exp]
        )
    }) %>%
      dplyr::mutate(
        row = seq_len(dplyr::n())
      )

    tip_len <- 0.03
    range_upper_bound <- 101.5 # was 102
    p_test <- p_est +
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
      expand_limits(y = range_upper_bound)


    # fine-tune
    p_grp <- p_test +
      labs(y = pop_to_lab_mem[[pop]]) +
      guides("shape" = "none")

    # ======================
    # Standardised effect size
    # ======================

    #if(pop != "") browser()

    diff_tbl_pop <- results_tbl_fig6_diff %>%
      dplyr::filter(pop == .env$pop) %>%
      dplyr::filter(!var_exp == "adult_no_bcg") %>%
      dplyr::mutate(
        pop_sub = factor(
          pop_sub,
          levels = pop_sub_vec
        ),
        var_exp = factor(
          var_exp,
          levels = c(
            "bcg", "day_365", "adult_bcg"
          )
        )
      ) %>%
      dplyr::select(-c(diff_p, diff_p_bonf, sd_std, age)) %>%
      dplyr::arrange(
        pop_sub, var_exp
      ) %>%
      dplyr::group_by(
        pop_sub
      ) %>%
      dplyr::mutate(
        x_order_num = seq(0, 4.5, length.out = 5)[2:4]
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        x_order_num = x_order_num + purrr::map_dbl(pop_sub, function(x) {
          switch(
            x,
            "CD45RA+CCR7+" = 0,
            "CD45RA-CCR7+" = 4.5,
            "CD45RA-CCR7-" = 9,
            "CD45RA+CCR7-" = 13
          )
        })
      ) %>%
      dplyr::mutate(
        x_order = paste0(pop_sub, "_", var_exp)
      )

    lab_y <- pop_to_lab_mem_diff[[1]]

    break_outer <- max(
      abs(ceiling(diff_tbl_pop$diff_ub_std)),
      abs(floor(diff_tbl_pop$diff_lb_std)),
      3
    )
    break_vec <- seq(-break_outer, break_outer)

    p_diff <- ggplot(data = diff_tbl_pop,
                     mapping = aes(x = x_order_num,
                                   shape = var_exp,
                                   fill = var_exp)) +
      cowplot::theme_cowplot() +
      cowplot::background_grid(major = "y") +
      geom_line(
        data = vline_tbl %>%
          dplyr::mutate(y = rep(c(break_vec[1], break_vec[length(break_vec)]), 3)),
        aes(x = x, y = y, grp = grp),
        linetype = "dotted",
        inherit.aes = FALSE,
        col = "gray50"
      ) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_errorbar(
        aes(ymin = diff_lb_std, ymax = diff_ub_std)
      ) +
      geom_point(
        aes(y = diff_est_std), size = 1.8
      ) +
      scale_y_continuous(
        breaks = break_vec
      ) +
      expand_limits(y= c(min(break_vec), max(break_vec))) +
      labs(x = "Days", y = lab_y) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank()) +
      scale_shape_manual(
        values = c(
          "bcg" = "triangle filled",
          "day_365" = "circle filled",
          "adult_bcg" = "square filled"
        )
      ) +
      scale_fill_manual(
        values = c(
          "bcg" = "#7570b3",
          "day_365" = "#1b9e77",
          "adult_bcg" = "#d95f02"
        )
      ) +
      guides("shape" = "none", "fill" = "none")

    # ======================
    # Marker expression
    # ======================

    plot_tbl_marker <- pop_sub_grp_to_x_order_and_num %>%
      dplyr::mutate(
        CCR7 = ifelse(grepl("CCR7\\+", pop_sub), "+", "-"),
        CD45RA = ifelse(grepl("CD45RA\\+", pop_sub), "+", "-"),
      ) %>%
      tidyr::pivot_longer(
        c(CCR7, CD45RA),
        names_to = "marker",
        values_to = "lvl"
      )
    p_marker <- ggplot(plot_tbl_marker,
           aes(x = x_order_num, y = marker, fill = lvl), alpha = 0.8) +
      cowplot::theme_cowplot() +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_blank()) +
      geom_tile() +
      scale_fill_manual(
        values = c("-" = "white",
                   "+" = "gold3")
      ) +
      guides("fill" = "none") +
      theme(axis.title = element_blank()) +
      theme(
        axis.text.y = element_text(size = 10)
      ) +
      scale_x_continuous(
        expand = expansion(mult = 0, add = c(0.1, 0))
      )

    list("p_grp" = p_grp, "p_diff" = p_diff, "p_marker" = p_marker)
  })
}
