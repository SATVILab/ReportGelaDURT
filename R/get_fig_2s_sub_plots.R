get_fig_2s_sub_plots <- function() {
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

    data_raw_pop <- data_tidy_fig2s %>%
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

    trans_root <- scales::identity_trans()

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
    break_vec <- seq(0, 100, by = 20)
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
          alpha = 0.65,
          maxwidth = 0.75,
          scale = "width"
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
    color_vec <- c(
      RColorBrewer::brewer.pal(
        9,
        "Reds"
        )[c(5, 7:9)])

    grp_to_col <- setNames(
      color_vec,
      data_raw_pop$grp %>% unique() %>% as.character()
    )

    p_raw <- p +
      scale_colour_manual(
        values = trt_to_col
      ) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank()) +
      guides("colour" = "none", "shape" = "none") +
      theme(legend.position = "none") +
      expand_limits(y = c(0, 100)) +
      geom_hline(yintercept = c(0, 100), col = "gray85")

    # ====================
    # Add estimates and CIs
    # ====================

    results_tbl_est_ci_pop <- results_tbl_fig2s_est_ci %>%
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
      ) +
      geom_line(
        data = results_tbl_est_ci_pop,
        mapping = aes(x = x_order_num,
                      y = est,
                      group = paste0(pop, pop_sub))
      )

    # ====================
    # Add p-values
    # ====================

    # if(pop != "") browser()

    pop_sub <- pop_sub_vec[1]
    p_tbl <- purrr::map_df(pop_sub_vec, function(pop_sub) {
      data_raw_pop_pop_sub <- data_raw_pop %>%
        dplyr::filter(pop_sub == .env$pop_sub)
      #range_axis_orig <- c(0, max(data_raw_pop_pop_sub$freq, na.rm = TRUE))
      range_axis_orig <- c(0, 100)
      range_axis_orig_trans <- trans_root$transform(range_axis_orig)

      length_axis_orig_trans <- diff(range_axis_orig_trans)
      y_pos_trans <- range_axis_orig_trans[2] + 0.05 * length_axis_orig_trans

      results_tbl_fig2s_diff %>%
        dplyr::filter(pop == .env$pop) %>%
        dplyr::filter(pop_sub == .env$pop_sub) %>%
        dplyr::select(pop_sub, diff_p, diff_p_bonf) %>%
        dplyr::mutate(
          xmin = 0,
          xmax = 3
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
          y = y_pos_trans
        )
    }) %>%
      dplyr::mutate(
        row = seq_len(dplyr::n())
      )

    vline_tbl_ticks <- tibble::tibble(
      x = rep(c(2, 3), each = 2),
      y = rep(c(100.5, p_tbl$y[1]), 2),
      grp_tick = rep(c("a", "b"), each = 2)
    )
    vline_tbl_ticks <- purrr::map_df(0:3, function(x) {
      vline_tbl_ticks %>%
        dplyr::mutate(x = x + .env$x * 4) %>%
        dplyr::mutate(grp_tick = paste0(grp_tick, "_", .env$x))
    })

    tip_len <- 0.03
    range_upper_bound <- 114 # was 101.5 # was 102
    p_test <- p_est +
      ggsignif::geom_signif(
        data = p_tbl,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = p_txt,
                      group = row),
        manual = TRUE, vjust = -1.54,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      ggsignif::geom_signif(
        data = p_tbl,
        mapping = aes(xmin = xmin, xmax = xmax,
                      y_position  = y, annotations = q_txt,
                      group = row),
        manual = TRUE, vjust = -0.2,
        textsize = 2.75,
        tip_length = tip_len
      ) +
      expand_limits(y = range_upper_bound) +
      geom_line(
        data = vline_tbl_ticks,
        aes(x = x, y = y, group = grp_tick)
      )



    # fine-tune
    p_grp <- p_test +
      labs(y = pop_to_lab_mem[[pop]]) +
      guides("shape" = "none")
    # ======================
    # Marker expression
    # ======================

    plot_tbl_marker <- pop_sub_grp_to_x_order_inc_num %>%
      dplyr::mutate(
        CCR7 = ifelse(grepl("CCR7\\+", pop_sub), "+", "-"),
        CD45RA = ifelse(grepl("CD45RA\\+", pop_sub), "+", "-"),
      ) %>%
      tidyr::pivot_longer(
        c(CCR7, CD45RA),
        names_to = "marker",
        values_to = "lvl"
      )
    plot_tbl_marker <- plot_tbl_marker %>%
      dplyr::group_by(pop_sub, marker, lvl) %>%
      dplyr::summarise(x_order_num = mean(x_order_num),
                       .groups = "drop")

    p_marker <- ggplot(plot_tbl_marker,
                       aes(x = x_order_num, y = marker, fill = lvl)) +
      cowplot::theme_cowplot() +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.line.y = element_blank()) +
      geom_tile() +
      geom_text(aes(label = lvl)) +
      scale_fill_manual(
        values = c("-" = "white",
                   "+" = "white") # cornsilk1/2 looks all right
      ) +
      scale_colour_manual(
        values = c("-" = "white",
                   "+" = "white") # was gold3
      ) +
      guides("fill" = "none") +
      theme(axis.title = element_blank()) +
      theme(
        axis.text.y = element_text(size = 10)
      ) +
      scale_x_continuous(
        expand = expansion(mult = 0, add = c(0.1, 0))
      )

    list("p_grp" = p_grp, "p_marker" = p_marker)
  })
}
