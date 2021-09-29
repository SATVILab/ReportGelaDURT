
#' @param dep numeric.
#' Values plotted for the dependent variable.
#' Used to calculate axis limits and appropriate
#' breaks.
#' @param trans object of class "trans". Transformation
#' for dependent variable.
dep <- plot_tbl_raw_tidy$hladr
add_p_value <- function(dep,
                        lvl,
                        var_dep,
                        prop_p,
                        buffer_tick,
                        tick_height,
                        buffer_data,
                        margin_top = 0,
                        trans) {

  # get axis limits
  range_axis_orig <- range(dep)
  range_axis_orig_trans <- trans_y$transform(range_axis_orig)
  range_axis_extend_trans <- range_axis_orig_trans
  range_axis_extend_trans[2] <- range_axis_orig_trans[1] %>%
    magrittr::add(diff(range(range_axis_orig_trans)) / (1 - prop_p))
  range_axis_extend <- trans_y$inverse(range_axis_extend_trans)

  # get top of p-value line
  p_pos_trans <- range_axis_extend_trans[2] * (1 - margin_top)
  p_pos <- p_pos_trans %>%
    trans_y$inverse()
}




shift_q <- 0.1
q_pos_trans <- p_pos_trans - (shift_q * length_axis_trans)
q_pos <- q_pos_trans %>%
  trans_y$inverse()


# proportion of axis that is there for p-values
prop_p <- ifelse(pop == "cd4", 0.2, 0.3)
prop_prop_p_buffer_data <- 1/10 # proportion of axis limit extension for buffer from data



# get axis limits for plotting p-values
range_axis_p_trans <- c(range_axis_orig_trans[2],
                        range_axis_extend_trans[2])
range_axis_p <- range_axis_p_trans %>%
  trans_y$inverse()

# lengths of range
length_axis_trans <- diff(range_axis_extend_trans)
length_axis <- diff(range_axis_extend)

# length of segments along y-axis for plotting p-values
length_axis_p_trans <- diff(range_axis_p_trans)
length_axis_p <- diff(range_axis_p)

# tick: bottom point
prop_buffer_data <- 0.02
tick_bottom_trans <- range_axis_p_trans[1] + (prop_buffer_data * length_axis_trans)
tick_bottom <- tick_bottom_trans %>%
  trans_y$inverse()

# tick: top point
tick_length <- 0.05
tick_top_trans <- tick_bottom_trans + (tick_length * length_axis_trans)
tick_top <- tick_top_trans %>%
  trans_y$inverse()
tick_vec <- c(tick_bottom, tick_top)

# q: y
prop_buffer_tick <- 0.05
q_pos_trans <- tick_top_trans + (prop_buffer_tick * length_axis_trans)
q_pos <- q_pos_trans %>%
  trans_y$inverse()

# p: y
prop_shift_p <- (1 - (q_pos_trans - range_axis_extend_trans[1]) / length_axis_trans) / 2
#print(prop_shift_p)
#prop_shift_p <- 0.15
# prop_shift_p <-
p_pos_trans <- q_pos_trans + (prop_shift_p * length_axis_trans)
# p_pos_trans <- max(range_axis_extend_trans[2], p_pos_trans)
#p_pos_trans <- range_axis_extend_trans[2]
p_pos <- p_pos_trans %>%
  trans_y$inverse()
