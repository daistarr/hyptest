#' @title Inference: Difference in proportions
#'
#' @description Response variable: categorical. \cr
#' Explanatory variable: binary, categorical \cr \cr
#' Here you are able to obtain the difference in two sample proportions and the confidence interval that captures the true difference between two proportions within the population. \cr \cr
#' This function relies in a randomization-based estimation. Firstly, it constructs a bootstrap distribution in order to approximate the standard error of statistic of interest (difference in two proportion means). Then, it computes a confidence interval for the population parameter (difference in two population proportions).
#'
#' @param df Dataframe.
#' @param catprop Categorical variable of interest, representing the predictor variable, found in the dataframe that contains the categories of interest. \cr It is given by a column containing only categories.
#' @param prop1 First category of interest found within `catprop`. \cr It is necessary to add quotation-marks to call the desired category.
#' @param prop2 Second category of interest found within `catprop`. \cr It is necessary to add quotation-marks to call the desired category.
#' @param catarget Binary categorical variable of interest, representing the response variable, found in the dataframe that contains the categories of interest. \cr It is given by a column containing only categories.
#' @param target Category of interest found within `catarget`. \cr It is necessary to add quotation-marks to call the desired category.
#' @param cil Confidence Interval Levels. \cr They are floats from 0 to 1; where the default value is 0.95, corresponding to 95% confidence interval.
#' @param bin_num Number of bins to use in the histogram. \cr The default number of bins is 30.
#' @param save_as Exports the generated histogram as pdf, jpeg, or png. \cr The default setting is `NULL`, which does not export the graph. It is necessary to add quotation-marks to the desired format for exporting the graph: "pdf", "jpeg" or "png".
#'
#' @section Figures:
#' \if{html}{This histogram was created by the est_diff_props function and data from the \code{\link{palmerpenguins}}. It calculates the difference in proportions of the Adelie penguins in the Torgersen and Dream islands.
#'
#'   \out{<div style="text-align: center">}\figure{diffprop.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'
#' }
#' @examples
#' library(hyptest)
#' library(palmerpenguins)
#'
#' est_diff_props(penguins, island, "Torgersen", "Dream", species, "Adelie", cil= 0.99, save_as = "png")
#'
#' [1] The difference in sample proportions is 0.55
#' [1] The true difference in population proportions is between 0.44 and 0.67 at 99% confidence interval
#' @export
est_diff_props <- function(df, catprop, prop1, prop2, catarget, target,  cil = 0.95, bin_num = 30, save_as = NULL){

  if(!is.numeric(cil)){
    stop('Please select a confidence interval range between 0 and 1.\n',
         'For example, confidence intervals at 95% will have cil = 0.95')}

  suppressMessages({
    suppressWarnings({

      #remove NAs
      df <- na.omit(df) |>
        dplyr::filter({{catprop}} %in% c(prop1, prop2)) |>
        dplyr::mutate(prop_bin = dplyr::case_when(
          {{catarget}} == target ~ target,
          {{catarget}} != target ~ "No")) |>
        dplyr::mutate(prop1 = {{prop1}}, prop2 = {{prop2}}, catprop = {{catprop}})


      #creating point estimate: difference in proportions
      diffprop <- df |>
        infer::specify(prop_bin ~ catprop, success = target) |>
        infer::calculate(stat = "diff in props", order = c(prop1, prop2), na.rm = TRUE)

      #rounding the point estimate
      diffprop_value <- diffprop |>
        dplyr::pull() |>
        round(digits = 2)

      #create the bootstrap distribution
      bootstrap_dist <- df |>
        infer::specify(prop_bin ~ catprop, success = target) |>
        infer::generate(reps = 1000, type = "bootstrap") |>
        infer::calculate(stat = "diff in props", order = c(prop1, prop2), na.rm = TRUE)

      #compute confidence interval at 95%
      ci <- bootstrap_dist |>
        infer::get_confidence_interval(type = "se", level = cil, point_estimate = diffprop)

    })
  })

  graph <- ggplot2::ggplot(bootstrap_dist, ggplot2::aes(x = stat)) +

    # creating the histogram
    ggplot2::geom_histogram(ggplot2::aes(x = stat, fill = ggplot2::after_stat(x)), bins = bin_num) +

    # setting the color of the histogram
    ggplot2::scale_fill_gradientn(colours = c(viridis::viridis(5), rev(viridis::viridis(5))[2:5]))+

    #creating the confidence intervals
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = ci$lower_ci, ymin = -Inf, ymax = Inf),
                       fill = "gray80", alpha = 0.006, color="grey80", linewidth = 0.1) +
    ggplot2::geom_rect(ggplot2::aes(xmin = ci$upper_ci, xmax = Inf, ymin = -Inf, ymax = Inf),
                       fill = "gray80", alpha = 0.006, color="grey80", linewidth = 0.1) +

    geomtextpath::geom_textvline(label = paste0("Difference in proportions"), xintercept = diffprop_value, vjust = 1.4) +
    geomtextpath::geom_textvline(label = paste0("Lower CI"), xintercept = ci$lower_ci, vjust = -0.7) +
    geomtextpath::geom_textvline(label = paste0("Upper CI"), xintercept = ci$upper_ci, vjust = 1.4) +

    #setting the theme of the graph
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = NA),
      panel.border = ggplot2::element_rect(fill = NA, color = "grey75"),
      axis.ticks = ggplot2::element_line(color = "grey85"),
      panel.grid.major = ggplot2::element_line(color = "grey95", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_line(color = "grey95", linewidth = 0.2),
      legend.key = ggplot2::element_blank()) +

    #defining the labels of axis and lines
    ggplot2::labs(caption = "Simulation-Based Bootstraps",
                  subtitle = paste0("Difference in proportions between ", prop1, " and ", prop2, ": ", diffprop_value, "\n", cil*100, "% Confidence Interval between ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits = 2)),
                  x = expression(paste("Statistic: Difference in Proportions (", hat(p)[2]-hat(p)[1], ")")), y = "Density")

  # save the graph
  if(!is.null(save_as)){
    suppressWarnings(ggplot2::ggsave(graph, file = paste0("diffprop.", save_as), width=8, height=4))
  }

  #print information and return the graph
  print(paste("The difference in sample proportions is", diffprop_value))
  print(paste0("The true difference in population proportions is between: ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits =2), " at ", cil*100, "% confidence interval"))
  #print(graph)
  suppressWarnings(ggplot2:::print.ggplot(graph))
}

