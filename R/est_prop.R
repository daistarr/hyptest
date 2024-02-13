#' @title Inference: Proportion
#'
#' @description Response variable: categorical. \cr \cr
#'
#' Here you are able to obtain the sample proportion and the confidence interval that captures the true population proportion. \cr \cr
#' This function relies in a randomization-based estimation. Firstly, it constructs a bootstrap distribution in order to approximate the standard error of statistic of interest (sample proportion). Then, it computes a confidence interval for the population parameter (population proportion).
#'
#' @param df Dataframe.
#' @param x Quantitative variable of interest found in the dataframe.
#' @param cat_prop Category of x from which you want to obtain the proportion from. \cr It is necessary to add the quotation-marks to the targeted category. For example: "1" or "cat".
#' @param cil Confidence Interval Levels. \cr They are floats from 0 to 1; where the default value is 0.95, corresponding to 95% confidence interval.
#' @param bin_num Number of bins to use in the histogram. \cr The default number of bins is 30.
#' @param save_as Exports the generated histogram as pdf, jpeg, or png. \cr The default setting is `NULL`, which does not export the graph. It is necessary to add quotation-marks to the desired format for exporting the graph: "pdf", "jpeg" or "png".
#' @section Figures:
#' \if{html}{This histogram was created by the est_prop function and data from the \code{\link{palmerpenguins}}. It calculates the proportion of Adelie penguins in the Palmer Archipelago.
#'
#'   \out{<div style="text-align: center">}\figure{prop.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'
#' }
#' @examples
#' library(hyptest)
#' library(palmerpenguins)
#'
#' est_prop(penguins, species, "Adelie", cil= 0.99, save_as = "png")
#'
#' "[1] The sample mean is 0.44"
#' "[1] The true population mean is between 0.37 and 0.51 at 99% confidence interval"
#' @export
est_prop <- function(df, x, cat_prop, cil = 0.95, bin_num = 30, save_as = NULL){

  if(!is.numeric(cil)){
    stop('Please select a confidence interval range between 0 and 1.\n',
         'For example, confidence intervals at 95% will have cil = 0.95')}

  #remove NAs & filter dataset
  df <- na.omit(df)

  df <- dplyr::mutate(df, prop_bin = dplyr::case_when(
    {{x}} == cat_prop ~ cat_prop,
    {{x}} != cat_prop ~ "No"
  ))

  #create the bootstrap distribution
  bootstrap_dist <- df |>
    infer::specify(response = prop_bin, success = cat_prop) |>
    infer::generate(reps = 1000, type = "bootstrap") |>
    infer::calculate(stat = "prop")

  #compute point estimate: proportion
  phat <- df |>
    infer::specify(response = prop_bin, success = cat_prop) |>
    infer::calculate(stat = "prop", na.rm = TRUE)

  #rounding the point estimate
  phat_value <- phat |> dplyr::pull() |> round(digits = 2)

  #compute confidence interval at 95%
  ci <- bootstrap_dist |>
    infer::get_confidence_interval(type = "se", level = cil, point_estimate = phat_value)

  #creating the graph
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

    geomtextpath::geom_textvline(label = paste0("Sample proportion"), xintercept = phat_value, vjust = 1.4) +
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
         subtitle = paste0("Sample proportion: ", phat_value, "\n", cil*100, "% Confidence Interval: ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits = 2)),
         x = expression(paste("Statistic: Proportion (", hat(p), ")")),
         y = "Density")


  # save the graph
  if(!is.null(save_as)){
    suppressWarnings(ggplot2::ggsave(graph, file = paste0("prop.", save_as), width=8, height=4))
  }

  #print information and return the graph
    print(paste("The sample proportion is", phat_value))
    print(paste0("The true population proportion is between ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits =2), " at ", cil*100, "% confidence interval"))
    #print(graph)
    suppressWarnings(ggplot2:::print.ggplot(graph))
}
