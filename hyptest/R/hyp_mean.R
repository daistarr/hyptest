#' @title Hypothesis Testing: Mean
#'
#' @description Response variable: quantitative. \cr \cr
#' Here you are able to obtain the sample mean and the confidence interval that captures the true population mean. \cr \cr
#' This function relies in a randomization-based estimation. Firstly, it specify the variable of interest, followed by the construction of the null distribution followed by the computation of the sample statistic (sample mean).
#'
#' @param df Dataframe.
#' @param x Quantitative variable of interest found in the dataframe. \cr It is given by a column containing only numbers.
#' @param condition The direction of the alternative hypothesis. \cr \cr A two-sided alternative hypothesis is given by "different" and it consists of the extreme areas in both sides on the null distribution. It is useful to compare if the sample mean is different from a estimate about the true mean of the population. \cr \cr An one-sided alternative hypothesis is given by "less" or "greater" and it consists of the extreme area in one tail. To compare if the mean is smaller than a estimate about the true mean of the population, use "less". To compare if the mean is bigger than a estimate about the true mean of the population, use "greater".
#' @param nullhyp The estimate about the true mean of the population that will be used in the null hypothesis.
#' @param cil Confidence Interval Levels. \cr They are floats from 0 to 1; where the default value is 0.95, corresponding to 95% confidence interval.
#' @param bin_num Number of bins to use in the histogram. \cr The default number of bins is 30.
#' @param save_as Exports the generated histogram as pdf, jpeg, or png. \cr The default setting is `NULL`, which does not export the graph. It is necessary to add quotation-marks to the desired format for exporting the graph: "pdf", "jpeg" or "png".
#' @section Figures:
#' \if{html}{This histogram was created by the est_mean function and data from the \code{\link{palmerpenguins}}. It tests if the mean of bill lenght (bill_length_mm) of the sample is less than 44.5.
#'
#'   \out{<div style="text-align: center">}\figure{hypmean.png}{options: style="width:750px;max-width:75\%;"}\out{</div>}
#'
#' }
#' @examples
#' library(hyptest)
#' library(palmerpenguins)
#'
#' hyp_mean(data, bill_length_mm, "less", 44.5, cil = 0.95, bin_num = 30, save_as = "png")
#'
#' [1] "The sample mean is 43.99"
#' [1] "The true population mean is between 43.4 and 44.58 at 95% confidence interval"
#' [1] "The p-value is 0.045"
#' [1] "The alternative hypothesis, the one that you actually testing for, is that the mean of bill_length_mm is less than 44.5"
#' [1] "Possible Interpretation: The p-value is small and the sample data represents evidence in favor of the alternative hypothesis."
#' [1] "The sample mean is 43.99"
#' @export
hyp_mean <- function(df, x, condition, nullhyp, cil = 0.95, bin_num = 30, save_as = NULL){

  suppressMessages({
    suppressWarnings({

      if(!is.numeric(cil)){
        stop('Please select a confidence interval range between 0 and 1.\n',
             'For example, confidence intervals at 95% will have cil = 0.95')}

      label_condition <- condition

      if(condition == "different"){
        condition[condition == 'different'] <- 'two.sided'
        original_direction <- "different"
      }

      if(condition == "greater"){
        original_direction <- "greater"
      }

      if(condition == "less"){
        original_direction <- "less"
      }

      #remove NAs
      df <- na.omit(df)

      #creating test statistic: sample mean
      x_bar <- df |>
        infer::specify(response = {{x}}) |>
        infer::calculate(stat = "mean", na.rm = TRUE)

      #rounding the point estimate
      x_bar_value <- x_bar |>
        dplyr::pull() |>
        round(digits = 2)

      #create the null distribution
      null_dist <- df |>
        infer::specify(response = {{x}}) |>
        infer::hypothesize(null = "point", mu = nullhyp) |>
        infer::generate(reps = 1000, type = "bootstrap") |>
        infer::calculate(stat = "mean", na.rm = TRUE)

      #compute p-value
      p_val <- null_dist |>
        infer::get_p_value(obs_stat = x_bar, direction = condition)

      p_val_value <- p_val |>
        dplyr::pull()

      null_value <- null_dist |>
        dplyr::pull()

      #compute confidence interval at 95%
      ci <- null_dist |>
        infer::get_confidence_interval(type = "se", level = cil, point_estimate = x_bar)

    })
  })

  # Calculate the critical value based on the null distribution if original_direction == different
  critval <- quantile(null_value, c(p_val_value/2, 1 - p_val_value/2))

  #creating the graph
  graph <- ggplot2::ggplot(null_dist, ggplot2::aes(x = stat)) +

    # creating the histogram
    ggplot2::geom_histogram(ggplot2::aes(x = stat, fill = ggplot2::after_stat(x)), bins = bin_num) +

    # setting the color of the histogram
    ggplot2::scale_fill_gradientn(colours = c(viridis::viridis(5), rev(viridis::viridis(5))[2:5])) +

    # setting the p-values if the original_direction == "different", two tails
    {if(original_direction == "different")ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = critval[1], ymin = 0, ymax = Inf), fill = "lightgreen", alpha = 0.009)} +
    {if(original_direction == "different")geomtextpath::geom_textvline(label = paste0("p-value"), xintercept = critval[1], vjust = -0.8, linetype = 'dotted', color = "gray50")} +
    {if(original_direction == "different")ggplot2::geom_rect(ggplot2::aes(xmin = critval[2], xmax = Inf, ymin = 0, ymax = Inf), fill = "lightgreen", alpha = 0.009)} +
    {if(original_direction == "different")geomtextpath::geom_textvline(label = paste0("p-value"), xintercept = critval[2], vjust = 1.4, linetype = 'dotted', color = "gray50")} +

    # setting the p-values if the original_direction == "less"
    {if(original_direction == "less")ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = x_bar_value, ymin = 0, ymax = Inf), fill = "lightgreen", alpha = 0.009)} +
    {if(original_direction == "less")geomtextpath::geom_textvline(label = paste0("p-value"), xintercept = x_bar_value, vjust = -0.8, linetype = 'dotted', color = "gray50")} +

    {if(original_direction %in% c("different", "less"))geomtextpath::geom_textvline(label = paste0("Sample mean"), xintercept = x_bar_value, vjust = 1.6, linewidth = 3, linecolor = "green4")} +

    # setting the p-values if the original_direction == "greater"
    {if(original_direction == "greater")ggplot2::geom_rect(ggplot2::aes(xmin = x_bar_value, xmax = Inf, ymin = 0, ymax = Inf), fill = "lightgreen", alpha = 0.009)} +
    {if(original_direction == "greater")geomtextpath::geom_textvline(label = paste0("p-value"), xintercept = x_bar_value, vjust = 1.55, linetype = 'dotted', color = "gray50")} +
    {if(original_direction == "greater")geomtextpath::geom_textvline(label = paste0("Sample mean"), xintercept = x_bar_value, vjust = -0.9, linewidth = 3, linecolor = "green4")} +

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
    ggplot2::labs(caption = "Simulation-Based Null Distribution",
                  subtitle = paste0("Sample mean: ", x_bar_value, "\n", cil*100, "% Confidence Interval: ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits = 2), "\n", "p-value: ", p_val_value),
                  x = expression(paste("Statistic: Mean (", mu, ")")),
                  y = "Density")

  # save the graph
  if(!is.null(save_as)){
    suppressWarnings(ggplot2::ggsave(graph, file = paste0("hypmean.", save_as), width=8, height=4))
  }

  #print information and return the graph
  print(paste0("The sample mean is ", x_bar_value))
  print(paste0("The true population mean is between ", round(ci$lower_ci, digits = 2), " and ", round(ci$upper_ci, digits =2), " at ", cil*100, "% confidence interval"))
  print(paste0("The p-value is ", p_val_value))
  print(paste0("The alternative hypothesis is that the mean of ", deparse(substitute(x)), " is " , label_condition, " than ", nullhyp))
  print(paste0("Possible Interpretation:"))
  {if(p_val_value < 0.5)print(paste0("The p-value is small and the sample data represents evidence in favor of the alternative hypothesis."))}
  {if(p_val_value > 0.51)print(paste0("The p-value is large and the sample data does not represent evidence in favor of the alternative hypothesis."))}

  #print(graph)
  suppressWarnings(ggplot2:::print.ggplot(graph))
}
