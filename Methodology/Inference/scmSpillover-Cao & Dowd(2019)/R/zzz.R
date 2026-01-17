# Global variables declaration to avoid R CMD check NOTEs
# These are column names used in ggplot2 aes()

utils::globalVariables(c(

  "Period", "Effect", "Method", "Lower", "Upper", "Value", "Type"
))

#' @importFrom stats quantile rnorm sd
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_hline geom_vline
#' @importFrom ggplot2 geom_ribbon scale_color_manual labs theme_minimal theme
#' @importFrom ggplot2 annotate ggsave
NULL
