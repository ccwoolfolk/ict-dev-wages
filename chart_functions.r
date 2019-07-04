
# Compare Per1000Jobs by location
makeplot_per1000ByLocation <- function (data_to_plot) {
  return (
    ggplot(
      data_to_plot,
      aes(
        x=reorder(Location, -Per1000Jobs),
        y=Per1000Jobs,
        fill=(Location == wichita_area_name)
      )
    ) +
    geom_col() +
    ylab('Developer Jobs Per 1000 Jobs') +
    xlab('') +
    theme(legend.position = 'none', axis.text = element_text(size=12)) +
    coord_flip()
  )
}

makeplot_salaryByLocation <- function(data_to_plot, rpp=FALSE, error = 0.03) {
  if (rpp) {
    base_plot <- ggplot(
      data_to_plot,
      aes(
        x=reorder(Location, -RppAdjSalary),
        y=RppAdjSalary,
        fill=(Location == wichita_area_name)
      )
    )

    error_bars <- geom_errorbar(
      aes(
        x=Location,
        ymin=(1-error) * RppAdjSalary,
        ymax=(1 + error) * RppAdjSalary
      ),
      width = 0.2
    )
  } else {
    base_plot <- ggplot(
      data_to_plot,
      aes(
        x=reorder(Location, -MeanSalary),
        y=MeanSalary,
        fill=(Location == wichita_area_name)
      )
    )

    error_bars <- geom_errorbar(
      aes(
        x=Location,
        ymin=(1-error) * MeanSalary,
        ymax=(1 + error) * MeanSalary
      ),
      width = 0.2
    )
  }

  return (
    base_plot +
    geom_col() +
    ylab(if (rpp) 'RPP-Adjusted Salaries' else 'Mean Salaries') +
    xlab('') +
    scale_y_continuous(label=comma) +
    theme(legend.position = 'none', axis.text = element_text(size=12)) +
    coord_flip() +
    error_bars
  )
}