
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
    theme(legend.position = 'none') +
    coord_flip()
  )
}

makeplot_rppAdjustedSalaryByLocation <- function(data_to_plot) {
  return (
    ggplot(
      data_to_plot,
      aes(
        x=reorder(Location, -RppAdjSalary),
        y=RppAdjSalary,
        fill=(Location == wichita_area_name)
      )
    ) +
    geom_col() +
    ylab('RPP-Adjusted Salaries') +
    xlab('') +
    theme(legend.position = 'none') +
    coord_flip()
  )
}