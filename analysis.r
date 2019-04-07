library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)

source(paste0(getwd(), '/chart_functions.r'))

force_digits <- function (num, n_digits) {
  return (format(round(num, n_digits), nsmall=n_digits))
}

data_year <- 2018

occupations <- c(
  'Web Developers',
  'Software Developers, Applications',
  'Software Developers, Systems Software',
  'Computer Programmers'
)

wichita_area_name <- 'Wichita, KS'
peer_locations <- c(
  wichita_area_name,
  'Kansas City, MO-KS',
  'Oklahoma City, OK',
  'Omaha-Council Bluffs, NE-IA',
  'Des Moines-West Des Moines, IA',
  'Tulsa, OK'
)

non_peer_locations <- c(
  'San Jose-Sunnyvale-Santa Clara, CA',
  'Denver-Aurora-Lakewood, CO',
  'Austin-Round Rock, TX',
  'Boston-Cambridge-Newton, MA NECTA Division'
)

locations <- c(peer_locations, non_peer_locations)

# RPPs
rpp <- read_excel(path=paste0(getwd(), '/regional_price_parities_full.xls'), skip=5) %>%
  mutate(Location=sub('\\s\\(Metropolitan Statistical Area)', '', GeoName), RPP = `2016`) %>%
  filter(!is.na(RPP)) %>%
  filter(RPP != '(NA)') %>%
  add_row(GeoFips=NA, GeoName='National', `2016`=100.0, Location='National', RPP=100.0) %>%
  replace(., . == 'Boston-Cambridge-Newton, MA-NH', 'Boston-Cambridge-Newton, MA NECTA Division') %>%
  select(Location, RPP) %>%
  mutate(RPP = as.numeric(RPP))

# National stats
national_raw <- read_excel(path=paste0(getwd(), '/national_M', data_year, '_dl.xlsx'))
total_national_employment = pull(filter(national_raw, OCC_CODE == '00-0000'), TOT_EMP)
national_filtered <- national_raw %>% filter(`OCC_TITLE` %in% occupations)
rm(national_raw)

# MSA stats
msa_raw <- read_excel(path=paste0(getwd(), '/MSA_M', data_year, '_dl.xlsx'))
msa_filtered <- msa_raw %>%
  filter(`OCC_TITLE` %in% occupations) %>%
  filter(`AREA_NAME` %in% locations)

# Overall Wichita stats
wichita_raw <- msa_raw %>% filter(AREA_NAME == wichita_area_name, OCC_CODE == '00-0000')
wichita <- list(TotalEmployment = as.numeric(pull(wichita_raw, TOT_EMP)), MeanSalary = as.numeric(pull(wichita_raw, A_MEAN)))
wichita$TotalSalary = wichita$TotalEmployment * wichita$MeanSalary
rm(msa_raw)

chart_msa <- msa_filtered %>%
  select(OCC_TITLE, AREA_NAME, `LOC QUOTIENT`, TOT_EMP, JOBS_1000, A_MEDIAN, A_MEAN)
chart_nat <- national_filtered %>%
  mutate(AREA_NAME = 'National') %>%
  mutate('LOC QUOTIENT' = 1.0) %>%
  mutate('JOBS_1000' = 1000 * as.numeric(TOT_EMP) / total_national_employment) %>%
  select(OCC_TITLE, AREA_NAME, `LOC QUOTIENT`, TOT_EMP, JOBS_1000, A_MEDIAN, A_MEAN)

chart_data <- rbind(chart_nat, chart_msa)
names(chart_data) <- c('Occupation', 'Location', 'LocQuotient', 'TotalEmployment', 'Per1000Jobs', 'MedianSalary', 'MeanSalary')
chart_data <- chart_data %>%
  mutate(
    LocQuotient=as.numeric(LocQuotient),
    Per1000Jobs = as.numeric(Per1000Jobs),
    MedianSalary=as.numeric(MedianSalary),
    MeanSalary=as.numeric(MeanSalary),
    TotalEmployment=as.numeric(TotalEmployment)) %>%
  mutate(SalaryDollars = MeanSalary * TotalEmployment)


location_data <- group_by(chart_data, Location) %>%
  summarize(
    TotalEmployment = sum(TotalEmployment),
    Per1000Jobs = sum(Per1000Jobs),
    MeanSalary = sum(SalaryDollars) / sum(TotalEmployment),
    TotalSalary=sum(SalaryDollars)
  )
location_data <- left_join(location_data, rpp, 'Location') %>%
  mutate(RppAdjSalary = MeanSalary * RPP / 100)

# Compare Per1000Jobs by location
plot_per1000ByLocation <- makeplot_per1000ByLocation(
  location_data %>%
  filter(Location %in% peer_locations)
)

plot_per1000ByLocation_all <- makeplot_per1000ByLocation(location_data)

# Compare RPP adjusted salaries to regional peers
plot_rppAdjustedSalaryByLocation <- makeplot_rppAdjustedSalaryByLocation(
  location_data %>%
  filter(Location %in% peer_locations)
)

plot_rppAdjustedSalaryByLocation_all <- makeplot_rppAdjustedSalaryByLocation(location_data)

# ggplot(chart_data, aes(LocQuotient, MedianSalary, label=Location)) +
#   geom_point(aes(color=Location)) +
#   geom_text(aes(color=Location, label=Location)) +
#   ggtitle(Occupation) +
#   facet_wrap(~ Occupation, ncol = 4)

# charts <- list()
# par(mfrow=c(2, 2))
# for (occupation in occupations) {
#   chart_msa <- msa_filtered %>%
#     filter(OCC_TITLE == occupation) %>%
#     select(AREA_NAME, `LOC QUOTIENT`, A_MEDIAN)
#   chart_nat <- national_filtered %>%
#     filter(OCC_TITLE == occupation) %>%
#     mutate(AREA_NAME = 'National') %>%
#     mutate('LOC QUOTIENT' = 1.0) %>%
#     select(AREA_NAME, `LOC QUOTIENT`, A_MEDIAN)
# 
#   chart_data <- rbind(chart_nat, chart_msa)
#   names(chart_data) <- c('Location', 'LocQuotient', 'MedianSalary')
#   chart_data <- chart_data %>%
#     mutate(LocQuotient=as.numeric(LocQuotient), MedianSalary=as.numeric(MedianSalary))
#   new_chart <- ggplot(chart_data, aes(LocQuotient, MedianSalary, label=Location)) +
#     geom_point() +
#     geom_text(aes(label=Location)) +
#     ggtitle(occupation)
# 
# }

valueToPctStr <- function (value) { paste(format(round(100 * value, 1), nsmall=1), '%', sep='') }
