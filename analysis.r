# https://www.bls.gov/opub/mlr/2016/article/purchasing-power-using-wage-statistics-with-regional-price-parities-to-create-a-standard-for-comparing-wages-across-us-areas.htm
# Need to make sure dates align if using multiple sources
# Original data is from 2014
# See national_M2017_dl for detailed national 2017 stats
library(readxl)
library(ggplot2)
library(dplyr)
library(gridExtra)

occupations <- c(
  'Web Developers',
  'Software Developers, Applications',
  'Software Developers, Systems Software',
  'Computer Programmers'
)

wichita_area_name <- 'Wichita, KS'
locations <- c(
  wichita_area_name,
  'Kansas City, MO-KS',
  'Oklahoma City, OK',
  'Omaha-Council Bluffs, NE-IA',
  'Des Moines-West Des Moines, IA',
  'Tulsa, OK'
)

# RPPs
rpp <- read_excel(path=paste0(getwd(), '/regional_price_parities.xls'), skip=5) %>%
  mutate(Location=sub('\\s\\(Metropolitan Statistical Area)', '', GeoName), RPP = `2016`) %>%
  filter(!is.na(RPP)) %>%
  add_row(GeoFips=NA, GeoName='National', `2016`=100.0, Location='National', RPP=100.0) %>%
  select(Location, RPP)

# National stats
national_raw <- read_excel(path=paste0(getwd(), '/national_M2017_dl.xlsx'))
total_national_employment = pull(filter(national_raw, OCC_CODE == '00-0000'), TOT_EMP)
national_filtered <- national_raw %>% filter(`OCC_TITLE` %in% occupations)
rm(national_raw)

# MSA stats
msa_raw <- read_excel(path=paste0(getwd(), '/MSA_M2017_dl.xlsx'))
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
plot_per1000ByLocation <- ggplot(location_data, aes(x=reorder(Location, -Per1000Jobs), y=Per1000Jobs, fill=(Location == wichita_area_name))) +
  geom_col() +
  ylab('Developer Jobs Per 1000 Jobs') +
  xlab('') +
  theme(legend.position = 'none') +
  coord_flip()

# Compare RPP adjusted salaries to regional peers
plot_rppAdjustedSalaryByLocation <- ggplot(location_data, aes(x=reorder(Location, -RppAdjSalary), y=RppAdjSalary, fill=(Location == wichita_area_name))) +
  geom_col() +
  ylab('RPP-Adjusted Salaries') +
  xlab('') +
  theme(legend.position = 'none') +
  coord_flip()


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
