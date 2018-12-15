library(dplyr)
library(dataRetrieval)
library(lubridate)
library(tidyr)
library(ggplot2)
library(viridis)


# Get data for the Yahara River at Windsor in Dane County, Wisconsin
yahara_no <- '05427718'

#80154 Suspended sediment concentration (SSC)懸浮沉積物
#00060 Stream flow, mean. daily 平均日流量
#00665 Phosphorus 磷
#00671 Orthophosphate 正磷酸鹽
params <- c('00060', '00671', '80154', '00665')

# get daily values from NWIS
yahara_dat <- readNWISdv(siteNumbers = yahara_no,
                         parameterCd = params, 
                         startDate = '1997-10-01',
                         endDate = '2017-09-30')
# rename columns using renameNWISColumns from package dataRetrieval
yahara_dat <- renameNWISColumns(yahara_dat, 
                                p00665 = "TP_mgL",
                                p00671 = "Orthophosphate_mgL",
                                p80154 = "SS_mgL")

# use function `grep` to identify which columns are code columns
grep('_cd', names(yahara_dat)) # returns the index of the match
## [1]  1  5  7  9 11
grep('_cd', names(yahara_dat), value = TRUE) # returns the matched elements themselves
# change the code column names to be more explicit about what they contain
# using function gsub
gsub('_cd', '_code', names(yahara_dat))

yahara_dat <- select(yahara_dat, -contains('_cd'))
yahara_dat <- addWaterYear(yahara_dat)
# calculate cumulative discharge for each year by first grouping by water year,
# and then using the "cumsum" function. Add day of water year for plotting purposes.
# These steps will build a new dataframe, with the existing information in yahara_dat
# but with two additional columns.
cumulative_dat <- group_by(yahara_dat, waterYear) %>%
                  mutate(cumulative_dis = cumsum(Flow), 
                         wy_doy = seq(1:n())) #n()依照wateryear分組而變成1:365

yahara_dat %>% mutate(cumulative_dis = cumsum(Flow), 
                      wy_doy = seq(1:n())) #n()沒分組則依照yahara_dat的列數

# visually compare cumulative discharge across years
cumulative_dat %>% 
  ggplot(aes(x = wy_doy, y = cumulative_dis, group = waterYear)) +
  geom_line(aes(color = waterYear),size = 1.5) +
  scale_color_viridis() +
  scale_x_continuous(breaks = c(1, 93, 184, 275), 
                     labels = c("Oct 1", "Jan 1", "Apr 1", "July 1")) +
  theme_bw() +
  labs(color = "年份", x = "", y = "累積流量")
# sum discharge by water year, also check to be sure each year has 
# data for every day by counting observations in a year
year_sums <- yahara_dat %>%
  filter(!is.na(Flow)) %>%
  group_by(waterYear) %>%
  summarize(yearly_discharge = sum(Flow, na.rm = TRUE),
            ndays = n()) 
# use ifelse to create a categorical variable that has two conditions
# 根據year_sums中位數上下進行分組
year_sums <- mutate(year_sums, 
                    discharge_high_low = 
                      ifelse(yearly_discharge <= median(year_sums$yearly_discharge),
                                                'low', 'high'))
# we should get equal group sizes of "low" and "high"
table(year_sums$discharge_high_low)

cut_vals <- quantile(year_sums$yearly_discharge,
                     probs = c(0.25, 0.75), 
                     na.rm = TRUE)
year_sums <- mutate(year_sums, 
                    discharge_dry_normal_wet = 
                      cut(yearly_discharge, 
                          breaks = c(-Inf, cut_vals, Inf), 
                          labels = c("dry", "normal", "wet")))
table(year_sums$discharge_dry_normal_wet)


####case_when用法#####
data_frame(x = 1:10, 
           y = rnorm(10),
           name = c("Apple", "Banana", "Kiwi", 
                    "Orange", "Watermelon", "Grapes", 
                    "Pear", "Cantelope", "Tomato", "Satsuma")) %>% 
  mutate(name_poor = case_when(y < 0 ~ name,TRUE ~ "123"))
  #y欄小於0的返還name
  #TRUE ~ 剩餘不符合規則的，返還"123"
############################################################


year_sums <- year_sums %>%
  mutate(discharge_before_after = case_when(
    yearly_discharge > median(year_sums$yearly_discharge) & waterYear > 2000 ~ "High Discharge, After",
    yearly_discharge > median(year_sums$yearly_discharge) & waterYear <= 2000 ~ "High Discharge, Before",
    yearly_discharge <= median(year_sums$yearly_discharge) & waterYear <= 2000 ~ "Low Discharge, Before",
    yearly_discharge <= median(year_sums$yearly_discharge) & waterYear > 2000 ~ "Low Discharge, After"
  ))
# check how many years of data fall into each category
table(year_sums$discharge_before_after)

cumulative_dat %>% ggplot(aes(x = Date, y = TP_mgL)) +
  geom_line(color = "pink",size = 1.3) +
  labs(x = "Date", y = 'TP (mg/L)') +
  theme_bw()

#gather 先命名key和value的名字,後面在指定要折疊的變數
yahara_long <- cumulative_dat %>%
  gather(key = measured_parameter, 
         value = value, 
         Flow, TP_mgL, Orthophosphate_mgL, SS_mgL)

yahara_long %>% 
  ggplot( aes(x = Date, y = value)) +
  geom_point() +
  facet_wrap(~measured_parameter, ncol = 1, scales = 'free_y') +
  labs(x = "Date", y = '') +
  theme_bw()

##等於 cumulative_dat %>% mutate(TP_OrthoP = TP_mgL/Orthophosphate_mgL)
yahara_wide <- yahara_long %>%
  spread(key = measured_parameter, value = value) %>%
  mutate(TP_OrthoP = TP_mgL/Orthophosphate_mgL)

# again, gather the phosphorus variables
# merge with our wet/dry variable of interest
p_dat <- ungroup(yahara_wide) %>%
  select(Date, TP_mgL, Orthophosphate_mgL, TP_OrthoP, Flow, waterYear) %>%
  gather(key = P_variables, value = value, -Date, -Flow, -waterYear) %>%
  left_join(select(year_sums, discharge_dry_normal_wet, waterYear))


# visualize phosphorus ~ discharge relationships and color code
# by wet/dry year
p_dat %>% 
  ggplot(aes(x = Flow, y = value)) +
  geom_point(aes(color = discharge_dry_normal_wet), alpha = 0.3) + 
  facet_wrap(~P_variables, ncol = 1, scales = 'free_y') +
  geom_smooth(aes(group = discharge_dry_normal_wet, color = discharge_dry_normal_wet), 
              alpha = 0.5, 
              method = 'lm') +
  scale_y_log10() +
  scale_x_log10() +
  annotation_logticks() +
  theme_bw() +
  labs(x = "流量/排放量", color = "年度乾溼條件")


# set USGS site and pull data
yahara_site2 <- '05427850'

yahara_dat2 <- readNWISdv(siteNumbers = yahara_site2, 
                          parameterCd = '00665', 
                          startDate = '1997-10-01', 
                          endDate = '2017-09-30') %>%
  select(Date, TP_Yah_Hwy113 = X_00665_00003)

# merge the two Yahara TP datasets
yahara_combo <- left_join(yahara_dat2, select(yahara_dat, TP_Yah_Windsor = TP_mgL, Date))

# find which site has has the maximum value by row
# 逐行運算
yahara_highest <- yahara_combo %>%
  rowwise() %>% ##針對行來group
  mutate(max_tp_site_rowwise = 
           c('Yahara_at_Hwy113', 'Yahara_at_Windsor')[which.max(c(TP_Yah_Hwy113, TP_Yah_Windsor))],
         max_tp_site_ifelse = ifelse(TP_Yah_Hwy113 > TP_Yah_Windsor,"Yahara_at_Hwy113","Yahara_at_Windsor" ))
table(yahara_highest$max_tp_site_rowwise);table(yahara_highest$max_tp_site_ifelse)
