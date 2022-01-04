library(dplyr)
summary(airquality)
airquality_no_na <- airquality %>% filter(!is.na(Ozone) & !is.na(Solar.R))
airquality_no_na
airquality.month.mean <- airquality %>% group_by(Month) %>% summarise( mean_Ozone=mean(Ozone,na.rm=T),
                                              mean_Solar.R=mean(Solar.R,na.rm=T),
                                              median_Ozone=median(Ozone,na.rm=T),
                                              median_Solar.R=median(Solar.R,na.rm=T))
airquality.month.mean %>% arrange(desc(mean_Ozone),desc(mean_Solar.R))
