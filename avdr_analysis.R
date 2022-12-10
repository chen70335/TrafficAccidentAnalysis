library(dplyr)
library(ggplot2)
library(plotly)
library(gganimate)
library(gridExtra)
library(stringr)
library(reshape2)
library(gapminder)


# 2019 AVDR Dataset
avdr_2019 = read.csv('2019AutonomousVehicleDisengagementReports.csv')
avdr_2019$Year = 2019
avmr_2019 = read.csv('2019AutonomousMileageReports.csv')

# 2020 AVDR Dataset
avdr_2020 = read.csv('2020-Autonomous-Vehicle-Disengagement-Reports.csv')
avdr_2020$Year = 2020
avmr_2020 = read.csv('2020-Autonomous-Mileage-Reports_Amended-4.csv')

# 2021 AVDR Dataset + Renaming columns
avdr_2021 = read.csv('2021-Autonomous-Vehicle-Disengagements-Reports-CSV.csv')
avdr_2021 = avdr_2021 %>% select(-X)
avdr_2021$Year = 2021
avmr_2021 = read.csv('2021-Autonomous-Mileage-Reports-CSV.csv')

avdr = do.call('rbind', list(avdr_2019, avdr_2020, avdr_2021))
avdr$Manufacturer = str_to_upper(avdr$Manufacturer)
avdr = avdr %>% rename(disengage_initiator = DISENGAGEMENT.INITIATED.BY..AV.System..Test.Driver..Remote.Operator..or.Passenger.)
avdr = avdr %>% rename(capable_without_driver = VEHICLE.IS.CAPABLE.OF.OPERATING.WITHOUT.A.DRIVER..Yes.or.No.)
avdr = avdr %>% rename(driver_present = DRIVER.PRESENT..Yes.or.No.)
avdr = avdr %>% rename(disengage_location = DISENGAGEMENT.LOCATION..Interstate..Freeway..Highway..Rural.Road..Street..or.Parking.Facility.)
avdr = avdr %>% rename(disengage_description = DESCRIPTION.OF.FACTS.CAUSING.DISENGAGEMENT)
avdr = slice(avdr, -5211)
for (i in seq(9128, 9164)) {
  avdr[i, "disengage_initiator"] = avdr[i, "driver_present"]
}

# Unifying column values for manufacturer names
avdr$Manufacturer[avdr$Manufacturer == 'AURORA INNOVATION, INC.'] =
  'AURORA OPERATIONS, INC.'
avdr$Manufacturer[avdr$Manufacturer == 'AUTOX TECHNOLOGIES, INC.'] =
  'AUTOX TECHNOLOGIES, INC'
avdr$Manufacturer[avdr$Manufacturer == 'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC.'] =
  'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC'
avdr$Manufacturer[avdr$Manufacturer == 'MERCEDES-BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC.'] =
  'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC'
avdr$Manufacturer[avdr$Manufacturer == 'NISSAN NORTH AMERICA, INC DBA ALLIANCE INNOVATION LAB'] =
  'NISSAN NORTH AMERICA, INC'
avdr$Manufacturer[avdr$Manufacturer == 'TOYOTA RESEARCH INSTITUTE'] =
  'TOYOTA RESEARCH INSTITUTE, INC.'
avdr$Manufacturer[avdr$Manufacturer == 'UDELV, INC.'] =
  'UDELV, INC'
avdr$Manufacturer[avdr$Manufacturer == 'NURO'] =
  'NURO, INC'

# Unifying column values for disengagement initiator
avdr$disengage_initiator[avdr$disengage_initiator == 'Driver'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Passenger'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Test driver'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Safety Driver'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Vehicle Operator'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Operator'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Test Drive'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Test Driver - Soft Stop'] =
  'Test Driver'
avdr$disengage_initiator[avdr$disengage_initiator == 'Software'] =
  'AV System'
avdr$disengage_initiator[avdr$disengage_initiator == 'AV System - Emergency Stop'] =
  'AV System'


# Unifying column values for disengagement location
avdr$disengage_location[avdr$disengage_location == 'freeway'] =
  'Highway'
avdr$disengage_location[avdr$disengage_location == 'Freeway'] =
  'Highway'
avdr$disengage_location[avdr$disengage_location == 'HIGHWAY'] =
  'Highway'
avdr$disengage_location[avdr$disengage_location == 'highway'] =
  'Highway'
avdr$disengage_location[avdr$disengage_location == 'street'] =
  'Street'
avdr$disengage_location[avdr$disengage_location == 'STREET'] =
  'Street'
avdr$disengage_location[avdr$disengage_location == 'Rural Road'] =
  'Street'
avdr$disengage_location[avdr$disengage_location == 'Rural'] =
  'Street'
avdr$disengage_location[avdr$disengage_location == 'parking facility'] =
  'Parking Facility'
avdr$disengage_location[avdr$disengage_location == 'Parking Lot'] =
  'Parking Facility'
avdr$disengage_location[avdr$disengage_location == 'Parking facility'] =
  'Parking Facility'

#656 entries with lane (change, maneuver, etc)
#1498 with perception (issues due to road conditions, user behavior, etc.)
#237 due to localization (accuracy / discrepancy)
#1256 due to software (performance issues, detection)
#924 due to motion (Planning, control, etc)


avdr %>% group_by(Year) %>% select(disengage_initiator) %>% table() %>% data.frame() %>% 
  ggplot(aes(x=Year, y=Freq, fill=disengage_initiator)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=Freq, label=Freq),
            vjust = -0.8, size = 2) +
  labs(title='Autonomous Vehicle Testing Over the Years',
       subtitle = '# of disengagements filtered by disengagement initiator or location')

avdr %>% group_by(Year) %>% summarise(Freq=length(Manufacturer), disengage_initiator) %>% 
  ggplot(aes(x=Year, y=Freq, fill=disengage_initiator)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=Freq, label=Freq),
            vjust = -0.8, size = 2) +
  labs(title='Autonomous Vehicle Testing Over the Years',
       subtitle = '# of disengagements filtered by disengagement initiator or location')

avdr_by_company = avdr %>% group_by(Year, Manufacturer) %>%
  select(Year) %>% table() %>% data.frame() %>% 
  rename(Total_Disengagements=Freq)
write.csv(avdr_by_company, '2019_to_2021_av_disengagement_report.csv', row.names = FALSE)

avmr_2019 = avmr_2019 %>% group_by(Manufacturer) %>% 
  summarize(Total_Mileage=sum(ANNUAL.TOTAL)) %>% data.frame()
avmr_2019$Year = 2019

avmr_2020$ANNUAL.TOTAL = gsub(',', '', as.character(avmr_2020$ANNUAL.TOTAL))
avmr_2020 = avmr_2020 %>% group_by(Manufacturer) %>% 
  summarize(Total_Mileage=sum(as.integer(ANNUAL.TOTAL))) %>% data.frame()
avmr_2020$Year = 2020

avmr_2021$ANNUAL.TOTAL = gsub(',', '', as.character(avmr_2021$ANNUAL.TOTAL))
avmr_2021$ANNUAL.TOTAL = gsub(' ', '', as.character(avmr_2021$ANNUAL.TOTAL))
avmr_2021 = avmr_2021 %>% group_by(Manufacturer) %>% 
  summarize(Total_Mileage=sum(as.integer(ANNUAL.TOTAL))) %>% data.frame()
avmr_2021$Year = 2021

avmr = do.call('rbind', list(avmr_2019, avmr_2020, avmr_2021))
avmr$Manufacturer = str_to_upper(avmr$Manufacturer)

avmr$Manufacturer[avmr$Manufacturer == 'AURORA INNOVATION, INC.'] =
  'AURORA OPERATIONS, INC.'
avmr$Manufacturer[avmr$Manufacturer == 'AUTOX TECHNOLOGIES, INC.'] =
  'AUTOX TECHNOLOGIES, INC'
avmr$Manufacturer[avmr$Manufacturer == 'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC.'] =
  'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC'
avmr$Manufacturer[avmr$Manufacturer == 'MERCEDES0BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC.'] =
  'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC'
avmr$Manufacturer[avmr$Manufacturer == 'MERCEDES-BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC.'] =
  'MERCEDES BENZ RESEARCH & DEVELOPMENT NORTH AMERICA, INC'
avmr$Manufacturer[avmr$Manufacturer == 'NISSAN NORTH AMERICA, INC DBA ALLIANCE INNOVATION LAB'] =
  'NISSAN NORTH AMERICA, INC'
avmr$Manufacturer[avmr$Manufacturer == 'TOYOTA RESEARCH INSTITUTE'] =
  'TOYOTA RESEARCH INSTITUTE, INC.'
avmr$Manufacturer[avmr$Manufacturer == 'UDELV, INC.'] =
  'UDELV, INC'
avmr$Manufacturer[avmr$Manufacturer == 'NURO'] =
  'NURO, INC'
write.csv(avmr, '2019_to_2021_av_mileage_report.csv', row.names=FALSE)

avmr %>% group_by(Year) %>% summarize(Total_Mileage=sum(Total_Mileage)) %>% 
  data.frame() %>% 
  ggplot(aes(x=Year, y=Total_Mileage)) +
  geom_bar(stat='identity')

by_company_df = merge(avdr_by_company, avmr, by=c('Manufacturer', 'Year'), all=TRUE)
by_company_df[is.na(by_company_df)] = 0
by_company_df$avg_miles_per_disengagement = round(by_company_df$Total_Mileage/by_company_df$Total_Disengagements, 1)
by_company_df$avg_miles_per_disengagement[is.nan(by_company_df$avg_miles_per_disengagement)] = 0
by_company_df$Year = as.integer(as.character(by_company_df$Year))
by_company_df$Total_Mileage = round(by_company_df$Total_Mileage, 2)
by_company_df = by_company_df %>% rename(Company = Manufacturer) 
by_company_df = by_company_df %>% 
  mutate(interactive_text = paste(Company, ',
',Total_Mileage,
                                  'Miles, 
',Total_Disengagements, 'Disengagements,
', 'MPD: ', avg_miles_per_disengagement))
 write.csv(by_company_df, '2019_to_2021_disengagement_reports.csv')


by_company_melted = by_company_df %>% rename(`# of Disengagements` = Total_Disengagements,
         `Total Mileage Tested` = Total_Mileage) %>% 
  melt(id.vars = c('Year', 'Company', 'avg_miles_per_disengagement', 'interactive_text'),
       variable.name = 'Data') %>% 
  rename(Value = value)
by_company_melted$Value = round(by_company_melted$Value)



filter_company_plot = by_company_melted %>%
  filter(Company %in% c('APPLE INC.', 'AIMOTIVE INC.')) %>% 
  ggplot(aes(x=Year, y=Value, color=Company,
             shape=Data, text=interactive_text,
             size=avg_miles_per_disengagement)) + 
  geom_point() +
  labs(title='Total Testing Mileage vs. # of Disengagements in AV Testing',
       subtitle = "Each company's number of disengagements compared to mileage tested, over the years")
filter_company_plot %>% ggplotly(tooltip = 'interactive_text') 


overview_plot = by_company_df %>% 
  ggplot(aes(x=Total_Mileage, y=Total_Disengagements,
             size=avg_miles_per_disengagement,
             color=Company, text=interactive_text)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  xlim(0, 2500000) +
  ylim(0, 3000) +
  labs(title = paste('Year: ', 2020), x = 'Total Mileage Tested', y = '# of Disengagements') +
  transition_time(year) +
  ease_aes('linear')




overview_plot = by_company_df %>% 
  filter(Year == input$av_year,
         avg_miles_per_disengagement >= input$Mpd) %>% 
  arrange(-avg_miles_per_disengagement) %>% 
  ggplot(aes(x=Total_Mileage, y=Total_Disengagements,
             size=avg_miles_per_disengagement,
             color=Company, text=interactive_text)) +
  geom_point(alpha=0.5) +
  theme_bw() +
  xlim(0, 2500000) +
  ylim(0, 3000) +
  labs(x = 'Total Mileage Tested', y = '# of Disengagements',
       title = paste('Year: ', input$av_year, ', ',
                     '*Bubble size indicates miles per disengagement')) +
  theme(plot.title = element_text(size=10),
        axis.title = element_text(size=10),
        axis.title.y = element_text(angle = 90),
        legend.title = element_text(size=10)
  ) +
  guides(size=FALSE)
overview_plot %>% ggplotly(tooltip = 'interactive_text') %>% 
  layout(width=900, height=400)




  


