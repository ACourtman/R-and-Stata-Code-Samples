

# Set working directory where data files are location (set your wd to wherever you store the files; files included in repository)

setwd("C:/Users/win/Dropbox/Research & Code Example/Data")

# install and load packages

install.packages("tidyverse")
install.packages("readxl")
install.packages("haven")
install.packages("cowplot")
install.packages("sandwich") 
install.packages("fixest")
install.packages("estimatr")


library("tidyverse")
library("readxl")
library("haven")
library("cowplot")
library("sandwich")  
library("fixest")
library("estimatr")

# Load Datasets

SessionTemp <- read_excel("SessionTemp.xlsx")

Final_Dataset <- read_dta("MasterListFinal.dta")

# create subsets of the dataset by country

Final_Dataset <- Final_Dataset |> 
  mutate(
    Country = case_when(country_city == "india_delhi" ~ "India",
                        country_city == "mexico_chapingo" ~ "Mexico",
                        country_city == "usa_davis" ~ "USA",
                        country_city == "kenya_nairobi" ~ "Kenya"))

# Recoding "q1_gender" into "Gender"

Final_Dataset <- Final_Dataset |> 
  filter(q1_gender == "Male" | q1_gender == "Female") |> 
  mutate(Gender = q1_gender)

# Creating Triggered & Not triggered subsets

Final_Dataset <- Final_Dataset |> 
  mutate(Trigger = if_else(trigger == 1, "Triggered", "Not Triggered"))

# Creating Interaction Variables 

Final_Dataset <- Final_Dataset |> 
  mutate(TempTrigger = mean_session_temp*trigger,
         TempMale = mean_session_temp*male,
         TempTriggerMale = mean_session_temp**trigger*male)

# Recoding "q2_age" into "age" and "q14_socioeconomic_status" into "socio-econ_status"

Final_Dataset <- Final_Dataset |> 
  mutate(age = q2_age,
         socio_econ_status = q14_socioeconomic_status)


# Creating Temperature subset by Country

India_Session_Temp <- subset(SessionTemp, Location == "India")
Mexico_Session_Temp <- subset(SessionTemp, Location == "Mexico")
US_Session_Temp <- subset(SessionTemp, Location == "US")
Kenya_Session_Temp <- subset(SessionTemp, Location == "Kenya")





# Create Temperature Variation graphs for Combined dataset and each site

# Combined

Combined_Session_Temp_Variation <- ggplot(SessionTemp, aes(x = session_n,y = mean_temp_celsius, color = Location)) + 
  geom_line(aes(group = Location), linewidth = 1.1) + 
  geom_line(y = 30, color = "black", linewidth = 1) + 
  geom_point(aes(x = session_n,y = mean_temp_celsius, shape = Location), size = 3) +
  labs(
    title = "Session Temp. (°C) Variation by Location", 
    x = "Session Number", 
    y = "Mean Temperature (Celsius)") + 
  scale_x_continuous(breaks = seq(0, 35, 5)) + 
  scale_y_continuous(limits = c(15, 36)) + 
  theme(plot.title = element_text(hjust = 0.5)) 


# India

India_Temp_Variation <- ggplot(India_Session_Temp, aes(x = session_n,y = mean_temp_celsius, color = Location)) + 
  geom_line(aes(group = Location), linewidth = 1.1) + 
  geom_line(y = 30, color = "black", linewidth = 1) + 
  geom_point(aes(x = session_n,y = mean_temp_celsius, shape = Location), size = 3) +
  labs(
    title = "Session Temp. (°C) Variation - India", 
    x = "Session Number", 
    y = "Mean Temperature (Celsius)") + 
  scale_x_continuous(breaks = seq(0, 35, 5)) + 
  scale_y_continuous(limits = c(15, 36)) + 
  theme(plot.title = element_text(hjust = 0.5))


# Mexico

Mexico_Temp_Variation <- ggplot(Mexico_Session_Temp, aes(x = session_n,y = mean_temp_celsius, color = Location)) + 
  geom_line(aes(group = Location), linewidth = 1.1) + 
  geom_line(y = 30, color = "black", linewidth = 1) + 
  geom_point(aes(x = session_n,y = mean_temp_celsius, shape = Location), size = 3) +
  labs(
    title = "Session Temp. (°C) Variation - Mexico", 
    x = "Session Number", 
    y = "Mean Temperature (Celsius)") + 
  scale_x_continuous(breaks = seq(0, 35, 5)) + 
  scale_y_continuous(limits = c(15, 36)) + 
  theme(plot.title = element_text(hjust = 0.5))


# US  

US_Temp_Variation <- ggplot(US_Session_Temp, aes(x = session_n,y = mean_temp_celsius, color = Location)) + 
  geom_line(aes(group = Location), linewidth = 1.1) + 
  geom_line(y = 30, color = "black", linewidth = 1) + 
  geom_point(aes(x = session_n,y = mean_temp_celsius, shape = Location), size = 3) +
  labs(
    title = "Session Temp. (°C) Variation - US", 
    x = "Session Number", 
    y = "Mean Temperature (Celsius)") + 
  scale_x_continuous(breaks = seq(0, 35, 5)) + 
  scale_y_continuous(limits = c(15, 36)) + 
  theme(plot.title = element_text(hjust = 0.5))


# Kenya

Kenya_Temp_Variation <- ggplot(Kenya_Session_Temp, aes(x = session_n,y = mean_temp_celsius, color = Location)) + 
  geom_line(aes(group = Location), linewidth = 1.1) + 
  geom_line(y = 30, color = "black", linewidth = 1) + 
  geom_point(aes(x = session_n,y = mean_temp_celsius, shape = Location), size = 3) +
  labs(
    title = "Session Temp. (°C) Variation - Kenya", 
    x = "Session Number", 
    y = "Mean Temperature (Celsius)") + 
  scale_x_continuous(breaks = seq(0, 35, 5)) + 
  scale_y_continuous(limits = c(15, 36)) + 
  theme(plot.title = element_text(hjust = 0.5))


# view above graphs

Combined_Session_Temp_Variation

India_Temp_Variation

Mexico_Temp_Variation

US_Temp_Variation

Kenya_Temp_Variation








# create subsets of the dataset by country

Final_Dataset <- Final_Dataset |> 
  mutate(
    Country = case_when(country_city == "india_delhi" ~ "India",
                        country_city == "mexico_chapingo" ~ "Mexico",
                        country_city == "usa_davis" ~ "USA",
                        country_city == "kenya_nairobi" ~ "Kenya"))






# Percentage Altruism Breakdown - by Country

Percent_Altruism_Breakdown <- Final_Dataset |> ggplot(
  aes(x = Altruism, color = "orange", fill = "orange")) +
  geom_bar(aes(y = after_stat(prop)), width = .6) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.2)) + 
  labs(
    x = "Altruism",
    y = "Proportion",
    title = "Altruism by Country"
  ) +
  theme(legend.position = "none")


# Percentage Altruism Breakdown - Combined Sample

Percent_Altruism_Total <- Final_Dataset |> ggplot(
  aes(x = Altruism, color = "orange", fill = "orange")) +
  geom_bar(aes(y = after_stat(prop)), width = .6) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1)) + 
  labs(
    x = "Costly Altruism",
    y = "Proportion",
    title = "Altruism - Entire Sample"
  ) +
  theme(legend.position = "none")






# Frequency of Costly Altruism - by Country

Freq_CostlyAlt_Breakdown <- Final_Dataset |> ggplot(
  aes(x = CostlyAlt, color = "orange", fill = "orange")) +
  geom_bar(width = .6) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(breaks = seq(0, 300, 50)) + 
  labs(
    x = "Costly Altruism",
    y = "Frequency",
    title = "Costly Altruism by Country"
  ) +
  theme(legend.position = "none")


# Frequency of Costless Altruism - by Country

Freq_CostlessAlt_Breakdown <- Final_Dataset |> ggplot(
  aes(x = CostlessAlt, color = "orange", fill = "orange")) +
  geom_bar(width = .6) +
  facet_grid(. ~ Country) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(breaks = seq(0, 300, 50)) + 
  labs(
    x = "Costless Altruism",
    y = "Frequency",
    title = "Costless Altruism by Country"
  ) +
  theme(legend.position = "none")






# Frequency of Costly Altruism - Combined Sample

Freq_CostlyAlt_Total <- Final_Dataset |> ggplot(
  aes(x = CostlyAlt, color = "orange", fill = "orange")) +
  geom_bar(width = .6) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(breaks = seq(0, 750, 50)) + 
  labs(
    title = "Costly Altruism - Entire Sample"
  ) +
  theme(legend.position = "none")


# Frequency of Costless Altruism - Combined Sample

Freq_CostlessAlt_Total <- Final_Dataset |> ggplot(
  aes(x = CostlessAlt, color = "orange", fill = "orange")) +
  geom_bar(width = .6) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(breaks = seq(0, 750, 50)) + 
  labs(
    title = "Costless Altruism - Entire Sample"
  ) +
  theme(legend.position = "none")







# View Altruism Bar Charts

Percent_Altruism_Breakdown
Percent_Altruism_Total

Freq_CostlyAlt_Breakdown
Freq_CostlessAlt_Breakdown

Freq_CostlyAlt_Total
Freq_CostlessAlt_Total














# Altruism Bar charts - Males, Females

p_male <- Final_Dataset |> 
  filter(Gender == "Male") |> 
  ggplot(aes(x = Altruism,  color = "orange", fill = "orange")) +
  geom_bar(aes(y = after_stat(prop)), width = .6) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1)) + 
  ylim(0, 0.7) +
  labs(
    x = "Altruism",
    y = "Proportion",
    title = "Altruism - Men"
  ) +
  theme(legend.position = "none")

p_female <- Final_Dataset |> 
  filter(Gender == "Male") |>  
  ggplot(aes(x = Altruism,  color = "orange", fill = "orange")) +
  geom_bar(aes(y = after_stat(prop)), width = .6) +
  scale_x_continuous(breaks = seq(0, 1, 1)) + 
  scale_y_continuous(labels = scales::percent_format(), 
                     breaks = seq(0, 1, 0.1)) + 
  ylim(0, 0.7) +
  labs(
    x = "Altruism",
    y = "Proportion",
    title = "Altruism - Women"
  ) +
  theme(legend.position = "none")


# Combine the plots using cowplot package

plot_grid(p_male, p_female, ncol = 2)





# Altruism-Temp variation - Males, Females, Combined

Final_Dataset |>
  filter(Gender == "Male") |> 
  ggplot(aes(x = mean_session_temp, y = Altruism)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism - Temp Variation - Men",
    x = "Mean Session Temperature",
    y = "Altruism"
  )

Final_Dataset |>
  filter(Gender == "Female") |> 
  ggplot(aes(x = mean_session_temp, y = Altruism)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism - Temp Variation - Women",
    x = "Mean Session Temperature",
    y = "Altruism"
  )

Final_Dataset |> 
  ggplot(aes(x = mean_session_temp, y = Altruism, color = Gender)) +
  geom_point(aes(shape = Gender), alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism - Temp Variation - Men v. Women",
    x = "Mean Session Temperature",
    y = "Altruism"
  )




# Costly Altruism-Temp variation - Males, Females, Combined

Final_Dataset |>
  filter(Gender == "Male") |> 
  ggplot(aes(x = mean_session_temp, y = CostlyAlt)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costly Altruism - Temp. Variation - Men",
    x = "Mean Session Temperature",
    y = "Costly Altruism"
  )

Final_Dataset |>
  filter(Gender == "Female") |> 
  ggplot(aes(x = mean_session_temp, y = CostlyAlt)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costly Altruism - Temp. Variation - Women",
    x = "Mean Session Temperature",
    y = "Costly Altruism"
  )

Final_Dataset |> 
  ggplot(aes(x = mean_session_temp, y = CostlyAlt, color = Gender)) +
  geom_point(aes(shape = Gender), alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costly Altruism - Temp. Variation - Men v. Women",
    x = "Mean Session Temperature",
    y = "Costly Altruism"
  )






# Costless Altruism - Temp variation - Males, Females, Combined

Final_Dataset |>
  filter(Gender == "Male") |> 
  ggplot(aes(x = mean_session_temp, y = CostlessAlt)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costless Altruism - Temp Variation - Men",
    x = "Mean Session Temperature",
    y = "Costless Altruism"
  )

Final_Dataset |>
  filter(Gender == "Female") |> 
  ggplot(aes(x = mean_session_temp, y = CostlessAlt)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costless Altruism - Temp Variation - Women",
    x = "Mean Session Temperature",
    y = "Costless Altruism"
  )

Final_Dataset |> 
  ggplot(aes(x = mean_session_temp, y = CostlessAlt, color = Gender)) +
  geom_point(aes(shape = Gender), alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Costly Altruism - Temp. Variation - Men v. Women",
    x = "Mean Session Temperature",
    y = "Costly Altruism"
  )






# Altruism-Temp Variation - Combined, Trigger, No Trigger

Final_Dataset |> 
  ggplot(aes(x = mean_session_temp, y = Altruism, color = Trigger)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism-Temp. Variation - Triggered v. Not Triggered Groups",
    x = "Mean Session Temperature",
    y = "Altruism"
  )

Final_Dataset |> 
  filter(Trigger == "Triggered") |> 
  ggplot(aes(x = mean_session_temp, y = Altruism, color = Trigger)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism-Temp. Variation - Triggered v. Not Triggered Groups",
    x = "Mean Session Temperature",
    y = "Altruism"
  )

Final_Dataset |> 
  filter(Trigger == "Not Triggered") |> 
  ggplot(aes(x = mean_session_temp, y = Altruism, color = Trigger)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  scale_x_continuous(breaks = seq(19, 35, 1)) + 
  scale_y_continuous(breaks = seq(0, 1, 1)) +
  labs(
    title = "Altruism-Temp. Variation - Triggered v. Not Triggered Groups",
    x = "Mean Session Temperature",
    y = "Altruism"
  )




## Fitting the initial regression model

view(Final_Dataset)

model1 <- lm_robust(Altruism ~ mean_session_temp + IND + USA + KEN + male + trigger + age + socio_econ_status,
                   se_type = "stata",
                   clusters = site_session,
                   data = Final_Dataset)

model1

## Fitting the regression model with specifications including interaction variables

model2 <- lm_robust(Altruism ~ mean_session_temp + IND + USA + KEN + male + trigger + TempTrigger + age + socio_econ_status,
                    se_type = "stata",
                    clusters = site_session,
                    data = Final_Dataset)

model2

model3 <- lm_robust(Altruism ~ mean_session_temp +IND + USA + KEN + male + trigger + TempTrigger + TempMale + age + socio_econ_status,
                    se_type = "stata",
                    clusters = site_session,
                    data = Final_Dataset)

model3

model4 <- lm_robust(Altruism ~ mean_session_temp +IND + USA + KEN + male + trigger + TempTrigger + TempMale + TempTriggerMale + age + socio_econ_status,
                    se_type = "stata",
                    clusters = site_session,
                    data = Final_Dataset)

model4
