#######descriptive plots#######

library(ggplot2)
############## TO DO ##############
# Weather Plots
# Daylight Plots
# Age distribution
# (unbalanced?) distribution of disease category
# SDI param plots: daily effect (3d) & accumulated effects

#prepare data

n<- length(levels(full.df_7$week))
sel.quantiles_age<- c(0.05,0.25,0.5,0.75,0.95)
age.matrix<- matrix(NA, nrow = n, ncol = length(sel.quantiles_age))
colnames(age.matrix)<- paste0("age_q_",sel.quantiles_age)
gender.vec<- numeric(n)
phi.vec<- numeric(n)
sel.quantiles_chronic<- c(0.05,0.25,0.5,0.75,0.95)
chronic.matrix<- matrix(NA, nrow = n, ncol = length(sel.quantiles_chronic))
colnames(chronic.matrix)<- paste0("chronic_q_",sel.quantiles_chronic)
mean.temperature<- numeric(n)

for(i in seq(1,n)){
  im<- full.df_7|>
    filter(week==levels(full.df_7$week)[i])
  age.matrix[i,]<- quantile(im$age,probs = sel.quantiles_age)
  gender.vec[i]<- mean(im$female)
  phi.vec[i]<- mean(im$PKV)
  chronic.matrix[i,]<- quantile(im$no_all_chronic_diseases, probs = sel.quantiles_chronic)
  mean.temperature[i]<- mean(im$daily_mean_temperature_kelvin)
}

df<- as.data.frame(cbind(as.numeric(levels(full.df_7$week)),
                         age.matrix,
                         gender.vec,
                         phi.vec,
                         chronic.matrix,
                         mean.temperature-273.16))
colnames(df)[1]<- "week"
colnames(df)[14]<- "mean.temperature"

# Define the colours for the quantiles
colours <- c("#FFD6D6", "#FFADAD", "#FF8585", "#FF5C5C", "#FF3333")

# Create the plot
ggplot(df, aes(x = week)) +
  geom_ribbon(aes(ymin = age_q_0.05, ymax = age_q_0.25, fill = "0.05-0.25"), alpha = 0.8, colour = NA) +
  geom_ribbon(aes(ymin = age_q_0.25, ymax = age_q_0.5, fill = "0.25-0.5"), alpha = 0.8, colour = NA) +
  geom_ribbon(aes(ymin = age_q_0.5, ymax = age_q_0.75, fill = "0.5-0.75"), alpha = 0.8, colour = NA) +
  geom_ribbon(aes(ymin = age_q_0.75, ymax = age_q_0.95, fill = "0.75-0.95"), alpha = 0.8, colour = NA) +
  scale_fill_manual(values = colours, guide = FALSE) +
  theme_minimal() +
  labs(x = "Week", y = "Value") +
  ggtitle("Age Quantiles by Week") +
  theme(plot.title = element_text(hjust = 0.5))

library(ggplot2)

# Define the colours for the quantiles
colours <- c("#FFD6D6", "#FFADAD", "#FF8585", "#FF5C5C", "#FF3333")

# Create the plot
ggplot(df, aes(x = week)) +
  # geom_ribbon(aes(ymin = age_q_0.05, ymax = age_q_0.25, fill = "0.05-0.25"), alpha = 0.8, colour = NA) +
  # geom_ribbon(aes(ymin = age_q_0.25, ymax = age_q_0.5, fill = "0.25-0.5"), alpha = 0.8, colour = NA) +
  # geom_ribbon(aes(ymin = age_q_0.5, ymax = age_q_0.75, fill = "0.5-0.75"), alpha = 0.8, colour = NA) +
  # geom_ribbon(aes(ymin = age_q_0.75, ymax = age_q_0.95, fill = "0.75-0.95"), alpha = 0.8, colour = NA) +
  geom_line(aes(y = age_q_0.05), colour = "blue", linewidth = 1.1) +
  geom_line(aes(y = age_q_0.25), colour = "green", linewidth = 1.1) +
  geom_line(aes(y = age_q_0.5), colour = "black", linewidth = 1.1) +
  geom_line(aes(y = age_q_0.75), colour = "green", linewidth = 1.1) +
  geom_line(aes(y = age_q_0.95), colour = "blue", linewidth = 1.1) +
  scale_fill_manual(values = colours, guide = FALSE) +
  scale_x_continuous(name = "week", breaks = c(1,seq(4,53,by=4))) +
  theme_minimal() +
  labs(x = "Week", y = "Age") +
  ggtitle("Age Quantiles by Week") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(df, aes(x = week)) +
  geom_line(aes(y = mean.temperature), colour = "black") +  
  scale_x_continuous(name = "week", breaks = c(1,seq(4,53,by=4))) +
  scale_fill_manual(values = colours, guide = FALSE) +
  theme_minimal() +
  labs(x = "Week", y = "Temperature in °C") +
  ggtitle("Temperature by Week") +
  theme(plot.title = element_text(hjust = 0.5)) 
