#libraries to load for code
library(stringr)
library(countrycode)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)



#Data cleaning section

#Data to read into code; contains the data supplies as well as
#data regarding a countries cost of living
full_data = read.csv(file.choose(), stringsAsFactors = FALSE)
cost_index_data = readxl::read_excel(file.choose())

#Provide a complete data set by merging the two data sets together by the country code variable
cost_index_data$employee_residence <- paste(countrycode(cost_index_data$Country, "country.name", "iso2c"))
total_df = merge(full_data, cost_index_data, by="employee_residence")



#Data aggregation and summary statistics section

#Find salaries based on seniority level and company size
#Since your manager wants a senior level person and they are currently small but expanding
#Focus on senior level, medium size company 
Salary_SE_M = total_df %>%
  select(Cost_living_index, salary_in_usd, company_size, experience_level, company_location, employment_type) %>%
  filter(experience_level == c("SE") & employment_type == c("FT") & Cost_living_index <= "61") %>%
  filter(company_size == c("L", "M"))
6
aggregate(salary_in_usd ~ company_size + company_location, data = Salary_SE_M, range)

#Find the mean salary based on SE level experience for FT employees
avg_salary = total_df %>%
  select(salary_in_usd, company_size, experience_level, work_year, remote_ratio, employment_type) %>%
  filter(experience_level == c("SE") & employment_type == c("FT")) %>%
  group_by(work_year, remote_ratio, experience_level, company_size) %>%
  summarise(avg_salary_year = mean(salary_in_usd))

#break the salaries up by categorical groups 
salary_groups = total_df %>%
  mutate(salary_in_usd = case_when(salary_in_usd < 10000 ~ ">10k", 
                                   salary_in_usd >= 10000 & salary_in_usd <50000 ~ "10k-50k", 
                                   salary_in_usd >= 50000 & salary_in_usd <100000 ~ "50k-100k", 
                                   salary_in_usd >= 100000 & salary_in_usd <150000 ~ "100k-150k", 
                                   salary_in_usd >= 150000 & salary_in_usd <200000 ~ "150k-200k",
                                   salary_in_usd >= 200000 & salary_in_usd <300000 ~ "200k-300k",
                                   salary_in_usd >= 300000 ~ ">=300k",
                                   TRUE ~ "NA")) %>%
  select(salary_in_usd, company_size, experience_level, remote_ratio, Cost_living_index, job_title) %>%
  filter(experience_level == c("SE") & company_size == c("M") & job_title == c("Data Scientist"))


#Selecting company location to be the US, SE level experience, and FT worker and M company size
#summarized mean salary for each job group
salary_job_title = total_df %>%
  select(salary_in_usd, job_title, company_size, experience_level, company_location, employment_type) %>%
  filter(company_location == c("US") & experience_level == c("SE") & employment_type == c("FT") & company_size == c("M")) %>%
  group_by(job_title, company_size) %>%
  summarise(avg_by_job = mean(salary_in_usd, na.rm = TRUE))



#Data plots section 

#Scatterplot of salaries based on cost of living index, color coded by experience level
#You could argue that if your employee can work from where ever, the salary could be decreased 
#by the cost of living if they are located elsewhere
total_df %>%
  ggplot(aes(x = Cost_living_index, y = salary_in_usd))+
  geom_point(aes(shape = company_size, col = experience_level))+
  scale_y_log10(labels=scales::dollar_format(), breaks = c(0, 20000, 50000, 75000, 100000, 150000, 200000, 300000, 400000, 500000, 600000)) +
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90))+
  theme(legend.position="bottom") +                                                     
  guides(col = guide_legend("Experience Level"), shape = guide_legend("Company Size"))+
  labs(x='Cost of Living Index',
         y='Salary ($)',
         title='Salary Relation to Cost of Living Index')

#scatterplot copy of the previous plot of salaries based on cost of living
#This plot only shows the specifics of SE level data scientist and M, L companies
Salary_SE_M %>%
ggplot(aes(x = Cost_living_index, y = salary_in_usd))+
  geom_point(aes(shape = company_size, col = company_location))+
  scale_y_log10(labels=scales::dollar_format())+ #breaks = c(0, 25000, 50000, 75000, 100000, 150000, 200000, 300000, 400000, 500000, 600000)) +
  scale_x_continuous(breaks = c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90))+
  guides(color = guide_legend(title = "Country Code"))+
  labs(x='Cost of Living Index',
       y='Salary ($)',
       title='Salary Relation to Cost of Living Index')

#Box plot of SE level experience and how much they earn based on country
Salary_SE_M %>%
  ggplot(aes(company_location, salary_in_usd, fill = company_size)) +
  geom_boxplot(varwidth = T) +
  #facet_wrap(~company_size) +
  scale_y_log10(labels=scales::dollar_format())+
  labs(title="Mean Salary per Country",
       x="Company Location",
       y="Mean Salary in USD")

#boxplot of all experience levels and how much they earn
total_df %>%
  ggplot(aes(experience_level, salary_in_usd)) +
  geom_boxplot(varwidth=T, fill = "purple" ) +
  scale_y_log10(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(vjust=0.6)) +
  scale_x_discrete(labels = c("Entry Level", "Exective", "Mid-Level", "Senior-Level"))+
  labs(title="Salary as a Function of Experience Level",
       x="Experience Level",
       y="Salary in USD")

#barplot on type of data engineer and mean salaries
salary_job_title %>%
  ggplot(aes(x = job_title, y = avg_by_job)) +
  geom_col() +
  scale_y_log10(labels=scales::dollar_format())+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Mean Salary of Various Data Scientists",
       x="Data Scientest Type",
       y="Salary in USD")


#stacked bar plot showing average salary by year for all company sizes
#stack is the remote ratio from 0-100
avg_salary %>%
  ggplot(aes(x =work_year, y = avg_salary_year, fill = remote_ratio)) +
  geom_col() +
  facet_wrap(~company_size)+
  scale_y_continuous(labels=scales::dollar_format())+
  guides(color = guide_legend("Remote Ratio"))+
  labs(title="Average Salary as it Varries by Time",
        x="Work Year",
        y="Average Salary in USD")


#Does not work

#barplot(table(data$experience_level))
# labels and breaks for X axis text
salary_groups %>%
  ggplot(aes(x = salary_in_usd, fill = salary_in_usd)) +
  geom_bar() +
  labs(title="Mean Salary of Various Data Scientists",
       x="Data Scientest Type",
       y="Salary in USD")


