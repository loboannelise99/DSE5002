library(stringr)
library(countrycode)

full_data = read.csv("R Project/data.csv", stringsAsFactors = FALSE)
cost_index_data = readxl::read_excel("R Project/cost_of_living.xlsx")

#add thhe cost of living data to the current dataset by merging on the country code variable
cost_index_data$employee_residence <- paste(countrycode(cost_index_data$Country, "country.name", "iso2c"))
total = merge(full_data, cost_index_data, by="employee_residence")

#Find salaries based on seniority level

#barplot(table(data$experience_level))
