library(stringr)
library(dplyr)   # for %>% and summarise()
library(knitr)   # for kable() to format tables
#Read in CSV
housing_data <- read.csv("Assessor_Historical_Secured_Property_Tax_Rolls_20251122.csv")

#Convert units to numeric
housing_data$Number.of.Units <- as.numeric(housing_data$Number.of.Units)

# Remove commas or symbols, then convert to numeric
housing_data$Assessed.Fixtures.Value <- as.numeric(gsub("[$,]", "", housing_data$Assessed.Fixtures.Value))
housing_data$Assessed.Improvement.Value <- as.numeric(gsub("[$,]", "", housing_data$Assessed.Improvement.Value))
housing_data$Assessed.Land.Value <- as.numeric(gsub("[$,]", "", housing_data$Assessed.Land.Value))
housing_data$Assessed.Personal.Property.Value <- as.numeric(gsub("[$,]", "", housing_data$Assessed.Personal.Property.Value))

housing_data$AssessedValue <- rowSums(housing_data[, c("Assessed.Fixtures.Value", 
                                                       "Assessed.Improvement.Value", 
                                                       "Assessed.Land.Value", 
                                                       "Assessed.Personal.Property.Value")],
                                      na.rm = TRUE)

#filter only useful data


#convert into numeric
housing_data$Lot.Area <- housing_data$Lot.Area |>
  as.character() |>                # ensure it's character, not factor
  str_trim() |>                    # remove leading/trailing spaces
  str_replace_all("[^0-9.]", "") |> # remove everything that's not a digit or dot
  as.numeric()

housing_data$Number.of.Bedrooms <- housing_data$Number.of.Bedrooms |>
  as.character() |>
  str_trim() |>
  str_replace_all("[^0-9.]", "") |>
  as.numeric()

housing_data$Number.of.Rooms <- housing_data$Number.of.Rooms |>
  as.character() |>
  str_trim() |>
  str_replace_all("[^0-9.]", "") |>
  as.numeric()

# we only want residential
housing_data <- subset(housing_data, Use.Code %in% c("MRES", "SRES"))

#drop rows with missing data
housing_data <- subset(housing_data, Lot.Area > 0 & !is.na(Lot.Area) & !is.na(Number.of.Units) & !is.na(Number.of.Stories) & !is.na(Number.of.Bedrooms) & !is.na(Number.of.Rooms))


#fit the model
model <- lm(AssessedValue ~ Lot.Area + Lot.Area:Number.of.Units + Lot.Area:Number.of.Stories, data=housing_data)
model_log <- lm(log1p(AssessedValue) ~ Lot.Area + Lot.Area:Number.of.Units + Lot.Area:Number.of.Stories, data=housing_data)

#create summary statistics
housing_data %>%
  summarise(
    Mean_Assessed_Value   = mean(AssessedValue, na.rm = TRUE),
    Assessed_Value_SD     = sd(AssessedValue, na.rm = TRUE),
    MinAssessedValue    = min(AssessedValue, na.rm = TRUE),
    MaxAssessedValue    = max(AssessedValue, na.rm = TRUE),
    Mean_LotArea = mean(Lot.Area, na.rm = TRUE),
    LotArea_SD   = sd(Lot.Area, na.rm = TRUE),
    Min_LotArea  = min(Lot.Area, na.rm = TRUE),
    Max_LotArea  = max(Lot.Area, na.rm = TRUE),
    Mean_NumberOfUnits = mean(Number.of.Units, na.rm= TRUE),
    NumberOfUnits_SD = sd(Number.of.Units, na.rm=TRUE),
    Min_NumberOfUnits = min(Number.of.Units, na.rm=TRUE),
    Max_NumberOfUnits = max(Number.of.Units, na.rm=TRUE),
    Mean_NumberOfStories = mean(Number.of.Stories, na.rm=TRUE),
    NumberOfStories_SD = sd(Number.of.Stories, na.rm=TRUE),
    Min_NumberOfStories = min(Number.of.Stories, na.rm=TRUE),
    Max_NumberOfStores = max(Number.of.Stories, na.rm=TRUE)
  ) %>%
  t() %>%
  kable(col.names = c("Value"))

#residual vs. fitted values
plot(model, which=1)

#residual vs. fitted values log
plot(model_log, which=1)
