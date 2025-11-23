library(stringr)
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

set.seed(123)
housing_sample <- housing_data[sample(nrow(housing_data), 50000), ]

#fit the model
model <- lm(AssessedValue ~ Year.Property.Built + Lot.Area*Number.of.Units + Lot.Area*Number.of.Stories + Number.of.Bedrooms + Number.of.Rooms, data=housing_sample)
