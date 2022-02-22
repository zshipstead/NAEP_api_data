# API information
# https://www.nationsreportcard.gov/api_documentation.aspx

library(httr)
library(jsonlite)

                          #State Level Query
# Query Variables
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", 
            "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", 
            "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

subjects <- c("reading", "mathematics")

years <- c("2019", "2009")


base_url <- "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"
type_url <- "?type=data"
#subject_url <- "&subject=reading"
grade_url <- "&grade=8"
variable_url <- "&variable=TOTAL"
#jurisdiction_url <- "&jurisdiction=AL"
stat_url <- "&stattype=MN:MN"
#year_url <- "&Year=2019"

state_data <- data.frame(matrix(nrow = 50, ncol = 5))
names(state_data) <- c("State", "Reading2019", "Reading2009", "Math2019", "Math2009")

# by State
for (i in 1:50){
  # by subject
  for (j in 1:2){
    current_line <- NULL
    # by year
    for (k in 1:2){
      jurisdiction_url <- paste0("&jurisdiction=", states[i])
      subject_url <- paste0("&subject=", subjects[j])
      year_url <- paste0("&Year=", years[k])
      full_url <- paste0(base_url, type_url, subject_url, grade_url, variable_url,
                         jurisdiction_url, stat_url, year_url)
      # Make call
      NAEP_call <- GET(full_url)
      # Convert to data frame
      NAEP_char <- rawToChar(NAEP_call$content)
      NAEP_JSON <- fromJSON(NAEP_char, flatten=TRUE)
      NAEP_sub_df <- NAEP_JSON$result
      # Fill data frame
      if ((j==1) && (k==1)) state_data[i,1] <- NAEP_sub_df[10]
      if ((k==1) && (j==1)) state_data[i,2] <- NAEP_sub_df[15]
      if ((k==2) && (j==1)) state_data[i,3] <- NAEP_sub_df[15]
      if ((k==1) && (j==2)) state_data[i,4] <- NAEP_sub_df[15]
      if ((k==2) && (j==2)) state_data[i,5] <- NAEP_sub_df[15]
      print(state_data[i,])
    }
  }
}


View(state_data)
setwd("~/PC Docs/R files/API/api test/NAEP")
write.csv(state_data, "1_api_pull.csv", row.names = FALSE)
state_data <- read.csv("1_api_pull.csv")




            # Verification Pulls

subject_url <- "&subject=mathematics"
jurisdiction_url <- "&jurisdiction=WY"
year_url <- "&Year=2009"

full_url <- paste0(base_url, type_url, subject_url, grade_url, variable_url,
                   jurisdiction_url, stat_url, year_url)

# Make call
NAEP_call <- GET(full_url)
# Convert to data frame
NAEP_char <- rawToChar(NAEP_call$content)
NAEP_JSON <- fromJSON(NAEP_char, flatten=TRUE)
NAEP_sub_df <- NAEP_JSON$result

NAEP_sub_df

            # Transformations

# Make Standardized Scores
state_data$Reading2019z <- as.numeric(scale(state_data$Reading2019)) 
state_data$Reading2009z <- as.numeric(scale(state_data$Reading2009))
state_data$Math2019z <- as.numeric(scale(state_data$Math2019))
state_data$Math2009z <- as.numeric(scale(state_data$Math2009))

# Make difference Scores
state_data$ReadDiffRaw <- state_data$Reading2019 - state_data$Reading2009 
state_data$MathDiffRaw <- state_data$Math2019 - state_data$Math2009

# Standardize the difference scores
state_data$ReadDiffZ <- as.numeric(scale(state_data$ReadDiffRaw))
state_data$MathDiffZ <- as.numeric(scale(state_data$MathDiffRaw))

# Rank scores
state_data$Reading_Rank_2019 <- rank(state_data$Reading2019)
state_data$Reading_Rank_2009 <- rank(state_data$Reading2009z)
state_data$Reading_Rank_Difference <- rank(state_data$ReadDiffZ)

state_data$Math_Rank_2019 <- rank(state_data$Math2019)
state_data$Math_Rank_2009 <- rank(state_data$Math2009z)
state_data$Math_Rank_Difference <- rank(state_data$MathDiffZ)



                # Basic analysis

head(state_data)

cor(state_data[-1])


library(psych)
describe(state_data[c(2,3,4,5)])
detach(package:psych, unload = TRUE)

write.csv(state_data, "2_state_data.csv", row.names = FALSE)
state_data <- read.csv("2_state_data.csv")

                # Stack data for Tableau

state_copy <- state_data

# Stack Reading data
colnames(state_copy)[c(6,7,12)] <- c("2019", "2009", "Difference")
state_stack1 <- cbind(state_copy[1], stack(state_copy[c(6,7,12)]))
names(state_stack1)[c(1,2,3)] <- c("State", "Reading_Score", "Category")

# Add Rank Scores
state_stack1 <- cbind(state_stack1, stack(state_copy[c(14,15,16)]))
names(state_stack1)[4] <- "Reading_Rank"
#


# Stack Math Data
colnames(state_copy)[c(8, 9, 13)] <- c("2019", "2009", "Difference")
state_stack2 <- cbind(state_copy[1], stack(state_copy[c(8, 9, 13)]))
names(state_stack2) <- c("State", "Math_Score", "Category")

# Add Ranks Scores
state_stack2 <- cbind(state_stack2, stack(state_copy[c(17,18,19)]))
names(state_stack2)[4] <- "Math_Rank"

# Merge Reading and Math
state_stack <- merge(state_stack1[-5], state_stack2[-5], by = c("State", "Category"), all.x = TRUE)


write.csv(state_stack, "3_Tableau_stack.csv", row.names = FALSE)




