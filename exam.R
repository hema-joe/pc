
#install.packages("tidyverse")
library(tidyverse)
#install.packages("psych")
library(psych)
#install.packages("survival")
library(survival)
#install.packages("survminer")
library(survminer)
#install.packages("mfp")
library(mfp)
#install.packages("lubridate")
library(lubridate)
#install.packages("rpart")
library(rpart)
#install.packages("caret")
library(caret)
#install.packages("plot_ly")
library(plot_ly)
#install.packages("pROC")
library(pROC)
#install.packages("readxl")
library(readxl)
#install.packages("rpart")
library(rpart)

Pharmacy_Council_Dec_2023_Results <- read_excel("Pharmacy Council Dec 2023 Results.xlsx")
print(Pharmacy_Council_Dec_2023_Results)

pc <- Pharmacy_Council_Dec_2023_Results

#Check missing values

missing_values <- sum(is.na(pc))
missing_values

# Check for missing values in each column
missing_values_per_column <- colSums(is.na(pc))

missing_values_per_column

# Print the results
cat("Total missing values in the dataset:", missing_values, "\n")
cat("Missing values per column:\n")

print(missing_values_per_column)


str(pc)
summary(pc)
col(pc)
row(pc)
ncol(pc)
nrow(pc)

pc

pc <- pc %>% select(`Examination Number...3`, `Examination Appearance`, RESULTS)

pc <- pc %>% rename("University" = "Examination Number...3", "Results" = "RESULTS")

pc

#Shorten and Summarize the University names

abbreviate_university <- function(full_name) {
  # Add more cases as needed
  case_when(
    grepl("Wenzhou Medical University", full_name) ~ "Wenzhou",
    grepl("St. Johns University of Tanzania", full_name) ~ "SJUT",
    grepl("University of Algiers", full_name) ~ "Algiers",
    grepl("Muhimbili University of Health and Allied Sciences", full_name) ~ "MUHAS",
    grepl("Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl("Kampala International University", full_name) ~ "KIU",
    grepl("St. John’s University of Tanzania", full_name) ~ "SJUT",
    grepl("St Johns University of Tanzania", full_name) ~ "SJUT",
    grepl("Kampala International University (western Campus)-uganda", full_name) ~ "Kampala Uganda",
    grepl("Catholic University of Health and Allied Sciences", full_name) ~ "CUHAS",
    grepl("China Three Gorges University", full_name) ~ "China Three",
    grepl("China Pharmaceutical University-china", full_name) ~ "China",
    grepl("Itm University – Gwalior", full_name) ~ "ITM India",
    grepl("Maharishi Markandeshwar University-india", full_name) ~ "Maharishi India",
    grepl("Guru Kashi University- Punjab India", full_name) ~ "Guru India",
    grepl("Zhengzhou University-china", full_name) ~ "Zhengzhou",
    grepl("Kampala International University in Tanzania", full_name) ~ "KIU",
    grepl("Adesh University, Bathinda, Punjab-india", full_name) ~ "Adesh India",
    grepl("Wenzhou Medical University-china", full_name) ~ "Wenzhou",
    grepl("Lovely Professional University-india", full_name) ~ "LPU India",
    grepl("Ct University-india", full_name) ~ "Ct India",
    grepl("Volgograd State Medical University", full_name) ~ "Volgograd",
    grepl("Jinzhou Medical University-china", full_name) ~ "Jinzhou",
    grepl("Maharishi Markandeshwar University", full_name) ~ "Maharishi India",
    grepl("Tula State Pedagogical University-russia", full_name) ~ "Tula Russia",
    grepl("Kampala International University (western campus)-Uganda", full_name) ~ "WMU",
    grepl("Mount Kenya University-kenya", full_name) ~ "Mount Kenya",
    grepl("Suresh Gyan Vihar University", full_name) ~ "Suresh",
    grepl("University of Constantine 3, Algeria", full_name) ~ "Constantine Algeria",
    grepl("Shenyang Pharmaceutical University", full_name) ~ "Shenyang",
    grepl("China Pharmaceutical University", full_name) ~ "WMU",
    grepl("Institute of Technology and Development (itm)-india", full_name) ~ "ITM India",
    grepl("Galgotias Univeristy-india", full_name) ~ "Galgotias India",
    grepl("United States International University-africa (kenya)", full_name) ~ "USI Kenya",
    grepl("Zhengzhou University of Industrial Technology-china", full_name) ~ "Zhengzhou",
    grepl("University of Algiers", full_name) ~ "Algiers",
    grepl("Sharda University-india", full_name) ~ "Sharda",
    grepl("Guru-rashi University", full_name) ~ "Guru-rashi",
    grepl("Manipal Academy of Higher Education (manipal College of Pharmaceutical Sciences)", full_name) ~ "Manipal Academy",
    grepl("ITM University - Gwalior", full_name) ~ "ITM India",
    grepl("Rayat Bahra University-india", full_name) ~ "Rayat Bahra",
    grepl("Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl("Muhimbili College of Health and Allied Sciences", full_name) ~ "MUHAS",
    grepl("United state international university of Kenya", full_name) ~ "SIU Kenya",
    grepl("University of Nairobi-kenya", full_name) ~ "Nairobi",
    grepl("United States International University-africa (kenya)", full_name) ~ "SIU Kenya",
    grepl("Institute of Technology and Development (itm)-india", full_name) ~ "ITM India",
    grepl("Itm University - Gwalior", full_name) ~ "ITM India",
    grepl("Jawaharial Nehru Technological University Hyderabad-India", full_name) ~ "Jawaharial",
    grepl("Jinzhou", full_name) ~ "Jinzhou",
    grepl(" Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl("Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl(" Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl(" Koneru Lakshmaiah Education Foundation (deemed to Be University)-india", full_name) ~ "Koneru India",
    grepl(" Institute of Technology and Development (itm)-india", full_name) ~ "ITM India",
    grepl("United States International University-africa (kenya)", full_name) ~ "Koneru SIU Kenya",
    grepl("Manipal Academy of Higher Education (manipal College of Pharmaceutical Sciences)", full_name) ~ "Manipal Academy",
    TRUE ~ full_name  # Keep the original name if no match is found
  )
}


# Create a new column with abbreviated university names

pc$University <- sapply(pc$University, abbreviate_university)

# Shorten the longer rows to two words maximum
pc$University <- sapply(pc$University, function(full_name) {
  words <- unlist(strsplit(full_name, " "))
  if (length(words) > 2) {
    shortened_name <- paste(words[1:2], collapse = " ")
  } else {
    shortened_name <- full_name
  }
  return(shortened_name)
})

#Create a new column called Education Indicating whether the candidate studied
#Locally or Foreign

pc <- pc %>%
  mutate(Education = ifelse(University %in% c("SJUT","MUHAS", "KIU", "CUHAS"), "Local", "Foreign"))


unique (pc$University)
unique(pc$Education)
unique(pc$Results)
unique(pc$`Examination Appearance`)


#Categorise all "SUPP" results in one group
pc$Results[grepl("SUPP", pc$Results)] <- "SUPP"


table(pc$University)
table(pc$Education)
table(pc$Results)
table(pc$`Examination Appearance`)

#How many absentees?
sum(pc$Results == "ABS")

#Remove Absentees columns

pc <- pc %>%
  filter(Results != "ABS")

#Recheck
table(pc$Results)


#Visualizing Distribution

# Create a bar plot for RESULTS

#Alternative 1
ggplot(pc, aes(x = Results)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "Examination Results Distribution", x = "Results", y = "Frequency")

#Alternative 2
ggplot(pc, aes(x = Results)) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Examination Results Distribution", x = "Results", y = "Frequency")

#Alternative 3
ggplot(pc, aes(x = Results)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black", size = 0.7) +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Examination Results Distribution", x = "Results", y = "Frequency") +
  theme_minimal() +  # You can change the theme if needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Create a bar plot for UNIVERSITIES

#Alternative 1
ggplot(pc, aes(x = University)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "University Distribution", x = "University", y = "Frequency")

#Visualise 5 highest and group the rest
pc$University_grouped <- ifelse(pc$University %in% names(sort(table(pc$University),
                                decreasing = TRUE)[1:5]), pc$University, "Other Universities")

# Create a bar plot with the grouped universities
#Alternative 1
ggplot(pc, aes(x = University_grouped)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "University Distribution", x = "University", y = "Frequency") +
  scale_x_discrete(labels = function(x) ifelse(x == "Other Universities", "Other Universities", x))

#Alternative 2
ggplot(pc, aes(x = University_grouped)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black", size = 0.7) +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "University Distribution", x = "University", y = "Frequency") +
  scale_x_discrete(labels = function(x) ifelse(x == "Other Universities", "Other Universities", x)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  # Rotate x-axis labels for better readability



# Create a bar plot for EXAMINATION APPEARANCES

ggplot(pc, aes(x = `Examination Appearance`)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "Examination Appearance Distribution", x = "Examination Appearance", y = "Frequency")

#Alternative 2
ggplot(pc, aes(x = `Examination Appearance`)) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Examination Appearance Distribution", x = "Examination Appearance", y = "Frequency")

#Alternative 3
ggplot(pc, aes(x = `Examination Appearance`)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black", size = 0.7) +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Examination Appearance Distribution", x = "Examination Appearance", y = "Frequency") +
  theme_minimal() +  # You can change the theme if needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Create a bar plot for Education

ggplot(pc, aes(x = Education)) +
  geom_bar(stat = "count", fill = "skyblue") +
  labs(title = "Education Background Distribution", x = "Education", y = "Frequency")

#Alternative 2
ggplot(pc, aes(x = Education)) +
  geom_bar(stat = "count", fill = "skyblue") +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "Education Background Distribution", x = "Education", y = "Frequency")

#Alternative 3
ggplot(pc, aes(x = Education)) +
  geom_bar(stat = "count", fill = "skyblue", color = "black", size = 0.7) +
  geom_text(stat = "count", aes(label = scales::percent(..count../sum(..count..))), 
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  labs(title = "Education Background Distribution", x = "Education", y = "Frequency") +
  theme_minimal() +  # You can change the theme if needed
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#Examination appearance vs Results
pc %>%
  group_by(`Examination Appearance`) %>%
  summarise(PASS_proportion = mean(Results == "PASS"))


ggplot(pc, aes(x = `Examination Appearance`, fill = Results)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of PASS by Examination appearance")


#University vs Results

pc %>%
  group_by(University) %>%
  summarise(PASS_proportion = mean(Results == "PASS"))


ggplot(pc, aes(x = University, fill = Results)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of PASS by Examination appearance")

#Group Universities
pc$University_grouped <- ifelse(pc$University %in% names(sort(table(pc$University),
                                                              decreasing = TRUE)[1:5]),
                                pc$University, "Other Universities")

ggplot(pc, aes(x = University_grouped, fill = Results)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of PASS by University")

#Examination appearance vs Results
pc %>%
  group_by(Education) %>%
  summarise(PASS_proportion = mean(Results == "PASS"))


ggplot(pc, aes(x = Education, fill = Results)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of PASS by Education Background")


#Logistic Regression
#Create binary columns
pc_1 <- pc %>%
  mutate(University = as.integer(University == "MUHAS"))

pc_1

pc_1 <- pc_1 %>%
  mutate(Education = as.integer(Education == "Local"))

pc_1

pc_1 <- pc_1 %>%
  mutate(Results = as.integer(Results == "PASS"))

pc_1

pc_1 <- pc_1 %>%
  mutate(`Examination Appearance` = as.integer(`Examination Appearance` == "First time"))

pc_1

pc_1$University_grouped <- NULL

pc_1


#Change the variables names
pc_1$MUHAS <- pc_1$University 
pc_1$`First Timer` <- pc_1$`Examination Appearance`
pc_1$Pass <- pc_1$Results
pc_1$Local <- pc_1$Education

pc_1 <- pc_1 %>% select(MUHAS, `First Timer`, Pass, Local)

pc_1

str(pc_1)

summary(pc_1$Pass)

table(pc_1$MUHAS)
table(pc_1$`First Timer`)
table(pc_1$Pass)
table(pc_1$Local)


#MUHAS?
table(pc_1$MUHAS, pc_1$Pass)

#Visualize

#Alternative 1
# Assuming "MUHAS" and "Pass" are binary variables (0 or 1)
ggplot(pc_1, aes(x = factor(MUHAS), fill = as.factor(Pass))) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of Pass for MUHAS vs Non-MUHAS candidates",
       x = "MUHAS",
       y = "Proportion") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))  # Customizing fill colors


#Alternative 2

# Assuming "MUHAS" and "Pass" are binary variables (0 or 1)
pc_1$MUHAS_category <- ifelse(pc_1$MUHAS == 1, "MUHAS", "Non-MUHAS")
pc_1$Pass_category <- ifelse(pc_1$Pass == 1, "Pass", "Not Pass")

ggplot(pc_1, aes(x = MUHAS_category, fill = Pass_category)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Proportion of Pass for MUHAS vs Non-MUHAS candidates",
       x = "MUHAS",
       y = "Proportion") +
  scale_fill_manual(values = c("Not Pass" = "blue", "Pass" = "red"))  # Customizing fill colors


#Alternative 3

# Assuming "MUHAS" and "Pass" are binary variables (0 or 1)
pc_1$MUHAS_category <- ifelse(pc_1$MUHAS == 1, "MUHAS", "Non-MUHAS")
pc_1$Pass_category <- ifelse(pc_1$Pass == 1, "Pass", "Not Pass")

ggplot(pc_1, aes(x = MUHAS_category, fill = Pass_category)) +
  geom_bar(position = "fill", stat = "count", color = "black", alpha = 0.7) +  # Adding black border and transparency
  labs(title = "Proportion of PASS for MUHAS vs Non-MUHAS candidates",
       x = "MUHAS",
       y = "Proportion") +
  scale_fill_manual(values = c("Not Pass" = "skyblue", "Pass" = "salmon"),  # Adjusting fill colors
                    labels = c("Not Pass", "Pass")) +
  theme_minimal() +  # Using a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels
        legend.position = "bottom",  # Moving legend to the bottom
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Removing grid lines

#FIRST TIME?
table(pc_1$`First Timer`, pc_1$Pass)
#Visualize

# Assuming "First Timer" and "Pass" are binary variables (0 or 1)
pc_1$First_Timer_category <- ifelse(pc_1$`First Timer` == 1, "First Timer", "Nth Timer")
pc_1$Pass_category <- ifelse(pc_1$Pass == 1, "Pass", "Not Pass")

ggplot(pc_1, aes(x = First_Timer_category, fill = Pass_category)) +
  geom_bar(position = "fill", stat = "count", color = "black", alpha = 0.7) +  # Adding black border and transparency
  labs(title = "Proportion of PASS for First Timer vs Nth Timers",
       x = "First Timer",
       y = "Proportion") +
  scale_fill_manual(values = c("Not Pass" = "skyblue", "Pass" = "salmon"),  # Adjusting fill colors
                    labels = c("Not Pass", "Pass")) +
  theme_minimal() +  # Using a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels
        legend.position = "bottom",  # Moving legend to the bottom
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Removing grid lines

#LOCAL?
table(pc_1$Local, pc_1$Pass)
#Visualize

# Assuming "First Timer" and "Pass" are binary variables (0 or 1)
pc_1$Local_category <- ifelse(pc_1$Local == 1, "Local", "Foreign")
pc_1$Pass_category <- ifelse(pc_1$Pass == 1, "Pass", "Not Pass")

ggplot(pc_1, aes(x = Local_category, fill = Pass_category)) +
  geom_bar(position = "fill", stat = "count", color = "black", alpha = 0.7) +  # Adding black border and transparency
  labs(title = "Proportion of PASS for Locally Educated vs Foreign Eduacated candidates",
       x = "Local",
       y = "Proportion") +
  scale_fill_manual(values = c("Not Pass" = "skyblue", "Pass" = "salmon"),  # Adjusting fill colors
                    labels = c("Not Pass", "Pass")) +
  theme_minimal() +  # Using a minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotating x-axis labels
        legend.position = "bottom",  # Moving legend to the bottom
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Removing grid lines


#Restore the columns
pc_1 <- pc_1 %>% select(MUHAS, `First Timer`, Pass, Local)

pc_1

#Data Partition
set.seed(1234)

index <- createDataPartition(pc_1$Pass, p = 0.8, list = FALSE)

# Create training and test sets
train_data <- pc_1[index, ]
test_data <- pc_1[-index, ]

mymodel <- glm(Pass~., data = pc_1, family = "binomial")

mymodel

summary(mymodel)

plot(mymodel)
 
#University (MUHAS) is statistical significant, drop Examination appearance (First Timer)

mymodel2 <- glm(Pass~MUHAS + Local, data = pc_1, family = "binomial")

mymodel2

summary(mymodel2)

plot(mymodel2)

#Check for Multicollinearity (Variance Inflation Factor (VIF))
install.packages("car")
library(car)

# Calculate VIF for each predictor variable
vif_values <- car::vif(mymodel2)

# Print the VIF values
print(vif_values)

#Model Performance
# Predict probabilities using the logistic regression model
predicted_probabilities <- predict (mymodel2, pc_1, type = "response")
predicted_probabilities

# Convert predicted probabilities to class predictions
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
predicted_classes

# Create a confusion matrix
conf_matrix <- table(Actual = pc_1$Pass, Predicted = predicted_classes)

conf_matrix

#Accuracy, Precision and Recall

# Function to calculate accuracy
accuracy <- function(conf_matrix) {
  sum(diag(conf_matrix)) / sum(conf_matrix)
}


precision <- function(conf_matrix) {
  conf_matrix[2, 2] / sum(conf_matrix[, 2])
}

# Function to calculate recall
recall <- function(conf_matrix) {
  conf_matrix[2, 2] / sum(conf_matrix[2, ])
}

# Calculate metrics
accuracy_value <- accuracy(conf_matrix)
precision_value <- precision(conf_matrix)
recall_value <- recall(conf_matrix)

# Print results
cat("Accuracy:", accuracy_value, "\n")
cat("Precision:", precision_value, "\n")
cat("Recall:", recall_value, "\n")


#ROC analysis


# Predict probabilities using the logistic regression model
predicted_probabilities <- predict (mymodel2, pc_1, type = "response")

# Create a ROC curve
roc_curve <- roc(pc_1$Pass, predicted_probabilities)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add labels and legend
xlabel <- "False Positive Rate (1 - Specificity)"
ylabel <- "True Positive Rate (Sensitivity)"
title <- "Receiver Operating Characteristic (ROC) Curve"
legend("bottomright", legend = paste("AUC =", round(auc(roc_curve), 3)), col = "blue", lty = 1, cex = 0.8)

# You can customize the appearance of the plot further if needed

#Decision Tree Analysis

model_tree <- rpart (Pass~., data = pc_1, method = "class",
                  control = rpart.control(minsplit = 1, minbucket = 1, cp= 0))

model_tree

plot(model_tree)

text(model_tree,digits = 3, cex= 0.6,  xpd = TRUE, use.n = TRUE)
title(main = "Decision Tree", sub = "xxx", font.main = 1, cex.main = 1.2)

#Alternative 1

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(model_tree, extra = 104, under = TRUE, fallen.leaves = TRUE, main = "Decision Tree")

#Alternative 2

# Assuming you have a decision tree model called "model_tree"

install.packages("partykit")
library(partykit)

# Convert the rpart object to a party object
party_tree <- as.party(model_tree)

# Plot the fancy decision tree
plot(party_tree, type = "simple", main = "Decision Tree")


#Alternative 3

# Create a decision tree model
model_tree <- rpart(Pass ~ ., data = pc_1, method = "class",
                    control = rpart.control(minsplit = 1, minbucket = 1, cp = 0))

# Plot the decision tree with enhanced aesthetics
rpart.plot(model_tree,
           extra = 104,               # Display percentage of observations in each node
           under = TRUE,              # Display node statistics
           fallen.leaves = TRUE,      # Display fallen leaves
           branch.lty = 2,            # Set branch line type
           branch.lwd = 2,            # Set branch line width
           shadow.col = "gray",       # Add a shadow effect
           nn = TRUE,                 # Display node numbers
           box.palette = "Reds",      # Set color palette for boxes
           main = "Decision Tree",    # Set main title
           sub = "xxx",               # Set subtitle
           cex.main = 1.2,            # Set main title size
           cex.sub = 0.8,             # Set subtitle size
           split.cex = 0.8,           # Set split text size
           
)



