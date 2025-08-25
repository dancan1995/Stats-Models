# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

# Define color
color <- "steelblue"

# Create the dataset (same as previous code)
data <- data.frame(
  Category = c(
    rep("Current APN Roles in Participant's Country", 7),
    rep("Participant's Highest Degree", 7),
    rep("Setting Currently Practiced by the Participant", 10),
    rep("Current APN roles in participant's country", 8),
    rep("Current APN roles emerging in participant's country", 7),
    rep("Response on Additional Needs to Develop Advanced Nursing Roles", 3),
    rep("Desired Outcomes in Participant's Country in Order of Importance", 4)
  ),
  Role = c(
    "Nurse Practitioner", "Clinical Nurse Specialist", "Faculty", "Administrator", "Advanced Nurse Practitioner", "Student", "Chief Deputy Nurse",
    "Master’s of Science in Nursing", "Doctor of Nursing Practice", "Doctor of Philosophy", "Bachelor’s of Science in Nursing", "Master of Arts in Clinical Education", "Master’s of Science in Health Policy", "PhD Candidate",
    "Hospital/Acute Care", "Academia/University", "Primary Care", "Specialty Care", "Emergency/Urgent Care", "Mental Health/Psychiatric", "Policy", "Homeless Crisis Shelter", "Palliative Care", "Aging Services",
    "Nurse Practitioner", "Clinical Nurse Specialist", "Midwife", "Nurse Anesthetist", "Advanced Nurse Practitioner", "Nurse Consultant", "Advanced Clinical Practitioner", "Nursing counsellor",
    "Clinical Nurse Specialist", "Advanced Nurse Practitioner", "Nurse Practitioner", "None", "Nurse Anesthetist", "CNS Specialty", "Midwife",
    "Yes", "No", "Unsure",
    "Patients", "Population", "System", "Policy"
  ),
  Frequency = c(
    44.2, 23.3, 11.6, 7, 6.2, 4.7, 3.1,
    48.6, 18.9, 18.9, 5.4, 2.7, 2.7, 2.7,
    35, 15, 12.5, 7.5, 7.5, 7.5, 5, 2.5, 2.5, 2.5,
    30, 27.8, 23.3, 13.3, 3.2, 0.8, 0.8, 0.8,
    30.8, 30.8, 23.1, 23.1, 11.5, 7.7, 3.8,
    62.8, 20.9, 16.3,
    51.9, 25.9, 14.8, 7.4
  )
)

# Create a function to generate crosstabs between two variables
create_crosstab <- function(df, var1, var2) {
  # Generate a contingency table (crosstab) between the two variables
  crosstab <- table(df[[var1]], df[[var2]])
  
  # Melt the crosstab for plotting
  crosstab_melted <- melt(crosstab)
  
  return(crosstab_melted)
}

# Create crosstab for Category and Role
crosstab_category_role <- create_crosstab(data, "Category", "Role")

# Create crosstab for Category and Frequency (if needed as a numeric category)
crosstab_category_frequency <- create_crosstab(data, "Category", "Frequency")

# Create crosstab for Role and Frequency
crosstab_role_frequency <- create_crosstab(data, "Role", "Frequency")

# Plotting the crosstabs

# Plot for Category vs. Role (Heatmap)
ggplot(crosstab_category_role, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = color) +
  theme_minimal() +
  labs(title = "Crosstab: Category vs Role", x = "Category", y = "Role", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"))

# Plot for Category vs Frequency (Heatmap)
ggplot(crosstab_category_frequency, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = color) +
  theme_minimal() +
  labs(title = "Crosstab: Category vs Frequency", x = "Category", y = "Frequency", fill = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"))

# Plot for Role vs Frequency (Heatmap)
ggplot(crosstab_role_frequency, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = color) +
  theme_minimal() +
  labs(title = "Crosstab: Role vs Frequency", x = "Role", y = "Frequency", fill = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"))
