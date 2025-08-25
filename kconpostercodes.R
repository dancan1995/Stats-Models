# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the dataset
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


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define color
color <- "steelblue"

# Function to create a bar plot (descending order)
create_bar_plot <- function(df, title) {
  ggplot(df, aes(x = reorder(Role, -Frequency), y = Frequency)) +
    geom_bar(stat = "identity", fill = color, width = 0.9) +
    geom_text(aes(label = Frequency), vjust = -0.5, size = 4, fontface = "bold") +
    labs(title = title, x = "", y = "Frequency (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 10))
}

# Function to create a dot plot (descending order)
create_dot_plot <- function(df, title) {
  ggplot(df, aes(x = Frequency, y = reorder(Role, Frequency))) +
    geom_point(size = 5, color = color) +
    geom_text(aes(label = Frequency), hjust = -0.5, size = 4, fontface = "bold") +
    labs(title = title, x = "Frequency (%)", y = "") +
    theme_minimal() +
    theme(axis.text.y = element_text(face = "bold", size = 10))
}

# Function to create a lollipop chart (descending order)
create_lollipop_chart <- function(df, title) {
  ggplot(df, aes(x = reorder(Role, -Frequency), y = Frequency)) +
    geom_segment(aes(x = Role, xend = Role, y = 0, yend = Frequency), color = color, size = 1.2) +
    geom_point(size = 5, color = color) +
    geom_text(aes(label = Frequency), vjust = -0.5, size = 4, fontface = "bold") +
    labs(title = title, x = "", y = "Frequency (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, face = "bold", size = 10))
}

# Plot 1: Current APN Roles in Participant's Country (Bar Chart)
plot1 <- create_bar_plot(data %>% filter(Category == "Current APN Roles in Participant's Country"),
                         "Current APN Roles in Participant's Country")

# Plot 2: Participant's Highest Degree (Dot Plot)
plot2 <- create_bar_plot(data %>% filter(Category == "Participant's Highest Degree"),
                         "Participant's Highest Degree")

# Plot 3: Setting Currently Practiced by the Participant (Bar Chart)
plot3 <- create_bar_plot(data %>% filter(Category == "Setting Currently Practiced by the Participant"),
                         "Setting Currently Practiced by the Participant")

# Plot 4: Current APN Roles in Participant's Country (Bar Chart - Fixed Category Name)
plot4 <- create_bar_plot(data %>% filter(Category == "Current APN roles in participant's country"),
                         "Current APN Roles in Participant's Country")

# Plot 5: Current APN Roles Emerging in Participant's Country (Dot Plot)
plot5 <- create_bar_plot(data %>% filter(Category == "Current APN roles emerging in participant's country"),
                         "Emerging APN Roles in Participant's Country")

# Plot 6: Response on Additional Needs to Develop Advanced Nursing Roles (Bar Chart)
plot6 <- create_bar_plot(data %>% filter(Category == "Response on Additional Needs to Develop Advanced Nursing Roles"),
                         "Needs to Develop Advanced Nursing Roles")

# Plot 7: Desired Outcomes in Participant's Country in Order of Importance (Lollipop Chart - Fixed Category Name)
plot7 <- create_bar_plot(data %>% filter(Category == "Desired Outcomes in Participant's Country in Order of Importance"),
                               "Desired Outcomes in Participant's Country")

# Print each plot separately
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)
print(plot7)


