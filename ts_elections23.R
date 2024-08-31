ts<-read.csv("D:/VISUALISATION_WORKSHOP/tselection/DATASET/Telengana_Assembly_election_2023.csv")
str(ts)

# Aggregate total votes by Constituency
total_votes_by_constituency <- aggregate(Total.Votes ~ Constituency, data = ts, sum)

# Print the result
print(total_votes_by_constituency)


# Aggregate total votes by Constituency
total_votes_by_constituency <- aggregate(Total.Votes ~ Constituency, data = ts, sum)

# Find the row index of the constituency with the highest total votes
index_max_votes <- which.max(total_votes_by_constituency$Total.Votes)

# Get the constituency name with the highest total votes
constituency_max_votes <- total_votes_by_constituency$Constituency[index_max_votes]

# Get the highest total votes
highest_votes <- total_votes_by_constituency$Total.Votes[index_max_votes]

# Print the constituency with the highest total votes
cat("Constituency with the highest total votes:", constituency_max_votes, "\n")
cat("Total votes in the constituency:", highest_votes, "\n")

# Aggregate total votes by Constituency
total_votes_by_constituency <- aggregate(Total.Votes ~ Constituency, data = ts, sum)

# Find the row index of the constituency with the lowest total votes
index_min_votes <- which.min(total_votes_by_constituency$Total.Votes)

# Get the constituency name with the lowest total votes
constituency_min_votes <- total_votes_by_constituency$Constituency[index_min_votes]

# Get the lowest total votes
lowest_votes <- total_votes_by_constituency$Total.Votes[index_min_votes]

# Print the constituency with the lowest total votes
cat("Constituency with the lowest total votes:", constituency_min_votes, "\n")
cat("Total votes in the constituency:", lowest_votes, "\n")



# Aggregate total votes by Party
total_votes_by_party <- aggregate(Total.Votes ~ Party, data = ts, sum)

# Find the row index of the party with the highest total votes
index_max_votes_party <- which.max(total_votes_by_party$Total.Votes)

# Get the party name with the highest total votes
party_max_votes <- total_votes_by_party$Party[index_max_votes_party]

# Get the highest total votes
highest_votes_party <- total_votes_by_party$Total.Votes[index_max_votes_party]

# Print the party with the highest total votes
cat("Party with the highest total votes:", party_max_votes, "\n")
cat("Total votes for the party:", highest_votes_party, "\n")



# Find the row index of the candidate with the highest total votes
index_max_votes <- which.max(ts$Total.Votes)

# Get the candidate's name with the highest total votes
candidate_max_votes <- ts$Candidate[index_max_votes]

# Get the party associated with the candidate with the highest total votes
party_max_votes <- ts$Party[index_max_votes]

# Get the highest total votes
highest_votes <- ts$Total.Votes[index_max_votes]

# Print the candidate's name and party with the highest total votes
cat("Candidate with the highest total votes:", candidate_max_votes, "\n")
cat("Party:", party_max_votes, "\n")
cat("Total votes for the candidate:", highest_votes, "\n")




# Filter the dataset for Bharat Rashtra Samithi party
brs_data <- ts[ts$Party == "Bharat Rashtra Samithi", ]

# Find the row index of the candidate with the lowest total votes
index_min_votes <- which.min(brs_data$Total.Votes)

# Get the candidate's name with the lowest total votes
candidate_min_votes <- brs_data$Candidate[index_min_votes]

# Get the constituency for the candidate with the lowest total votes
constituency_min_votes <- brs_data$Constituency[index_min_votes]

# Get the total votes for the candidate with the lowest total votes
lowest_votes <- brs_data$Total.Votes[index_min_votes]

# Print the candidate's name, constituency, and total votes
cat("Candidate with the lowest votes in Bharat Rashtra Samithi:", candidate_min_votes, "\n")
cat("Constituency:", constituency_min_votes, "\n")
cat("Total votes for the candidate:", lowest_votes, "\n")


# Sort the dataframe by Total.Votes in descending order
sorted_data <- ts[order(ts$Total.Votes, decreasing = TRUE), ]

# Extract the top 5 rows
top_5 <- head(sorted_data, 5)

# Create a pie chart
library(ggplot2)

# Plot the pie chart
pie_chart <- ggplot(top_5, aes(x = "", y = Total.Votes, fill = Candidate)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Top 5 Candidates with Highest Votes",
       fill = "Candidate") +
  theme_minimal()

# Display the pie chart
print(pie_chart)




# Subset the data to include only independent candidates
independent_candidates <- ts[ts$Party == "Independent", ]

# Sort the subsetted data by Total.Votes in descending order
sorted_data <- independent_candidates[order(independent_candidates$Total.Votes, decreasing = TRUE), ]

# Extract the top 10 rows
top_10 <- head(sorted_data, 10)

# Create a bar chart
library(ggplot2)

# Plot the bar chart
bar_chart <- ggplot(top_10, aes(x = reorder(Candidate, Total.Votes), y = Total.Votes, fill = Constituency)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Independent Candidates with Highest Votes",
       x = "Candidate",
       y = "Total Votes",
       fill = "Constituency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the bar chart
print(bar_chart)







