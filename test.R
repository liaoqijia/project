
library(writexl)
library(tidyverse)
library(janitor)
# URL for CSV export
sheet_url <- "https://docs.google.com/spreadsheets/d/14QC-5jJ651nSW0OY0kb_YxnTZyugxNyz_8wa2KDmKs0/export?format=csv&gid=1009489028"

# Read the sheet into a data frame
data <- read.csv(sheet_url)
colnames(data) <- data[1,]

data_1 <- data[3:nrow(data),] %>%
  clean_names() %>%
  select(index:note_12, first_round_sent_percent,job, first_round, flyout) %>%
  filter(!continent %in% c( "AS", "OC", "EU", "SA", "AF")) %>%
  mutate(website = str_extract(link, "https://[^/]+") )


# Count the occurrences of each website and sort in descending order
website_counts <- data_1 %>%
  count(website) %>%
  arrange(desc(n)) # Sort by descending frequency

# Create the bar plot
ggplot(website_counts, aes(x = reorder(website, -n), y = n)) +
  geom_bar(stat = "identity", fill = "lightblue") + # Plot the bar chart
  geom_text(aes(label = n), vjust = -0.5) + # Add counts as text above the bars
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  labs(x = "Website", y = "Count", title = "Website Frequency Distribution")


glimpse(data_1)

# View the first few rows
head(data_1)

links <- data_1$link

extracted_urls <- str_extract(links, "https://[^/]+")
filtered_links <- extracted_urls[str_detect(extracted_urls, "^https://[^/]+$") & !str_detect(extracted_urls, "\\.(ca|edu)$")]

unique(filtered_links)



###### teaching related works ############

data_1 <- data_1 %>%
  mutate(
    contains_teaching = str_detect(note_tenure_track_only_there_is_another_work_sheet_for_ntt, regex("teaching", ignore_case = TRUE)),
    contains_tenure_track = str_detect(note_tenure_track_only_there_is_another_work_sheet_for_ntt, regex("tenure[-]track", ignore_case = TRUE))
  )

# Count the occurrences where 'contains_teaching' is TRUE
teaching_count <- sum(data_1$contains_teaching)

# Count the occurrences where 'contains_tenure_track' is TRUE
tenure_track_count <- sum(data_1$contains_tenure_track)

# Print the results
cat("Number of rows containing 'teaching':", teaching_count, "\n")
cat("Number of rows containing 'tenure track' or 'tenure-track':", tenure_track_count, "\n")

universities_with_teaching <- data_1 %>%
  filter(contains_teaching == TRUE) %>%
  select(name)

# Filter rows where 'contains_tenure_track' is TRUE and select the 'name' column
universities_with_tenure_track <- data_1 %>%
  filter(contains_tenure_track == TRUE) %>%
  select(name)

# View the result
print(universities_with_tenure_track)




library(tidygeocoder)
library(tidyverse)



schools_geocoded <- data_1 %>%
  geocode(address = name, method = "arcgis", lat = latitude, long = longitude)


ggplot(schools_geocoded, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  theme_void()

write_xlsx(data_1, "schools_data.xlsx")

