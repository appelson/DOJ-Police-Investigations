# Loading Libraries
library(xml2)
library(rvest)
library(tidyverse)
library(janitor)

# Reading DOJ Special Litigation HTML
html_doc <- read_html("Civil Rights Division _ Special Litigation Section Cases and Matters.html")

# Filtering for the XML children interested in
children <- xml_children(html_node(html_doc, ".field-formatter--text-default.field-text-format--wysiwyg.text-formatted.field_body"))

# Defining the H2 positions
h2_positions <- which(xml_name(children) == "h2")

# Getting all links from each section and state by looping through h2 and h3
result <- map(seq_along(h2_positions), function(i) {
  
  # Starting at an h2 position
  h2_start <- h2_positions[i]
  
  # Defining the h2 end position
  h2_end <- ifelse(i < length(h2_positions),
                   h2_positions[i + 1] - 1, 
                   length(children))
  
  # Defining the h2 section as the start + 1 to the end
  h2_section <- children[(h2_start + 1):h2_end]
  
  # Defining the h3 positions within the h2 position
  h3_positions <- which(xml_name(h2_section) == "h3")
  
  # Looping through all h3 positions within the h2
  map(seq_along(h3_positions), function(j) {
    
    # Defining the h3 starting position
    h3_start <- h3_positions[j]
    
    # Defining the j3 ending position
    h3_end <- ifelse(j < length(h3_positions),
                     h3_positions[j + 1] - 1,
                     length(h2_section))
    
    # Defining the h3 section
    h3_section <- h2_section[h3_start:h3_end]
    
    # Finding all <a> in the h3 section
    a_nodes <- xml_find_all(h3_section, ".//a")
    
    # Creating a tibble with the link and text from the h3 links
    tibble(href = xml_attr(a_nodes, "href"), text = xml_text(a_nodes))
  }) %>% set_names(html_text(h2_section[h3_positions]))
}) %>% set_names(html_text(children[h2_positions]))

# Turning the results into a dataframe
df_links <- imap_dfr(result, ~ {
  imap_dfr(.x, ~ {
    mutate(.x, h2 = .y, h3 = .y) %>% select(h2, h3, href, text)
  }) %>% mutate(h2 = .y)
}) %>%
  mutate(url_name = make_clean_names(paste0(h2," ",h3," ",text))) %>%
  mutate(download = 
           case_when(
             grepl("\\.pdf$", href) ~ "PDF",
             grepl("justice.gov/media/", href) | grepl("justice.gov/crt/media/", href) ~ "Media",
             grepl("justice.gov/crt/case-document", href) | grepl("justice.gov/case-document", href) ~ "Case Document",
             TRUE ~ NA
           )
  ) %>%
  filter(!str_detect(href,"casesummaries|special-litigation-section-case-summaries"))

# Writing the df_links to a csv
write_csv(df_links, "links.csv")

# Downloading the links
for (i in seq_len(nrow(df_links))) {
  
  # Defining the row
  row <- df_links[i, ]
  
  # Only continuing if the link is not empty
  if (is.na(row$href) || row$href == "") next
  
  # Defining the directory path
  dir_path <- file.path(row$h2, row$h3)
  
  # If the directory path does not exist, creating a directory path
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  
  # Creating an extension as either HTML or PDF 
  ext <- ifelse(grepl("\\.php$", row$href), "html", "pdf")
  
  # Adding file extension if non-existent
  file_name <- row$url_name
  
  # Creating file name
  if (!grepl(paste0("\\.", ext, "$"), file_name)) {
    file_name <- paste0(file_name, ".", ext)
  }
  
  # Creating file path
  file_path <- file.path(dir_path, file_name)
  
  # Downloading the file
  tryCatch({
    download.file(row$href, destfile = file_path)
    message("Downloaded: ", row$href, " -> ", file_path)
  }, error = function(e) {
    message("Failed to download ", row$href, ": ", e$message)
  })
}
