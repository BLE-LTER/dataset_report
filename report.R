library(dplyr)

doi <- read.csv("knb-lter-ble_report.csv") %>%
  mutate(datasetid = as.integer(stringr::str_extract(Package.ID, "(?<=\\.)(.+)(?=\\.)"))) %>%
  mutate(rev = stringr::str_split(Package.ID, "\\.", simplify = T)[, 3]) %>%
  group_by(datasetid) %>%
  filter(rev == max(rev))

author <- read.csv("author.csv") %>%
  mutate(fullname = ifelse(givenname == "", organization, paste(givenname, givenname2, surname, sep = " "))) %>%
  mutate(role = ifelse(authorshiprole == "creator", "creator", "associate")) %>%
  select(datasetid, role, fullname) %>%
  group_by(datasetid, role) %>%
  mutate(associates = paste(fullname, collapse = ", ")) %>%
  select(-fullname) %>%
  distinct() %>%
  tidyr::pivot_wider(., id_cols = datasetid, names_from = role, values_from = associates)


key <- read.csv("keywords.csv") %>%
  add_row(datasetid = 7, keyword = "movement of inorganic matter") %>%
  add_row(datasetid = 10, keyword = "population dynamics") %>%
  add_row(datasetid = 19, keyword = "population dynamics") %>%
  group_by(datasetid) %>%
  mutate(keywords = paste(keyword, sep = ", ", collapse = ", ")) %>%
  select(-keyword) %>%
  distinct()

report <- full_join(doi, author, by = "datasetid") %>%
  full_join(., key, by = "datasetid") %>%
  select(datasetid, rev, Package.ID, Title, DOI, Upload.Date.Time, creator, associate, keywords) %>%
  rename(package_id = Package.ID) %>%
  rename(title = Title) %>%
  rename(upload_datetime = Upload.Date.Time) %>%
  filter(!is.na(title)) %>%
  arrange(datasetid) %>%
  ungroup %>%
  add_row(title = "Model estimates of dissolved organic carbon, runoff, snowmelt, and snow water equivalent from 1981-2010 across the western Arctic", DOI = "doi:10.15485/1809256", upload_datetime = "2021", creator = "Michael Rawlins, James McClelland, Craig Connolly") %>%
  add_row(title = "Beaufort Lagoon Ecosystem Long Term Ecological Research (BLE-LTER) shotgun metagenome sequences
", DOI = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA642637", upload_datetime = "2020-06-28", creator = "Byron Crump", keywords = "population dynamics") %>%
  add_row(title = "Beaufort Lagoon Ecosystem Long Term Ecological Research (BLE-LTER) PCR Amplicon sequences
", DOI = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA530074", upload_datetime = "2019-03-31", creator = "Byron Crump", keywords = "population dynamics")

write.csv(report, "dataset_report.csv")

# ---------------------------------------------------------------------------

library(EDIutils)

ids <- list_data_package_identifiers("knb-lter-ble")
revs <-
  sapply(ids, EDIutils::list_data_package_revisions, scope = "knb-lter-ble")
rev <- sapply(revs, max)
pkg_ids <- paste0("knb-lter-ble.", ids, ".", rev)

citations <- sapply(pkg_ids, read_data_package_citation, access = FALSE)

df <- data.frame(package_id = pkg_ids,
                 citation = citations,
                 stringsAsFactors = F)

key2 <- read.csv("keywords.csv") %>%
  add_row(datasetid = 7, keyword = "movement of inorganic matter") %>%
  add_row(datasetid = 10, keyword = "population dynamics") %>%
  add_row(datasetid = 19, keyword = "population dynamics") %>%
  mutate(keyword = recode(keyword, "primary production" = "A",
                  "population dynamics" = "B",
                  "movement of organic matter" = "C",
                  "movement of inorganic matter" = "D",
                  "disturbance patterns" = "E")) %>%
  arrange(keyword) %>%
  group_by(datasetid) %>%
  mutate(keywords = paste(keyword, sep = ", ", collapse = ", ")) %>%
  select(-keyword) %>%
  distinct()

report2 <- full_join(doi, author, by = "datasetid") %>%
  full_join(., key2, by = "datasetid") %>%
  select(datasetid, rev, Package.ID, Title, DOI, Upload.Date.Time, creator, associate, keywords) %>%
  rename(package_id = Package.ID) %>%
  rename(title = Title) %>%
  rename(upload_datetime = Upload.Date.Time) %>%
  filter(!is.na(title)) %>%
  arrange(datasetid) %>%
  ungroup %>%
  add_row(title = "Model estimates of dissolved organic carbon, runoff, snowmelt, and snow water equivalent from 1981-2010 across the western Arctic", DOI = "doi:10.15485/1809256", upload_datetime = "2021", creator = "Michael Rawlins, James McClelland, Craig Connolly") %>%
  add_row(title = "Beaufort Lagoon Ecosystem Long Term Ecological Research (BLE-LTER) shotgun metagenome sequences
", DOI = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA642637", upload_datetime = "2020-06-28", creator = "Byron Crump", keywords = "B") %>%
  add_row(title = "Beaufort Lagoon Ecosystem Long Term Ecological Research (BLE-LTER) PCR Amplicon sequences
", DOI = "https://www.ncbi.nlm.nih.gov/bioproject/PRJNA530074", upload_datetime = "2019-03-31", creator = "Byron Crump", keywords = "B")

df <- merge(df, report2[, c(3, 9)], by = "package_id")
df[nrow(df) + 1, ] <- c(NA, 'Rawlins M ; Connolly C ; McClelland J (2021): Model estimates of dissolved organic carbon, runoff, snowmelt, and snow water equivalent from 1981-2010 across the western Arctic. Quantifying Variability and Controls of Riverine Dissolved Organic Carbon Exported to Arctic Coastal Margins of North America, ESS-DIVE repository. Dataset. doi:10.15485/1809256', NA)
df[nrow(df) + 1, ] <- c(NA, "Crump, Byron. \"BioProject 642637 - NCBI,\" June 28, 2020. https://www.ncbi.nlm.nih.gov/bioproject/PRJNA642637.", "B")
df[nrow(df) + 1, ] <- c(NA, "Crump, Byron. \"BioProject 530074 - NCBI.\" Accessed December 14, 2022. https://www.ncbi.nlm.nih.gov/bioproject/PRJNA530074.", "B")


write.csv(df, "citations.csv")


