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
df <- merge(df, report2[, c(3, 9)], by = "package_id")
df[nrow(df) + 1, ] <- c(NA, 'Rawlins M ; Connolly C ; McClelland J (2021): Model estimates of dissolved organic carbon, runoff, snowmelt, and snow water equivalent from 1981-2010 across the western Arctic. Quantifying Variability and Controls of Riverine Dissolved Organic Carbon Exported to Arctic Coastal Margins of North America, ESS-DIVE repository. Dataset. doi:10.15485/1809256', NA)
df[nrow(df) + 1, ] <- c(NA, "Crump, Byron. \"BioProject 642637 - NCBI,\" June 28, 2020. https://www.ncbi.nlm.nih.gov/bioproject/PRJNA642637.", "population dynamics")
df[nrow(df) + 1, ] <- c(NA, "Crump, Byron. \"BioProject 530074 - NCBI.\" Accessed December 14, 2022. https://www.ncbi.nlm.nih.gov/bioproject/PRJNA530074.", "population dynamics")

df <- df %>%
  recode(keywords, "primary pro")

write.csv(df, "citations.csv")
