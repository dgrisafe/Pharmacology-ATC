library(tidyverse)

# xlsx file name
path_xlsx <- "2023_WHOCC_ATC_index_2023-03-03_ref.xlsx"

# names of sheets in xlsx file
readxl::excel_sheets(path_xlsx)

# read sheets into separate dataframes
df_atc_1anatomical <- readxl::read_xlsx(path_xlsx, "1 Anatomical")
df_atc_2therapeutic <- readxl::read_xlsx(path_xlsx, "2 Therapeutic")
df_atc_3pharmacological <- readxl::read_xlsx(path_xlsx, "3 Pharmacological")
df_atc_4chemicalsubgroup <- readxl::read_xlsx(path_xlsx, "4 Chemical Subgroup")
df_atc_5chemicalsubstance <- readxl::read_xlsx(path_xlsx, "5 Chemical Substance")

# function that combines variables to a single html line interpretable by Anki
concat_atc_html <- function(df, label, code, description, link){
  df %>% 
    mutate(c_html = paste0("<a href = '", link, "'>", code, "</a><td>", description, "</td>")) %>% 
    rename_with(~paste0(., label))
}

# add column c_atc_html to each data set
df_html_1anatomical <- concat_atc_html(df_atc_1anatomical, "1")
df_html_2therapeutic <- concat_atc_html(df_atc_2therapeutic, "2")
df_html_3pharmacological <- concat_atc_html(df_atc_3pharmacological, "3")
df_html_4chemicalsubgroup <- concat_atc_html(df_atc_4chemicalsubgroup, "4")
df_html_5chemicalsubstance <- concat_atc_html(df_atc_5chemicalsubstance, "5")

# separate code of individual 
df_5chemicalsubstance <- df_html_5chemicalsubstance %>% 
  separate_wider_position(
    cols = code5, cols_remove = FALSE,
    widths = c(
      # anatomic group is same as code1
      "code1" = 1, 
      # t = therapeutic subgroup
      "code_2t" = 2, 
      # p = pharmacological subgroup
      "code_3p" = 1, 
      # csg = chemical subgroup
      "code_4csg" = 1, 
      # css = chemical substance
      "code_5css" = 2)
  ) %>% 
  # merge codes so they match higher order, smaller dataframes
  mutate(
    code2 = paste0(code1, code_2t),
    code3 = paste0(code2, code_3p),
    code4 = paste0(code3, code_4csg)
  )

# combine all datasets with codes for each hierarchy of each drug in highest, 5th level (chemical substance)
df_html_1to5_anki_messy <- df_5chemicalsubstance %>% merge(df_html_1anatomical, by = "code1") %>% 
  merge(df_html_2therapeutic, by = "code2") %>% 
  merge(df_html_3pharmacological, by = "code3") %>% 
  merge(df_html_4chemicalsubgroup, by = "code4") %>%
  # create single column of html to be input to anki field
  mutate(atc_html_anki = paste(
    "<table style='text-align: left'><tbody>", 
      "<tr><td>", c_html1, "</td></tr>",
      "<tr><td>", c_html2, "</td></tr>", 
      "<tr><td>", c_html3, "</td></tr>", 
      "<tr><td>", c_html4, "</td></tr>", 
      "<tr><td>", c_html5, "</td></tr>", 
    "</tbody></table>"
    )
    ) %>% glimpse

df_html_atc_anki <- df_html_1to5_anki_messy %>% 
  # rename description5 to be generic name
  rename(generic = description5) %>% 
  select(generic, code5, atc_html_anki)

# example with more than one ATC code
# df_html_atc_anki %>% filter(str_detect(generic, "dextromethorphan"))

# if more than one ATC code for a drug (e.g., combination with another drug)
# then use the following code to separate the tables
# <tr><td> ~ </td><td> ~ </td></tr>

# save csv file
write_excel_csv(file = paste0("./!ankiATC_", Sys.Date(), ".csv"), x = df_html_atc_anki)
