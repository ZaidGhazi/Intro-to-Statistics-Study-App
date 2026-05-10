library(fs)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)
library(jsonlite)
library(pdftools)
library(officer)
library(readxl)
library(digest)

raw_dirs <- list(
  current_professor = "data/raw/current_professor",
  dr_south = "data/raw/dr_south",
  exam_materials = "data/raw/exam_materials"
)

processed_dir <- "data/processed"
text_dir <- file.path(processed_dir, "text")
manifest_path <- file.path(processed_dir, "source_manifest.csv")

unzip_all <- function() {
  walk(names(raw_dirs), function(source_name) {
    src_dir <- raw_dirs[[source_name]]
    zip_files <- dir_ls(src_dir, glob = "*.zip", recurse = FALSE)
    
    walk(zip_files, function(zip_file) {
      out_dir <- file.path(src_dir, path_ext_remove(path_file(zip_file)))
      dir_create(out_dir)
      
      message("Unzipping: ", zip_file)
      unzip(zip_file, exdir = out_dir)
    })
  })
}

classify_doc_type <- function(path) {
  file <- str_to_lower(path_file(path))
  
  case_when(
    str_detect(file, "formula|chart") ~ "formula_sheet",
    str_detect(file, "practice exam|practice") ~ "practice_problem",
    str_detect(file, "solution|answers") ~ "solution",
    str_detect(file, "review") ~ "review",
    str_detect(file, "lecture|notes|m\\d+d\\d+") ~ "lecture_note",
    str_detect(file, "template|xlsx|excel") ~ "excel_template",
    str_detect(file, "lockdown") ~ "admin",
    TRUE ~ "unknown"
  )
}

source_priority <- function(source_name, doc_type) {
  case_when(
    source_name == "exam_materials" & doc_type == "formula_sheet" ~ 1L,
    source_name == "current_professor" ~ 2L,
    source_name == "dr_south" ~ 3L,
    TRUE ~ 9L
  )
}

extract_pdf <- function(path) {
  paste(pdftools::pdf_text(path), collapse = "\n\n")
}

extract_docx <- function(path) {
  doc <- officer::read_docx(path)
  content <- officer::docx_summary(doc)
  
  content %>%
    filter(content_type %in% c("paragraph", "table cell")) %>%
    pull(text) %>%
    paste(collapse = "\n")
}

extract_pptx <- function(path) {
  ppt <- officer::read_pptx(path)
  content <- officer::pptx_summary(ppt)
  
  content %>%
    filter(!is.na(text), text != "") %>%
    pull(text) %>%
    paste(collapse = "\n")
}

extract_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  
  map_chr(sheets, function(sheet) {
    dat <- suppressWarnings(readxl::read_excel(path, sheet = sheet, col_names = FALSE))
    txt <- dat %>%
      mutate(across(everything(), as.character)) %>%
      tidyr::unite("row_text", everything(), sep = " | ", na.rm = TRUE) %>%
      pull(row_text) %>%
      paste(collapse = "\n")
    
    paste0("# Sheet: ", sheet, "\n", txt)
  }) %>%
    paste(collapse = "\n\n")
}

extract_text_safely <- function(path) {
  ext <- str_to_lower(path_ext(path))
  
  tryCatch(
    {
      if (ext == "pdf") {
        extract_pdf(path)
      } else if (ext == "docx") {
        extract_docx(path)
      } else if (ext == "pptx") {
        extract_pptx(path)
      } else if (ext %in% c("xlsx", "xls")) {
        extract_xlsx(path)
      } else {
        NA_character_
      }
    },
    error = function(e) {
      warning("Could not extract text from: ", path, "\n", conditionMessage(e))
      NA_character_
    }
  )
}

build_manifest <- function() {
  files <- imap_dfr(raw_dirs, function(src_dir, source_name) {
    dir_ls(
      src_dir,
      recurse = TRUE,
      type = "file",
      regexp = "\\.(pdf|docx|pptx|xlsx|xls)$"
    ) %>%
      as.character() %>%
      tibble(file_path = .) %>%
      mutate(source_name = source_name)
  })
  
  files %>%
    mutate(
      file_name = path_file(file_path),
      extension = str_to_lower(path_ext(file_path)),
      doc_type = map_chr(file_path, classify_doc_type),
      priority = map2_int(source_name, doc_type, source_priority),
      file_hash = map_chr(file_path, digest::digest, file = TRUE),
      extracted_text_path = file.path(
        text_dir,
        paste0(row_number(), "_", str_replace_all(path_ext_remove(file_name), "[^A-Za-z0-9]+", "_"), ".txt")
      )
    ) %>%
    distinct(file_hash, .keep_all = TRUE) %>%
    arrange(priority, source_name, file_name)
}

extract_all_text <- function(manifest) {
  manifest <- manifest %>%
    mutate(
      extracted_ok = FALSE,
      char_count = 0L
    )
  
  for (i in seq_len(nrow(manifest))) {
    message("Extracting [", i, "/", nrow(manifest), "]: ", manifest$file_name[[i]])
    
    txt <- extract_text_safely(manifest$file_path[[i]])
    
    if (!is.na(txt) && nchar(txt) > 0) {
      write_lines(txt, manifest$extracted_text_path[[i]])
      manifest$extracted_ok[[i]] <- TRUE
      manifest$char_count[[i]] <- nchar(txt)
    }
  }
  
  manifest
}

run_ingestion <- function() {
  dir_create(processed_dir)
  dir_create(text_dir)
  unzip_all()
  
  manifest <- build_manifest()
  manifest <- extract_all_text(manifest)
  
  readr::write_csv(manifest, manifest_path)
  
  message("\nDone.")
  message("Manifest written to: ", manifest_path)
  message("Extracted text folder: ", text_dir)
  
  manifest
}
