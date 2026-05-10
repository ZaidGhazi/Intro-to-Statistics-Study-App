# Final Cleanup Report

This cleanup freezes the app before tutorial writing.

## UI changes

- Removed the redundant **Select all modules / Clear all modules** shortcut from the landing page.
- Kept **Cumulative Review** as the single mixed-practice option.
- Kept the student-facing app language generic: **Introduction to Statistics Study App**.

## Repository cleanup

Removed local/runtime or abandoned artifacts that are not needed for reproducibility:

- `.Rhistory`
- `.Rproj.user/`
- the old `statquest.sqlite` runtime database
- generated edge-case result files
- vitals run logs and summary outputs
- raw LLM question-generation JSON/errors
- draft wiki pages and raw LLM JSON intermediate files
- stale tutorial draft/rendered output

Kept the scripts and processed assets needed to run, test, audit, and explain the app:

- `app.R`
- core `R/` scripts for retrieval, tutoring, visual helpers, checks, smoke tests, audits, and vitals
- `data/processed/question_bank.csv`
- `data/processed/retrieval_index.rds`
- `data/wiki/concept_pages/`
- recreated visuals under `www/visuals/recreated/`
- `.Renviron.example`, `.gitignore`, `README.md`, and project documentation

## Source-material note

The repo does not include a textbook PDF or copyrighted textbook figures. The architecture can support a licensed/permission-cleared textbook as a first-line authority for question-bank generation and retrieval, but public deployment should use licensed, open-license, or instructor-created materials.
