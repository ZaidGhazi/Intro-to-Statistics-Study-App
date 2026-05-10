# Update Notes

## Deep audit and tutor cleanup

This build focuses on making the app feel like a general **Introduction to Statistics Study App** for college-level introductory statistics students.

### Question-bank audit and rebuild

- Rebuilt the processed question bank around the book/course sequence instead of relying on loosely generated rows.
- Current bank size: **450 questions**.
- Each main module now has **45 questions**.
- Cumulative Review has **45 questions**.
- Removed placeholder wording such as `Take the same core idea into a fresh context`, `Scenario`, and `new setting`.
- Audited z-score/standard Normal questions so they route to **Module 5: Normal and Binomial Distributions**, not Module 1.
- Added a deep audit file: `data/processed/question_bank_deep_audit.csv`.
- Added/updated module, visual, family, and metadata audit outputs under `data/processed/`.

### Visual policy

- Visuals are no longer forced into ordinary questions.
- A visual appears in the question card only when `visual_required = TRUE` and the question explicitly says to use the visual aid.
- Tutor visuals can still be shown when the student asks for a visual or when the current question clearly matches an approved visual template.
- Visuals are recreated/deploy-safe assets or deterministic R/ggplot-style helpers, not textbook images.

### Tutor guardrails

- The tutor no longer tells the student that the current question belongs outside the selected module. The current practice question context is now treated as the priority.
- The tutor prompt now emphasizes two main rules:
  1. Do not give away the answer before submission.
  2. Stay grounded in retrieved evidence and practice context.
- Lightweight faithfulness checking is enabled for practice tutor responses by default.
- Internal diagnostics are hidden by default because development mode now defaults to off.

### Commands to run

```r
source("R/check_setup.R")
check_setup()

source("R/audit_question_bank.R")
run_question_bank_audit()

source("R/smoke_test.R")
run_smoke_test(run_vitals = FALSE)

shiny::runApp()
```

## Answer-option audit update

- Added a dedicated answer-option audit to `R/audit_question_bank.R`.
- The audit now checks for duplicate visible answer choices, duplicate IDs, missing correct-choice IDs, blank options, too few/many options, correct-answer text missing from options, overly long options, and student-facing placeholder/internal wording.
- Cleaned the processed question bank so the displayed answer choices are unique within each multiple-choice question.
- Added audit outputs: `question_bank_answer_option_audit.csv` and `question_bank_answer_option_issues.csv`.


## Smoke-test patch: resistant-measures concept anchor

- Added a student-facing, answer-safe concept explanation for resistant-measures concept mode.
- The explanation names resistance, middle position, mean, outliers, and tail behavior without directly giving the blank answer before submission.
- Added `data/raw/.gitkeep` so setup checks that expect the raw-data folder pass while keeping raw textbook/course materials out of the repo.

## Final freeze cleanup before tutorial

- Removed the redundant **Select all modules / Clear all modules** shortcut. The **Cumulative Review** module card is now the single mixed-practice option.
- Renamed the runtime SQLite database path from `statquest.sqlite` to `intro_stats_study_app.sqlite`; runtime databases are ignored by Git and created locally when the app runs.
- Removed local/runtime clutter from the packaged project: `.Rhistory`, `.Rproj.user/`, generated vitals logs, generated edge-case results, old raw LLM question-generation output, draft wiki pages, and stale tutorial draft/output files.
- Kept reproducibility assets: setup checks, smoke tests, audit scripts, vitals test set, processed question bank, retrieval index, concept pages, topic evidence, and recreated visuals.
- Set `.Renviron.example` defaults to keep development mode and local textbook visuals off unless the maintainer intentionally enables them.

## Feedback explanation cleanup

- Improved submitted-answer feedback so the explanation field teaches the relevant statistical idea instead of simply repeating the correct answer.
- Added a runtime fallback in `app.R` that detects weak feedback explanations and replaces them with a concept-specific explanation based on the question metadata.
- Refreshed the question bank's `explanation`, `solution_explanation`, and `concept_explanation` fields with more informative, student-facing language.
- Added feedback-explanation audit outputs:
  - `data/processed/question_bank_feedback_explanation_audit.csv`
  - `data/processed/question_bank_feedback_explanation_issues.csv`
