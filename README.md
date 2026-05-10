# Introduction to Statistics Study App

[![R checks](https://github.com/ZaidGhazi/Intro-to-Statistics-Study-App/actions/workflows/r-check.yaml/badge.svg)](https://github.com/ZaidGhazi/Intro-to-Statistics-Study-App/actions/workflows/r-check.yaml)

Local proof-of-concept R/Shiny app for module-based introductory statistics practice, review, and grounded tutor help.

This is a portfolio and learning project. The code and tutorial are intended to show the architecture, reproducible workflow, and LLM/NLP design choices. The current textbook-backed version is not a public student-facing deployment.

The app is designed as a frictionless cold-start demo: open it, choose one or more modules, and start practicing. There is no visible login, role switcher, or production-style LMS account flow.

## Quick Start

From the project root in R or RStudio:

```r
source("R/check_setup.R")
check_setup()

shiny::runApp()
```

For a closer pre-demo check:

```r
source("R/smoke_test.R")
run_smoke_test(run_vitals = FALSE)
```

If packages are missing, `check_setup()` prints the exact `install.packages()` command. For a more reproducible setup, install `renv` and restore the locked package versions:

```r
install.packages("renv")
renv::restore()
```

## Source Authority

The RAG tutor is designed around a textbook-centered core:

1. A licensed or permission-cleared introductory-statistics text can serve as the universal first-line authority for concepts, formulas, terminology, and examples.
2. Professor or section materials are overlays that can adjust notation, emphasis, and examples inside the selected module.
3. Formula sheets and exam materials can support retrieval, but should not silently override conflicts.
4. Supplemental notes can add examples or alternate explanations.

Student-facing navigation remains module-centered even though the knowledge authority is textbook-centered.

## Tutor Design

The student-facing practice flow is intentionally simple: students choose one or more modules, start practice, answer the current question, receive feedback, and ask the embedded tutor for help when needed. Question type, difficulty, source policy, and next-question decisions are handled internally. Multi-module practice supports cumulative review while keeping retrieval constrained to the selected module pool.

The LLM tutor is integrated into the practice flow so students can ask for help while working on a specific question. The tutor keeps a short session-level conversation window for the current practice item, including the selected module pool, the current question's module, question text, student answer, attempt count, weak concept tag, previous tutor answer, retrieved evidence, and visual metadata when available.

The tutor is designed to guide rather than simply give final answers. It uses retrieval, reranking, source policy, module policy, notation normalization, and faithfulness checks to reduce hallucination and keep responses grounded in course evidence.

For responsiveness, the live tutor caches retrieved evidence and visual metadata per practice question. The cache key includes the current question, current module, selected module pool, and expected concept. Stored hints are shown immediately when possible, and full faithfulness checks are reserved for debug/evaluation settings instead of running on every simple hint. Vitals and edge-case tests remain available for stricter offline evaluation.

Starting practice does not call the LLM or rebuild retrieval assets. It filters the already-loaded question bank, chooses a question from the selected module pool, and initializes session state. Retrieval and LLM calls wait until the student asks for tutor help or the app explicitly needs an AI-generated follow-up.

## Visual Support

Practice questions can optionally include linked visuals through `visual_id`, `visual_ids`, `visual_required`, and `tutor_visual_ids`. The app only displays a visual inside the practice card when that visual is intentionally required for the question. Otherwise, visuals are treated as optional tutor aids. The quick tutor buttons are kept simple (`Give me a hint` and `Explain this concept`); if a visual is clearly relevant, the tutor can attach it to that same response. Students can also type a visual request such as "Can you show this with a graph?" in the follow-up box.

Tutor visual aids are attached to the specific tutor message that produced them. Each assistant turn can carry its own `visuals` metadata, caption, source/safety fields, evidence pointer, and retrieval trace. This keeps visual explanations reviewable and traceable: when a student scrolls through the conversation, the plot or image stays inside the same bubble as the explanation that referenced it.

Visuals are resolved through `R/images.R`, which maintains a metadata table for textbook-derived local-only figures, recreated visuals, and open-license visuals. The live app prefers visuals that are explicitly linked to the current question or strongly matched to the current concept. Broad visual retrieval is intentionally conservative so that unrelated plots are not forced into ordinary hints or concept explanations. If a file is missing, the app keeps the question usable and shows a small local-run fallback message instead of crashing.

For common demo concepts, `R/visual_helpers.R` also renders deterministic R/Shiny visual aids without asking the LLM to draw anything. Current examples include a right-skewed distribution comparing mean and median, an outlier boxplot, a bar-chart-vs-histogram comparison, p-value tail shading, a confidence-interval number line, a standard normal curve, a scatterplot, a regression residual plot, a binomial distribution bar chart, a random-assignment diagram, a two-way-table segmented bar chart, side-by-side boxplots, and a sampling-distribution/CLT visual. Session-generated tutor visuals are written under `www/session_visuals/` for local display and are ignored by Git.

The repo includes a few simple recreated SVG visuals under `www/visuals/recreated/` so the architecture can be demonstrated without redistributing textbook figures. Textbook-derived visuals should remain local-only in ignored folders such as `data/visuals/` or `www/visuals/textbook/`.


## Question Bank Coverage

The stored question bank is intentionally broad enough for longer demo practice sessions. It includes conceptual multiple-choice, choose-best-answer, short fill-in-the-blank, and interactive-style items organized by module, topic, concept tag, and question family. Visual questions are included where the visual is central to the skill being practiced; other visual links are kept optional for tutor explanations.

The current topic scope follows a standard introductory-statistics sequence similar to *The Basic Practice of Statistics*, 6th edition, especially Chapters 1-5 and 8-21. The app does not redistribute the textbook PDF or textbook figures; a public deployment should use licensed, open-license, or instructor-created source materials. The repo stores deploy-safe recreated visuals and generated practice metadata.

## Local Proof-of-Concept Limits

This repo may point to local copyrighted textbook PDFs, extracted text, generated wiki pages, and local-only visuals. Those files are intentionally ignored by Git and should not be deployed without permission review.

Do not hard-code API keys. Copy `.Renviron.example` to `.Renviron` locally and fill in keys there if you want live LLM responses. The app falls back to retrieval-based responses when optional services are unavailable.

## What Is Published vs. Local-Only

The GitHub repository is intended to demonstrate the app architecture, workflow, tutorial, audits, deploy-safe recreated visuals, and a demo question bank. It intentionally does **not** redistribute raw textbook PDFs, extracted copyrighted figures, local API keys, runtime SQLite databases, generated session visuals, local vector indexes, extracted source text, or source-derived topic evidence files.

The public repo includes files such as:

- Shiny app code and modular R helpers
- setup, smoke-test, audit, edge-case, and vitals evaluation scripts
- a generated demo question bank and audit summaries
- a small public-safe demo corpus for retrieval/RAG smoke tests
- safe recreated visual assets under `www/visuals/recreated/`
- the rendered HTML tutorial and screenshots

Local-only files are ignored by `.gitignore`, including:

- `.Renviron`
- `data/raw/`
- `data/processed/text/`
- `data/processed/topic_evidence/`
- `data/processed/retrieval_index.rds`
- `data/processed/source_manifest.csv`
- `data/wiki/concept_pages/*.md`, except the folder README
- `www/session_visuals/`
- runtime `.sqlite` / `.db` files

After cloning the public repo, the app should still launch as a proof-of-concept practice app using the included question bank. Textbook-backed retrieval indexes and source-derived concept pages must be rebuilt locally from permission-cleared materials.

The file `data/processed/public_demo_chunks.csv` provides a tiny synthetic/open demo corpus so retrieval and grounded tutor flows have public-safe evidence immediately after cloning. It is not a replacement for a full textbook-backed knowledge base; it is a reproducibility scaffold for portfolio review and tests.

## Deployment / Readiness Note

Current local proof of concept:

- A local textbook PDF may be used by the local maintainer for ingestion and testing.
- Local vector indexes and processed chunks may be created.
- Local-only textbook images may be used while building and evaluating the prototype.

Before any public deployment:

- Obtain rights or permission for textbook/course content, or replace it with licensed, open-license, or instructor-created material.
- Replace copyrighted textbook figures with recreated visuals or permission-cleared images.
- Configure secure API key handling outside the app code.
- Define a privacy and logging policy for student questions and tutor responses.
- Host a permission-cleared processed knowledge base or vector database.
- Run expanded evaluations with instructors and representative users.

## RAG Files

- `R/chunk_schema.R`: shared chunk metadata schema and module mapping.
- `R/aliases.R`: notation and spelling normalization.
- `R/ingest_textbook.R`: safe first-pass textbook PDF ingestion by page/chapter/section.
- `R/overlays.R`: professor and supplemental overlay ingestion.
- `R/retrieval.R`: hybrid retrieval, reranking, module policy, source policy.
- `R/tutor.R`: grounded prompt construction, feedback generation, faithfulness checks.
- `R/images.R`: visual metadata, local-only/deployment-safe filtering, visual retrieval, and visual explanation context.
- `R/vitals_check.R`: retrieval and groundedness test harness.
- `R/evals_vitals.R`: vitals-based evaluation suite for the RAG tutor.
- `R/edge_case_tests.R`: local edge-case suite for notation, vague follow-ups, wrong-module questions, direct-answer safety, weak evidence, visuals, and API fallback.

## STAT 6395 Final Project Materials

- `docs/stat6395_alignment_checklist.md`: rubric and course-topic alignment checklist.
- `tutorial/intro_stats_study_app_tutorial.html`: rendered tutorial explaining the NLP/LLM/RAG workflow.
- `intro_stats_study_app_presentation_cleaned_v2.pptx`: final project presentation deck.

The rendered tutorial can be published online without raw copyrighted course content. The app itself should only be deployed with permission-cleared or recreated materials.

## Run Locally

From R or RStudio:

```r
source("R/check_setup.R")
check_setup()

source("R/smoke_test.R")
run_smoke_test(run_vitals = FALSE)

source("R/edge_case_tests.R")
edge_results <- run_edge_case_tests(dry_run = TRUE)
summarize_edge_case_results(edge_results)

source("R/performance_check.R")
run_performance_check()

source("R/images.R")
visuals <- load_image_metadata()
retrieve_relevant_visuals("Can you show p-value visually?", module_id = "hypothesis_testing")
```

```r
shiny::runApp()
```

Run the vitals harness:

```r
source("R/chunk_schema.R")
source("R/aliases.R")
source("R/overlays.R")
source("R/retrieval.R")
source("R/tutor.R")
source("R/vitals_check.R")
run_vitals_check()
```

Run the vitals package eval suite:

```r
install.packages("vitals")
source("R/evals_vitals.R")
eval_run <- run_vitals_eval(dry_run = TRUE)
eval_run$summary
open_vitals_view()
```

Use `dry_run = FALSE` when `ANTHROPIC_API_KEY` or `OPENAI_API_KEY` is configured and you want answer-level evaluation, not retrieval-only evaluation.
The eval runner writes `data/processed/vitals_summary.csv`, `data/processed/vitals_summary.rds`, and vitals logs under `data/processed/vitals_logs/`.

Optional textbook ingestion:

```r
source("R/chunk_schema.R")
source("R/aliases.R")
source("R/ingest_textbook.R")
result <- ingest_textbook_pdf("data/raw/textbook/your_textbook.pdf")
saveRDS(result$chunks, "data/processed/textbook_chunks.rds")
```

The saved chunks are local-only by default and ignored by Git.

## Reproducibility Notes

- `DESCRIPTION` declares the app's required and optional packages.
- `renv.lock` records the package versions used for this public snapshot.
- `.github/workflows/r-check.yaml` runs the setup check and smoke test on GitHub Actions.
- `R/check_setup.R` now exposes `intro_stats_*` helper names while keeping backward-compatible `stat2331_*` aliases for older scripts.

## Practice-bank and visual-question architecture

The live Shiny app is designed to cold start quickly: a reviewer opens the app, chooses one or more modules, and starts practicing from the stored question bank. The app should not call an LLM when practice starts or when it randomly selects the next question.

The processed question bank is module-tagged and now supports visual metadata fields such as `visual_id`, `visual_ids`, `visual_template_id`, `visual_required`, and `tutor_visual_ids`. These fields let some questions show deploy-safe recreated visuals directly in the question card and let the embedded tutor attach visuals to the specific tutor response that used them. The current bank is expanded for longer practice sessions and is aligned to the scope of *The Basic Practice of Statistics*, 6th edition: variables/graphs, descriptive summaries, normal and binomial distributions, sampling/experiments, probability, sampling distributions, confidence intervals, hypothesis tests, and inference cautions.

Visuals used for the portfolio demo are recreated assets under `www/visuals/recreated/` or deterministic R/ggplot-style visual helpers. Textbook-derived PDFs or extracted textbook figures remain local-only and are not included in the repository. A public deployment would need textbook/content rights or replacement with instructor-created/open-license materials.

The LLM is used selectively for grounded tutoring, conversational follow-ups, optional visual explanations, and optional similar-question generation when stored same-concept questions are unavailable. Stored hints, stored explanations, local grading, question selection, and deterministic visual rendering should work without an API key.

Useful audits:

```r
source("R/audit_question_bank.R")
run_question_bank_audit()

source("R/audit_workflow.R")
run_workflow_audit()
```

## Tutor guardrails

The embedded tutor is designed to do two things during practice:

1. **Guide without giving away the answer.** Before a student submits an answer, the tutor should avoid filling in blanks, selecting the correct option, or saying "the answer is ...". A post-processing guardrail checks for exact answer leakage and redacts it when needed.
2. **Stay grounded in course evidence.** Tutor responses are built from the current question context plus retrieved course evidence. A lightweight faithfulness check runs by default and can replace weakly supported responses with a safer grounded fallback.

The **Give me a hint** button favors stored hint ladders for speed. The **Explain this concept** button now uses the grounded tutor path rather than directly dumping stored concept-page text.


## Deep-audited question bank

The app uses a prebuilt, module-tagged question bank for fast practice. The current bank has 450 questions, with 45 questions in each main module and 45 cumulative-review questions. The bank was audited for module/topic/concept alignment, placeholder wording, duplicate question text, z-score routing, visual relevance, student-facing internal language, and answer-option quality. The answer-option audit checks for duplicate visible choices, duplicate IDs, missing/invalid correct-choice IDs, blank choices, too few/many choices, and correct-answer text missing from the displayed options.

Visuals are intentionally conservative: a visual appears in a question card only when the question explicitly asks the student to use the visual aid. The tutor can still attach a recreated ggplot-style visual when the student asks for a visual or when the current question clearly matches an approved visual template.

The tutor is designed around two guardrails: do not give away the answer before the student submits, and stay grounded in retrieved course evidence plus the current practice-question context.


## Feedback quality checks

The question-bank audit now includes answer-option checks and feedback-explanation checks. This helps catch cases where submitted-answer feedback simply repeats the correct answer instead of explaining the statistical idea. Run:

```r
source("R/audit_question_bank.R")
run_question_bank_audit()
```

The audit writes summary files under `data/processed/`, including `question_bank_feedback_explanation_audit.csv` and `question_bank_feedback_explanation_issues.csv`.
