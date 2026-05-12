# IntroStats Coach

[![R checks](https://github.com/ZaidGhazi/Intro-to-Statistics-Study-App/actions/workflows/r-check.yaml/badge.svg)](https://github.com/ZaidGhazi/Intro-to-Statistics-Study-App/actions/workflows/r-check.yaml)

**IntroStats Coach** is a module-based R/Shiny practice app for introductory statistics. It combines an audited question bank, retrieval-augmented tutoring, answer-safety guardrails, and trusted R/ggplot-style visual aids.

**Live demo:** https://zaidghazi.shinyapps.io/intro-statistics-study-app/

This is a portfolio and course project proof of concept. The goal is to demonstrate an educational LLM workflow: ordinary practice stays fast and local, while the tutor uses current-question context, retrieved evidence, and guardrails when students ask for help.

## What the app does

A student can:

1. open the app without a login,
2. choose one or more introductory-statistics modules,
3. answer stored practice questions,
4. receive immediate answer feedback,
5. ask for a hint, a concept explanation, or a typed follow-up,
6. request a visual aid when a picture would help,
7. continue practice without needing a live LLM call for every click.

The current student-facing quick actions are intentionally simple:

- **Give me a hint**
- **Explain this concept**
- typed follow-up chat for custom questions, including visual requests

The app is not designed to be an unrestricted chatbot. The tutor is embedded inside the current practice question and is expected to help without giving away the final answer before submission.

## Quick start

From the project root in R or RStudio:

```r
source("R/check_setup.R")
check_setup()

source("R/smoke_test.R")
run_smoke_test(run_vitals = FALSE)

shiny::runApp()
```

For a reproducible package setup:

```r
install.packages("renv")
renv::restore()
```

The app can launch without an API key. Without a key, tutor behavior falls back to stored/contextual explanations and local retrieval summaries. With a configured API key, the LLM is prioritized for richer, contextual tutoring.

## API key configuration

Do not commit API keys. Locally, copy `.Renviron.example` to `.Renviron` and fill in your key:

```bash
cp .Renviron.example .Renviron
```

Example entries:

```bash
ANTHROPIC_API_KEY="your-key-here"
ANTHROPIC_MODEL="claude-haiku-4-5"
ANTHROPIC_FAST_TUTOR_MODEL="claude-haiku-4-5"
ANTHROPIC_STRONG_MODEL="claude-sonnet-4-6"
STAT2331_DEV_MODE="false"
STAT2331_LOCAL_TEXTBOOK_VISUALS="false"
```

For a short shinyapps.io class demo, a bundled `.Renviron` can be included in the explicit deployment file list. This is a demo-only workaround. A production deployment should use managed secrets through a hosting platform such as Posit Connect, Connect Cloud, or an equivalent secure server environment.

## Question bank design

The practice loop uses a stored question bank rather than asking the LLM to write a new question each time. This keeps practice fast, reproducible, and easier to audit.

The current bank contains **450 questions**: 45 questions for each main module and 45 review questions. The bank is organized by:

- `module_id`
- `topic_id`
- `concept_tag`
- `question_family`
- answer choices and answer key
- stored explanations and hints
- optional visual metadata

The bank was generated with LLM assistance and then audited. The audit checks included:

- duplicate question text,
- duplicate or invalid answer choices,
- missing correct answer keys,
- weak or repetitive feedback explanations,
- module/topic/concept mismatches,
- irrelevant or forced visuals,
- student-facing artifact text such as `variant 3` or `Use this as another short practice setting`.

The app also cleans known generation artifacts at render time so leftover template text does not appear to students.

## How the tutor works

The tutor follows a controlled workflow instead of sending the student message directly to a generic chatbot.

```text
Student asks for help
  -> App collects current question context
     question text, answer choices, module, concept tag, submitted/not-submitted state
  -> App detects intent
     hint, concept explanation, typed follow-up, or visual request
  -> Retrieval runs when needed
     alias normalization, module-aware search, evidence ranking/reranking
  -> LLM drafts response when API key is available
     current question + answer choices + retrieved evidence
  -> Guardrails check response
     avoid answer leakage, stay on topic, prefer evidence/context
  -> Student sees the tutor response
     hint, explanation, follow-up, or visual explanation
```

When an API key is available, the LLM is prioritized for hints, concept explanations, typed follow-ups, and visual explanations. Retrieval still matters because it gives the model course-aligned evidence and keeps responses tied to the current question. When the key is unavailable or the model call fails, the app uses local fallback behavior.

## RAG role

RAG is part of the tutor layer, not the basic grading loop.

RAG helped because it gave the tutor an evidence layer instead of relying only on model memory. The app can use the current question, module, concept tag, aliases, and retrieved chunks to keep explanations closer to introductory-statistics content.

RAG also made the tutor more complex. Retrieval had to be aware of:

- the current module versus the selected module pool,
- notation variants such as `p-hat`, `p_hat`, `phat`, and `xbar`,
- whether the student had submitted an answer,
- whether a direct answer should be refused or turned into a nudge,
- whether retrieved evidence was too broad for the current question.

The final design treats RAG as supporting evidence for the LLM, not as a replacement for tutoring logic.

## Visual support

The app does **not** execute arbitrary LLM-generated R code. Instead, the LLM can help interpret the visual need, and the app maps that need to trusted R/ggplot-style templates.

Examples of supported visual families include:

- density-curve area,
- p-value tail area,
- confidence-interval number line,
- margin-of-error interpretation,
- residual visualization,
- lurking-variable diagram,
- sampling-bias / voluntary-response diagram,
- IQR / middle-50% boxplot,
- binomial distribution bars,
- sampling-distribution/CLT visual.

Visuals are attached to the tutor message that generated them. This keeps the explanation and the image together instead of showing a disconnected global plot.

## Evaluation and checks

The project uses several checks because generated educational content and LLM tutoring can fail in different ways.

### Question-bank audit

The question-bank audit checks the stored educational content before students see it. It looks for duplicate questions, bad answer options, missing keys, weak explanations, visual issues, and student-facing artifact text.

```r
source("R/audit_question_bank.R")
run_question_bank_audit()
```

### Smoke test

The smoke test checks whether the app's core components work together after code changes. It covers setup, parsing, retrieval, alias normalization, tutor fallback behavior, answer-safety behavior, visual routing, and message-scoped visuals.

```r
source("R/smoke_test.R")
run_smoke_test(run_vitals = FALSE)
```

### Vitals-style checks

The vitals-style evaluation is different from the smoke test. It is closer to a behavior test for the tutor. It checks cases such as:

- out-of-scope questions,
- ambiguous questions,
- notation questions,
- wrong-module retrieval,
- direct-answer requests,
- groundedness and refusal behavior.

```r
source("R/evals_vitals.R")
eval_run <- run_vitals_eval(dry_run = TRUE, view = FALSE)
eval_run$summary
```

Use `dry_run = FALSE` only when a valid API key is configured and answer-level evaluation is desired.

## Repository organization

```text
app.R
README.md
.Renviron.example
.gitignore

R/
  check_setup.R
  retrieval.R
  tutor.R
  practice_selection.R
  images.R
  visual_helpers.R
  audit_question_bank.R
  audit_workflow.R
  smoke_test.R
  edge_case_tests.R
  evals_vitals.R
  vitals_check.R

data/
  raw/                         # local-only; ignored except .gitkeep
  processed/
    question_bank.csv
    public_demo_chunks.csv
    question_bank_audit.csv
    question_bank_answer_option_audit.csv
    question_bank_feedback_explanation_audit.csv

www/
  visuals/recreated/            # deployment-safe recreated visuals
  session_visuals/              # generated locally; ignored by Git

tutorial/
  intro_stats_study_app_tutorial.qmd

docs/
  stat6395_alignment_checklist.md
```

## Source and deployment policy

The architecture is designed for permission-cleared course materials. A local development workspace may use licensed textbooks, instructor-created materials, or private course resources. The public repository should not redistribute raw copyrighted textbook PDFs, extracted textbook figures, private lecture materials, local retrieval indexes, `.Renviron`, or runtime databases.

A fresh public clone should still demonstrate:

- the Shiny practice app,
- the stored demo question bank,
- deploy-safe recreated visuals,
- public-safe demo chunks for retrieval smoke tests,
- setup checks, audits, and smoke tests,
- the tutorial and presentation artifacts.

Full textbook-backed RAG should be rebuilt locally from licensed, open, or instructor-created content.

## Limitations

This is not a production classroom deployment. Current limitations include:

- LLM response latency for richer tutor answers,
- reliance on external API availability for best tutor behavior,
- prototype-level progress/review-sheet features,
- no production authentication or student-data privacy layer,
- no fully permission-cleared public course corpus,
- automated checks that still need instructor review before classroom use.

A production version would need secure secrets management, hosted retrieval infrastructure, student authentication, privacy/logging policies, instructor review tools, and a larger human-reviewed question bank.

## Project framing

**IntroStats Coach is not just a Shiny chatbot.** It is a structured practice app where an audited question bank drives the practice loop, while the LLM tutor uses current question context, RAG evidence, answer-safety guardrails, and trusted visual templates to provide grounded support.
