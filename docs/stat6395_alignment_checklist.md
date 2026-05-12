# STAT 6395 Final Project Alignment Checklist

This document summarizes how **IntroStats Coach** aligns with the STAT 6395 final-project goals and the LLM/NLP techniques covered in the course. The project is not just a Shiny UI wrapped around a chatbot; it is a controlled educational LLM workflow with a stored question bank, retrieval grounding, tutor guardrails, visual support, and evaluation checks.

## Rubric readiness

| Rubric area | Status | Evidence in repo | Notes before submission |
|---|---|---|---|
| Problem statement | Present | README and tutorial frame the app around intro-statistics practice with embedded help. | Emphasize the student pain point: students need help while working practice questions, not after leaving the app. |
| LLM/API tools | Present | `R/tutor.R`, `app.R`, and related helper scripts use `ellmer` with environment-variable API keys. | Explain that LLM behavior is best with an API key, with local fallbacks when keys are unavailable. |
| Corpus / knowledge base | Present with public-safe scaffold | The repo includes a demo question bank, retrieval code, and a small public-safe demo corpus. | Full textbook-backed RAG must be rebuilt from licensed, open, or instructor-created sources. |
| Question bank | Present | `data/processed/question_bank.csv` and audit outputs. | Generated with LLM assistance, then audited for answer choices, feedback, metadata, visuals, and artifact text. |
| RAG | Central to tutor layer | `R/retrieval.R`, `R/tutor.R`, alias normalization, retrieval/reranking logic. | Present honestly: RAG helped grounding, but also added routing/latency complexity. |
| Tutor guardrails | Present | `R/tutor.R`, smoke tests, vitals-style checks. | Guardrails target answer leakage, unsupported claims, out-of-scope prompts, and ambiguous prompts. |
| Technical rigor | Strong | Hybrid retrieval, module policy, source policy, prompt construction, visual helpers, audits, smoke tests, vitals-style checks. | Explain the system as a chain: context -> retrieval -> LLM -> guardrails -> response. |
| Visual support | Present | `R/visual_helpers.R`, `R/images.R`, recreated visual metadata. | Visuals are trusted R/ggplot-style templates; the app does not execute arbitrary LLM-generated R code. |
| Reproducibility | Present | `DESCRIPTION`, `renv.lock`, `.Renviron.example`, setup checks, smoke tests, GitHub Actions. | A reviewer can run the app shell and demo retrieval; private source assets remain local-only. |
| Tutorial | Present | `tutorial/intro_stats_study_app_tutorial.qmd` and rendered screenshots. | Update screenshots/text if the app name or UI changes. |
| Presentation | Present | `intro_stats_coach_final_presentation.pptx` or latest final deck. | Use live demo link plus backup screenshots. |
| Deployment | Appropriately scoped | README describes live demo and local-only source policy. | Public production use would need content permissions, secure secrets, privacy/logging, and hosted infrastructure. |

## Course topic coverage

| Course topic area | Status | How the project uses it |
|---|---|---|
| Tidy text representation | Present | Chunks and question-bank rows are structured as tabular metadata. |
| Tokenization | Present | Retrieval and scoring tokenize normalized text. |
| Stop words | Present | Retrieval helpers remove low-value terms. |
| N-grams / phrase handling | Partial | Phrase and alias matching are used; formal n-gram feature engineering could be future work. |
| Word/document frequency | Partial | Keyword scoring uses token overlap; full corpus-level diagnostics are not central to the app. |
| TF-IDF / dense retrieval | Present/partial | `text2vec` TF-IDF/LSA-style retrieval is supported when indexes are available; fallback retrieval remains public-safe. |
| BM25-like retrieval | Partial | The app uses keyword-style scoring and reranking rather than a standalone BM25 package. |
| Topic modeling | Not used directly | A controlled course taxonomy is more appropriate than unsupervised LDA for syllabus-aligned tutoring. |
| Text feature engineering | Present | Module IDs, concept tags, source type, source scope, aliases, and notation variants are explicit features. |
| Spelling/notation normalization | Present | Aliases normalize variants like `p-hat`, `p_hat`, `phat`, `xbar`, and common misspellings. |
| Text classification / routing | Present | Module routing and tutor intent detection act as lightweight classification tasks. |
| Transformer/LLM foundations | Present | The LLM uses a controlled context window: current question, answer choices, evidence, and guardrails. |
| Prompt engineering | Present | Tutor prompts separate hint, concept explanation, typed follow-up, visual request, and refusal behavior. |
| Tool/chain design | Present | The app uses a controlled chain rather than an autonomous agent. |
| RAG | Central | Retrieval provides course-aligned evidence for tutor responses. |
| Vitals-style evaluation | Present | `R/evals_vitals.R` checks retrieval, tutor behavior, refusal behavior, ambiguity, notation, and visuals. |
| Multimodal/images | Present as scaffold | Visual metadata and deterministic ggplot-style visuals support question/tutor explanations. |
| Fine-tuning | Intentionally omitted | RAG is preferred because course materials and notation may change; fine-tuning is future work at most. |

## Tutor system summary

The current tutor system works as follows:

```text
Student asks for help
  -> collect current question + answer choices + module/concept metadata
  -> detect intent: hint, concept explanation, typed follow-up, or visual request
  -> retrieve/rerank evidence when useful
  -> ask LLM for a contextual response when API key is available
  -> apply guardrails against answer leakage and unsupported claims
  -> show the student a grounded hint, explanation, follow-up, or visual explanation
```

RAG helped by adding a course-aligned evidence layer. It also made the tutor more complicated because retrieval has to respect module selection, current question context, notation aliases, and answer-safety rules. This tradeoff should be discussed in the presentation.

## Evaluation summary

| Evaluation layer | What it checks | Why it matters |
|---|---|---|
| Question-bank audit | Generated content quality: duplicates, bad options, missing keys, weak explanations, visual relevance, artifact text. | Prevents low-quality generated questions from reaching students. |
| Smoke test | Core app mechanics: setup, parsing, retrieval, tutor fallback, answer-safety, visual routing, message-scoped visuals. | Verifies the app still works after code changes. |
| Vitals-style checks | Tutor behavior on tricky prompts: ambiguity, out-of-scope requests, direct-answer requests, notation, wrong-module retrieval, grounding. | Evaluates whether the tutor behaves like a safe educational assistant. |

## Minimum alignment check

| Requirement | Status |
|---|---|
| LLM API/tool use in R | Present via `ellmer`; fallback behavior exists when keys are unavailable. |
| RAG over course-aligned content | Present. |
| Keyword / hybrid retrieval | Present. |
| Dense retrieval scaffold | Present when indexes are built. |
| Reranking/source-priority logic | Present. |
| Grounded generation / hallucination controls | Present. |
| Answer-leakage guardrails | Present. |
| Module-based student workflow | Present. |
| Question-bank audit | Present. |
| Smoke testing | Present. |
| Vitals-style evaluation | Present. |
| Reproducible setup/tutorial | Present. |

## Main gaps and future work

1. Run the final live demo link from a clean session before presenting.
2. Keep backup screenshots in the deck in case the hosted app is slow.
3. Make clear that the included public demo corpus is only a scaffold; full textbook-backed RAG requires permission-cleared sources.
4. For real deployment, add authentication, privacy/logging, secure API-key handling, and instructor review tools.
5. Expand human review of generated questions before classroom use.

## Suggested framing sentence

> IntroStats Coach is a module-based introductory-statistics practice app where an audited question bank drives fast practice, while an LLM tutor uses current question context, RAG evidence, answer-safety guardrails, and trusted visual templates to provide grounded help.
