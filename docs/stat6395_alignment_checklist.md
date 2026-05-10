# STAT 6395 Final Project Alignment Checklist

This document checks the introductory statistics study app against the final-project rubric and the LLM/NLP topics covered in STAT 6395. The project does not need to use every course topic, but it should clearly show that the app is a thoughtful LLM/NLP system, not only a Shiny interface wrapped around a chatbot.

## Rubric Readiness

| Rubric area | Current status | Evidence in repo | What to do before submission |
|---|---|---|---|
| Problem statement | Present | README describes a module-based introductory statistics practice and grounded tutor app. | Make the tutorial open with the student pain point: students need help inside course modules while working practice questions. |
| LLM/API tools | Present | `R/tutor.R`, `app.R`, and generation scripts use `ellmer` with environment-variable API keys. | In the tutorial, name the provider options and explain fallback behavior when keys are missing. |
| Corpus/knowledge base | Present with public-safe scaffold | Public repo includes `data/processed/public_demo_chunks.csv`, the demo question bank, and ingestion/retrieval code; private textbook-derived assets are ignored. | For any public deployment, rebuild the corpus from licensed, open, or instructor-created materials. |
| Technical rigor | Strong | Hybrid retrieval, module policy, source policy, grounded prompts, practice tutor, vitals evals, visual helpers, and CI smoke checks. | In the presentation, explain this as a controlled RAG chain, not a generic chatbot. |
| Robust product behavior | Present | Smoke tests, setup checks, retrieval fallback, no-key fallback, public demo corpus, and cold-start practice flow. | Run the live demo from a clean session before presenting. |
| Reproducibility | Present for portfolio use | `DESCRIPTION`, `renv.lock`, `.Renviron.example`, GitHub Actions, setup checks, smoke tests, and public-safe demo chunks. | Reviewers can reproduce the app shell and demo RAG behavior; full textbook-backed RAG requires permission-cleared local sources. |
| Creativity/ambition | Strong | Professor overlays, module-first retrieval, practice-integrated tutor, weak concept tracking, visual metadata hooks, and deterministic visual explanations. | Clearly distinguish complete demo features from production extensions. |
| Tutorial | Present | Rendered HTML tutorial, screenshots, and final presentation deck are included. | Keep the source `.qmd` local-only unless intentionally publishing tutorial source later. |
| Deployment/publishing | Appropriately scoped | README frames the project as a portfolio/local proof of concept and documents local-only copyrighted materials. | Public deployment would require content permissions, secure API keys, privacy/logging policy, and hosted infrastructure. |
| Presentation | Present | `intro_stats_study_app_presentation_cleaned_v2.pptx`. | Use the deck with the live app demo. |

Note: the rubric PDF says "Project Rigor (20 points total)" but its point summary lists "Project Rigor 25." Confirm the grading total with the instructor if needed.

## Course Topic Coverage

| Course topic area | Status | How this project uses it |
|---|---|---|
| Tidy text representation | Present | Chunks are represented as tibbles with explicit metadata fields. |
| Tokenization | Present | `tokenize_rag_text()` tokenizes normalized text for keyword scoring, faithfulness checks, and visual scoring. |
| Stop word handling | Present | `rag_stopwords` removes low-value tokens before retrieval scoring. |
| N-grams | Partial | The current retrieval uses token and phrase matching, not formal n-gram features. Mention this as a future extension for better phrase retrieval. |
| Word/document frequency | Partial | Keyword scoring counts token and distinct-token hits. It does not yet expose corpus-level document frequency diagnostics. |
| TF-IDF | Present | Dense fallback uses `text2vec` TF-IDF plus LSA when the vector index is available. |
| BM25 | Partial | The app uses a BM25-like keyword fallback, not a formal BM25 package. This is acceptable for a proof of concept if documented honestly. |
| Topic modeling/LDA | Not used directly | Concept tags and modules act as a controlled topic layer. Explain why controlled course taxonomy is more appropriate than unsupervised LDA for a syllabus-aligned tutor. |
| Sentiment analysis | Not relevant | Student help quality does not depend on sentiment. Reasonable to omit. |
| Text feature engineering | Present | Domain aliases, spelling fixes, module IDs, concept tags, source types, and source scopes are explicit features. |
| Spelling and notation variation | Present | `R/aliases.R` normalizes variants such as `p^`, `p-hat`, `phat`, `xbar`, and `hyo test`. |
| Semantic similarity | Present/partial | `dense_retrieve()` supports TF-IDF/LSA dense vectors through `text2vec` when an index exists. Stronger API embeddings can be future work. |
| Text classification | Present as routing logic | `route_question_to_module()` treats module routing as a lightweight domain classifier. Intent classification supports direct-answer safety and visual requests. |
| Deep learning for text | Discussed, not trained | The project appropriately uses pretrained LLMs instead of training a neural model from scratch. |
| Transformer/LLM foundations | Present in design | Context windows and retrieved chunks control what the LLM sees; generation is evidence-constrained. |
| Prompt engineering | Present | `build_grounded_prompt()` and `build_practice_help_prompt()` separate general help, practice hints, concept explanation, diagnosis, and refusal/clarification behavior. |
| Tool/chain design | Present | The app uses a controlled chain: normalize -> retrieve -> rerank -> generate -> verify -> log. This is safer than an autonomous agent for tutoring. |
| RAG | Central | Ingestion, metadata, hybrid retrieval, reranking, module/source policy, parent context, grounded generation, and evidence trace are implemented. |
| Vitals evaluation | Present | `R/evals_vitals.R` builds task-specific vitals cases for retrieval, grounding, refusal, module routing, notation, practice help, and visuals. |
| Multimodal/images | Implemented scaffold + demo visuals | `R/images.R` supports visual metadata, local-only/deployment-safe filtering, visual retrieval, and tutor visual explanations. Recreated SVG visuals provide deployment-safe examples; full textbook figure extraction and multimodal image-input explanations remain future work. |
| Fine-tuning | Intentionally omitted | RAG is better because answers must stay grounded in changing course documents. Fine-tuning can be future work. |

## Minimum Alignment Check

| Requirement | Status |
|---|---|
| LLM API/tool use in R | Present via `ellmer`; fallback behavior exists when keys are missing. |
| RAG over course documents | Present. |
| Dense or semantic retrieval | Present when `text2vec` index is available; otherwise graceful fallback. |
| Keyword or hybrid retrieval | Present. |
| Reranking/source-priority logic | Present. |
| Grounded generation/hallucination controls | Present. |
| Prompt engineering for tutor behavior | Present. |
| Vitals-style evaluation | Present. |
| Module-based student workflow | Present. |
| Reproducible tutorial/setup | Present via rendered HTML tutorial, README quick start, `DESCRIPTION`, `renv.lock`, setup check, smoke test, and CI workflow. |

## Main Gaps to Close

1. Run a final live demo check before presenting.
2. Optionally add a short demo video or GIF to the repo if the class/project submission benefits from it.
3. If sharing beyond class, make clear that the included public demo corpus is synthetic and small; the full textbook-backed evidence layer must be rebuilt from permission-cleared sources.
4. If deploying publicly later, add authentication, privacy/logging policy, hosted storage, and instructor review.

## Suggested Project Framing

Use this one-sentence framing in the tutorial and presentation:

> This project builds a module-based introductory statistics practice app where LLM help is constrained by a textbook-centered RAG pipeline, domain-specific notation normalization, source-aware reranking, and vitals-style evaluation so students receive grounded hints while working actual course-style practice questions.
