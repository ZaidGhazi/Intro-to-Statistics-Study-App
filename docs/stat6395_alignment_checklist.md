# STAT 6395 Final Project Alignment Checklist

This document checks the introductory statistics study app against the final-project rubric and the LLM/NLP topics covered in STAT 6395. The project does not need to use every course topic, but it should clearly show that the app is a thoughtful LLM/NLP system, not only a Shiny interface wrapped around a chatbot.

## Rubric Readiness

| Rubric area | Current status | Evidence in repo | What to do before submission |
|---|---|---|---|
| Problem statement | Present | README describes a module-based introductory statistics practice and grounded tutor app. | Make the tutorial open with the student pain point: students need help inside course modules while working practice questions. |
| LLM/API tools | Present | `R/tutor.R`, `app.R`, and generation scripts use `ellmer` with environment-variable API keys. | In the tutorial, name the provider options and explain fallback behavior when keys are missing. |
| Corpus/knowledge base | Present | `data/raw/`, `data/processed/`, `data/wiki/`, `R/ingest_textbook.R`, `R/overlays.R`. | Do not publish copyrighted raw text. Describe the corpus structure and local-only restriction. |
| Technical rigor | Strong but needs narrative | Hybrid retrieval, module policy, source policy, grounded prompts, practice tutor, vitals evals. | Explain the workflow as a controlled RAG chain, not a generic chatbot. |
| Robust product behavior | Partial | Smoke tests, setup checks, retrieval fallback, no-key fallback, app pre-flight fixes. | Run smoke tests and include screenshots or a short demo video. |
| Reproducibility | Partial | `.Renviron.example`, `R/check_setup.R`, `R/smoke_test.R`, modular R files. | Add package installation instructions and render the Quarto tutorial. |
| Creativity/ambition | Strong | Professor overlays, module-first retrieval, conversational practice-integrated tutor, weak concept tracking, image metadata hooks. | Present these as ambition features, with clear notes on what is complete vs scaffolded. |
| Tutorial | Planned next | the planned Quarto tutorial file. | Fill in screenshots, sample eval output, and any demo links. |
| Deployment/publishing | In tension with local-only design | README says local proof of concept; rubric asks for online access. | Publish the tutorial/repo online, but deploy the app only with non-copyrighted or permission-cleared materials. For class, a recorded local demo may be safest. |
| Presentation | Not present in repo | None. | Use the architecture and checklist tables as slide material. |

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
| Reproducible tutorial/setup | To be written/rendered after the final app freeze; needs screenshots and rendered output. |

## Main Gaps to Close

1. Render and polish the Quarto tutorial.
2. Add screenshots or a short demo video showing the app workflow.
3. Run `run_edge_case_tests(dry_run = TRUE)` and `run_vitals_eval(dry_run = TRUE)`, then paste small summary tables into the tutorial.
4. Decide how to satisfy the online-access expectation without publishing copyrighted textbook material.
5. Clearly label dense retrieval as TF-IDF/LSA unless API embeddings are added.
6. Treat image support as scaffolded unless actual metadata and visuals are available.
7. Add a final presentation architecture diagram using the controlled RAG chain.

## Suggested Project Framing

Use this one-sentence framing in the tutorial and presentation:

> This project builds a module-based introductory statistics practice app where LLM help is constrained by a textbook-centered RAG pipeline, domain-specific notation normalization, source-aware reranking, and vitals-style evaluation so students receive grounded hints while working actual course-style practice questions.
