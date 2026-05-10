# Project Instructions

This is an R/Shiny app for a Introductory Statistics study and practice system.

This is currently a local proof of concept and portfolio project, not a deployment-ready public student product. The GitHub repo should demonstrate architecture and reproducibility without redistributing copyrighted course content.

The default app should open directly into anonymous practice. Do not add visible student login, role labels, instructor role switching, or production LMS-style authentication unless the user explicitly asks for it.

Use the course textbook as the universal first-line authority for core concepts, formulas, terminology, and student-facing explanations.

Treat professor-specific materials as optional overlays inside the shared module structure. They may modify notation, emphasis, examples, or exam framing for a selected section, but they should not overwrite textbook-centered content.

Use supplemental materials only for alternate explanations, extra examples, and practice question inspiration.

Do not expose professor names, private file names, or source identities to students.

When conflicts are detected between textbook, professor overlays, formula sheets, or supplemental notes, flag them for admin review instead of automatically exposing them to students.

Keep the student experience module-centered: module_id -> topic_id -> concept_tag -> textbook chunks, visuals, examples, overlays, and practice templates.

Keep student-facing practice setup simple: students should mainly choose one or more modules and start practice. Question type, difficulty, source policy, and next-question decisions can remain internal. Multi-module practice is for cumulative review, not for exposing advanced setup controls.

Keep Start Practice fast. Starting a session should filter the cached question bank, choose the first question, and initialize state. It should not call an LLM, ingest PDFs, rebuild embeddings/indexes, or prefetch RAG evidence.

For practice and tutoring, distinguish the selected module pool from the current question module:
- `active_module_ids` is the selected practice/retrieval pool.
- `current_module_id` is the module for the current practice question.
- Legacy `active_module_id` may be used as an alias for the current question module when older functions require it.

Keep tutor help practice-integrated and conversational. The tutor may keep a short session-level conversation window for the current practice question, but all substantive course claims should still be grounded in retrieved evidence.

The tutor should guide students with hints, concept explanations, diagnosis, and follow-up support. It should not simply reveal final homework, quiz, or test answers, especially on early attempts.

Prefer cached per-question evidence and stored hint ladders for live tutor responsiveness. Full faithfulness/evaluation checks can run in debug, smoke, edge-case, or vitals workflows rather than on every simple hint.

Visuals can be used in practice questions and tutor explanations through metadata, not hard-coded copyrighted content. Textbook-derived figures are local-only unless permission is obtained. Recreated or open-license visuals may be deployment-safe when their metadata marks `safe_for_deployment = TRUE`.

The app should display local-only textbook visuals only when local proof-of-concept visual use is enabled. When local-only visuals are unavailable or disabled, prefer recreated/open visuals or provide a text-only visual explanation.

Tutor visual aids should be attached to the specific tutor message that generated them, with message-level visual metadata/path/caption, rather than rendered as a separate global visual area.

Public deployment would require permission-cleared textbook/course content, recreated or licensed visuals, secure API key handling, hosted knowledge infrastructure, and a defined privacy/logging policy.

Prefer R-first solutions. Use Shiny, bslib, DBI, RSQLite, dplyr, purrr, stringr, fs, yaml, jsonlite, pdftools, officer, readxl, and ellmer where appropriate.

Keep raw course files, generated wiki files, databases, local-only visuals, retrieval caches, and API keys out of GitHub.

## Current app UX decisions

- The app is a cold-start practice proof of concept: no student sign-in, no role switching, and no general course-chat tab in the main flow.
- Module selection is button/card-based and supports selecting one or more modules.
- Practice uses stored questions first and randomly samples from the selected module pool while avoiding immediate repeats when possible.
- Submitted-answer feedback appears directly below the answer controls and above the embedded tutor so students do not need to scroll past chat history to continue.
- The tutor quick actions are: Give me a hint and Explain this concept. A separate visual-help button is intentionally omitted; if a visual is clearly relevant, the tutor may attach it to the hint/concept response, and students can still type a visual request in the follow-up box.
- Question visuals and tutor visuals should be traceable: tutor visuals are attached to the specific tutor message object that produced them.
