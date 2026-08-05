# HANDOFF

作業を再開する際は、着手前に必ずこのファイルを読むこと（`/Dropbox/CLAUDE.md` 参照）。

## 現在の状況（2026-08-05時点）

- 講義資料（和歌山向けhandout）の修正作業中。複数ファイルにまたがって編集している。
- git上で未コミットの変更あり（`git status` で確認可能）:
  - `labor_intro.qmd`
  - `quiz.html`
  - `seminar-theme.scss`
  - `docs/` 配下のレンダリング済みHTML・アセット多数（Quartoのビルド出力。手動編集ではなく `quarto render` 等の再ビルドで差分が出ている可能性が高い）
- ローカルブランチ `main` は `origin/main` より1コミット進んでいる（未push）。

## 次にやること

- （ここに次のアクションを追記していく）

## 保留・懸念事項

- `docs/` 配下で大量のファイルが「削除」扱いになっている（`dataintro_estimation.html`, `labor_hc.html` など）。意図した変更か、ビルド設定・出力先の変化によるものか要確認。

## 解決済み

- `.quarto` フォルダのDropbox選択型同期競合コピー問題: `.quarto` に `com.dropbox.ignored` 属性を設定し、Dropbox同期対象から除外することで解消（2026-08-05）。職場PC側でも未設定なら同様の対応が必要:
  `cmd /c "echo 1 > .quarto:com.dropbox.ignored"`
