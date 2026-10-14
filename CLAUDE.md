[講義資料](https://ayakanakamuraecon.github.io/handout/index.html)で公開している講義資料のファイルです。

## seminar_*.html のデザイン規約

`seminar_*.html`（`seminar_kiso.html` `seminar_r.html` `seminar_tokei.html` 等、Quartoを経由しない手書きHTML＋インラインCSSのページ群）は、`docs/`配下がビルド成果物、ルート直下のファイルが原本。

- 本文（地の文）を運ぶボックスパーツ（`.callout` `.note` `.alert` `.tip` `.compare-box` など）のフォントサイズは、固定pxで個別に小さくせず、`body`（16px）と同じ`font-size:1em`にする。
  - 理由：各ボックスに13.5px〜14.5px等の値が個別にハードコードされ、`body`のデフォルト16pxより小さく見える不具合が複数回発生したため。
  - 一方、キャプション（`.table-caption`）・コード（`.codebox` / `code`）・ナビ/タグ的UI要素（`.jumpnav a` `.related-nav a` `.toc-return`）は本文ではなくメタ情報/UI要素なので、これまで通り小さめのpxのままでよい。
  - `.table-caption`は、図表の出典・データ識別など**純粋なメタ情報**の表記に限定して使う。図の内容を解説する説明文（グラフの読み方の補足など、実質的に地の文の一部）には使わず、`.fig-note`（`seminar_tokei.html`で導入、`font-size:1em`・色のみ`var(--sub)`）か、プレーンな`<p>`を使う。判断基準：その注釈を読まないと図の意味が追えないなら地の文（`.fig-note`等）、単なる出典表記ならメタ情報（`.table-caption`）（2026-10時点、`seminar_tokei.html`での指摘を機に明文化）。
- `.note-label`（補足・まとめ等のラベル見出し）は、上記のメタ情報/UI要素の例外として、`.tag`等とは異なり本文と同じ`font-size:1em`にする（2026-10時点の方針変更。以前は小さめpxだったが、視認性のためサイズアップした）。新規ページ作成時・既存ページ修正時とも、この1em版で統一する。
- 新規ページを作る際は、既存の`seminar_*.html`（特に`seminar_kiso.html`のトーン、`seminar_r.html`のパーツ一式：callout/note/tip/alert/codebox/resultbox/compare/steps等）からCSSとコーディング規約をそのまま踏襲する。
- `<head>`には`<meta charset>` → viewportメタタグ → `<link rel="icon" href="favicon.svg" type="image/svg+xml">`（タブアイコン、2026-10-08追加）の順で入れる。`favicon.svg`（ルート直下、`#2e5476`の円＋白い角帽シルエット）を既存ページからコピーする形で踏襲すれば自動的に引き継がれる。なお`labor_*.qmd`/`dataintro_*.qmd`等のQuarto生成ページは`_quarto.yml`の`website.favicon`で全ページに一括適用されるため、個別対応は不要。
- Rコード・練習用データ（`mpg`等）を扱う場合は、`seminar_r.html`の記述と数値・出力を完全に一致させる。
