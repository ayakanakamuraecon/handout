[講義資料](https://ayakanakamuraecon.github.io/handout/index.html)で公開している講義資料のファイルです。

## seminar_*.html のデザイン規約

`seminar_*.html`（`seminar_kiso.html` `seminar_r.html` `seminar_tokei.html` 等、Quartoを経由しない手書きHTML＋インラインCSSのページ群）は、`docs/`配下がビルド成果物、ルート直下のファイルが原本。

- 本文（地の文）を運ぶボックスパーツ（`.callout` `.note` `.alert` `.tip` `.compare-box` など）のフォントサイズは、固定pxで個別に小さくせず、`body`（16px）と同じ`font-size:1em`にする。
  - 理由：各ボックスに13.5px〜14.5px等の値が個別にハードコードされ、`body`のデフォルト16pxより小さく見える不具合が複数回発生したため。
  - 一方、キャプション（`.table-caption`）・コード（`.codebox` / `code`）・ナビ/タグ的UI要素（`.jumpnav a` `.related-nav a` `.toc-return`）は本文ではなくメタ情報/UI要素なので、これまで通り小さめのpxのままでよい。
- `.note-label`（補足・まとめ等のラベル見出し）は、上記のメタ情報/UI要素の例外として、`.tag`等とは異なり本文と同じ`font-size:1em`にする（2026-10時点の方針変更。以前は小さめpxだったが、視認性のためサイズアップした）。新規ページ作成時・既存ページ修正時とも、この1em版で統一する。
- 新規ページを作る際は、既存の`seminar_*.html`（特に`seminar_kiso.html`のトーン、`seminar_r.html`のパーツ一式：callout/note/tip/alert/codebox/resultbox/compare/steps等）からCSSとコーディング規約をそのまま踏襲する。
- Rコード・練習用データ（`mpg`等）を扱う場合は、`seminar_r.html`の記述と数値・出力を完全に一致させる。
