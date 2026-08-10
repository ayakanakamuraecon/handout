# HANDOFF

作業を再開する際は、着手前に必ずこのファイルを読むこと（`/Dropbox/CLAUDE.md` 参照）。

**最終更新**: 2026-08-10 / 家PC（THINKPAD-AURA）

## 目的・背景

和歌山大学向け講義資料サイト（Quarto製、`docs/`配下がビルド成果物、GitHub Pagesで公開）の整備。主な目的は2つ。

1. `seminar_*.html`群のデザインを、Quarto生成ページ（`labor_*`, `dataintro_*`）にも統一適用すること。
2. `labor_*.qmd`が参考文献（大森義明・永瀬伸子『労働経済学をつかむ』有斐閣）の文言に近すぎる箇所を、意味・数値・設例（タカシ/マミ/ハナコ等の固有名詞は維持）を保ったまま言い換え、著作権上のリスクを下げること。あわせて、`.seminar-steps`（丸数字ステップ）等を使ってスライドのように読みやすい構造に整えること。

## TODO

- [x] `seminar-theme.scss`にデザインを一本化（配色・見出し階層・callout・引用ブロック・確認クイズボックス・図表白背景化など）。`quiz.html`の重複styleも解消（2026-08-06）
- [x] labor_intro / labor_supdem / labor_hc / labor_search の言い換え＋構造化（2026-08-06、個別＋全体で`quarto render`検証済み）
- [x] `dataintro_*.qmd`は言い換え対象外と判断（ユーザー確認: 教科書からの引き写しではなくオリジナル資料のため、2026-08-06）
- [x] `HANDOFF.md`が公開サイトに巻き込まれる問題を`_quarto.yml`の除外設定で解消（2026-08-06）
- [x] ブラウザでの全体の見た目最終確認（2026-08-10、家PCでユーザー確認済み。以後変更した分は都度再確認が必要）
- [x] デザイン統一＋言い換え作業一式をコミット（`117a46d`、家PC 2026-08-10）
- [x] labor_intro.qmdの構造化4件（箇条書き化2件、seminar-tip変換、メリット/デメリット表記）＋HANDOFF.md刷新をコミット（`87ce0bb`、家PC 2026-08-10）
- [ ] ローカルコミット（`117a46d`, `87ce0bb`）を`origin/main`へpushするか判断（ユーザー未確認）
- [ ] `labor_search.qmd`末尾のコメントアウトされた未使用下書き（「失業と不安定雇用」セクション）の扱いを検討（要ユーザー相談。現状は非表示のまま放置でよい）
- [ ] labor_intro.qmd のさらなる構造化（「スライド資料のように、一目で認知しやすく読みやすい資料に」というユーザーの意図に基づく作業。以下は洗い出し済みの候補）
  - [x] 「賃金率」節、生産性差に関する3つの問いかけを箇条書き化（2026-08-10）
  - [x] 「統計における働き方の分類」節、正社員/パートの賃金格差の対比を箇条書き化（2026-08-10）
  - [x] 「調査分析の方法」表（横断面分析/時系列分析/パネル分析、4列）を、代表的な労働統計3種と同様の`.seminar-tip`×3に変換（2026-08-10）。ラベルは「主な用途/留意点」ではなく「**メリット**/**デメリット**」（太字）に変更（2026-08-10）
  - [ ] 「労働力率」の定義（労働力＝就業者＋完全失業者）を`.callout-note`で定義ボックス化
  - [ ] 「労働時間」節の有償労働／無償労働の定義を`.callout-note`化（1文に2つの定義が詰め込まれていて読みにくい）
  - [ ] 「賃金率」の定義を`.callout-note`化
  - [ ] 「賃金率」節、正規雇用者の給与構成（所定内給与→超過労働給与→賞与の3区分）を`.seminar-steps`化（labor_hcの「後払い賃金の3つの狙い」と同型のパターン）
  - [ ] 「統計における働き方の分類」節、雇用形態区分の歴史的経緯（1980年代→1990-2000年代→2000年代初頭の時系列）を`.seminar-steps`化
  - [ ] 「日本の労働統計」節、「世帯への調査」vs「企業への調査」の比較を構造化（表組みか`.seminar-tip`か、着手時にユーザーと相談）
  - この続きに着手する場合は、まずlabor_intro.qmdを読んで上記候補の該当箇所を確認してから進めること（言い換え後の文言が変わっている可能性があるため、行番号ではなく見出し・内容で照合する）

## 直近の意思決定とその理由

- 内容修正時は必ず`quarto render`までセットで行う方針にした。理由: ユーザー指示（2026-08-10）。編集直後にHTMLへ反映されている状態を保つため。
- セッション中に判明した環境情報（git/quarto導入状況、Dropbox関連の不具合対処など）は、Claude Codeのローカルメモリではなくこの`HANDOFF.md`や`/Dropbox/CLAUDE.md`に書く方針にした。理由: ローカルメモリは家PC・職場PC間でDropbox同期されないため（`/Dropbox/CLAUDE.md`参照）。
- PC固有の情報を書くときは「このPC」ではなくホスト名／ニックネームで識別する方針にした（`/Dropbox/CLAUDE.md`の「PCの識別」参照）。

## 環境メモ（家PC / THINKPAD-AURA、2026-08-10時点）

- git: `C:\Program Files\Git\cmd` にインストール済み。PowerShellセッションのPATHに自動反映されないため、都度 `$env:Path += ";C:\Program Files\Git\cmd"` が必要。
- git identityが未設定だったため設定済み: `user.name "Ayaka Nakamura"` / `user.email "ayaka.nakamura.oc@gmail.com"`（`--global`）。
- quarto: `winget install --id Posit.Quarto` で導入（`C:\Program Files\Quarto\bin`）。同様にセッション内でのPATH追加が都度必要: `$env:Path += ";C:\Program Files\Quarto\bin"`。
- git commit時、Dropbox同期起因とみられる `.git/logs/HEAD` への書き込みでコマンドがタイムアウトすることがあるが、実際にはコミット自体は成功していることが多い。タイムアウトしたら `git log` / `git status` で実際の結果を確認してから再実行を判断する（二重コミット防止）。`core.fsyncMethod fsync` / `windows.appendAtomically false` を設定済み。
- `quarto render` 時、Dropboxの「クラウド ファイル プロバイダーが実行されていません」(os error 362) で `site_libs` 配下のコピーに失敗することがある。対処: Dropboxプロセスを再起動する（`Stop-Process -Name Dropbox -Force` → `${env:ProgramFiles(x86)}\Dropbox\Client\Dropbox.exe` を起動）。
- `.quarto` キャッシュフォルダが壊れて disk I/O エラーになることがある。中身は再生成可能なキャッシュなので、フォルダごと削除すれば直る。

## 未解決の懸念・保留事項

- `quarto render` 実行時、ロックされたファイル（`site_libs` 配下など）の削除に失敗する警告が頻発する。実害はなく出力は毎回正常に生成されているが、原因（Dropboxの同期プロセスなど）が気になる場合は要調査。
- git操作（status/commit）がDropbox同期プロセスとの競合で一時的にハング・タイムアウトすることがある（上記参照）。頻発する場合は `.git` フォルダもDropbox同期除外（`com.dropbox.ignored`）の対象にすることを検討してもよい。
- 職場PCのホスト名が未登録（`/Dropbox/CLAUDE.md`の「PCの識別」表を参照。次回職場PCで作業する際に追記する）。

## 解決済み（過去の経緯）

- `.quarto` フォルダのDropbox選択型同期競合コピー問題: `.quarto` に `com.dropbox.ignored` 属性を設定し、Dropbox同期対象から除外することで解消（2026-08-05）。職場PC側でも未設定なら同様の対応が必要:
  `cmd /c "echo 1 > .quarto:com.dropbox.ignored"`
- `docs/styles.css` に誤って手動CSSを書き込んでいた問題: 内容を `seminar-theme.scss` に統合し、`docs/styles.css` を本来の空スタブに戻すことで解消（2026-08-06）。
- `dataintro_estimation.html`（リポジトリ直下、`docs/`配下ではない方）が作業ディレクトリ上で削除扱いになっていた件: ユーザー確認の結果、対応不要と判明（`.qmd`が残っており `quarto render` で `docs/` 配下に正しく生成されるため）（2026-08-06）。
- `seminar_bunken.html`（直下）と `docs/seminar_bunken.html` の、無関係な未コミット変更: 別PCでの作業によるものとユーザーが確認済み。現状のものを最新として維持し、触らない（2026-08-06）。
