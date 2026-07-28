# ===============================================
# 3回生ゼミ｜Rサンプルコード
# 詳しい説明はゼミ資料の R ページを参照
# ===============================================

# コードの実行: Ctrl + Enter
#   1行実行：実行したい行にカーソルを置いて
#   複数行実行：実行したい範囲を選択して

# ===============================================
# 第2回｜Rの基本操作・パッケージ
# ===============================================

# -----------------------------------------------
# 1. tidyverse のインストールと読み込み
#    ※インストールは最初の1回だけ実行すればOK
# -----------------------------------------------

install.packages("tidyverse")   # インストール（初回のみ）
library(tidyverse)               # 読み込み（毎回実行する）


# -----------------------------------------------
# 2. 基本演算
# -----------------------------------------------

1 + 1        # 足し算
10 - 3       # 引き算
4 * 5        # 掛け算
20 / 4       # 割り算
2^3          # べき乗（2の3乗）
10 %% 3      # 余り（10 ÷ 3 の余り）


# -----------------------------------------------
# 3. オブジェクト（変数への代入）
# -----------------------------------------------

x <- 10           # xに10を代入
y <- 3            # yに3を代入
x + y             # 計算できる
z <- x * y        # 計算結果を新しいオブジェクトに保存
z                 # 中身を確認


# -----------------------------------------------
# 4. ベクトル操作
# -----------------------------------------------

# ベクトルの作成
scores <- c(80, 65, 90, 72, 88)

# ベクトルの確認
scores
length(scores)    # 要素数
sum(scores)       # 合計
mean(scores)      # 平均
max(scores)       # 最大値
min(scores)       # 最小値

# ベクトルの要素を取り出す（インデックスは1から始まる）
scores[1]         # 1番目
scores[2:4]       # 2〜4番目


# ===============================================
# 第3回｜データのインポート・前処理
# ===============================================

# -----------------------------------------------
# 1. CSVファイルの読み込み
# -----------------------------------------------

# ※ファイルはプロジェクトフォルダに置いてから実行する
df <- read_csv("ocean_temperature.csv")

# 練習用：Rに組み込まれているデータを使う
df <- mpg      # 自動車の燃費データ（tidyverseに含まれる）
df             # データを表示
head(df)       # 変数の型と先頭数行を確認
nrow(df)       # 行数（サンプルサイズ）
ncol(df)       # 列数（変数数）


# -----------------------------------------------
# 2. パイプ演算子（%>%）と主要な関数
# -----------------------------------------------

# filter：条件に合う行を抽出
df %>% filter(year == 2008)                  # 2008年のデータのみ
df %>% filter(cyl >= 6)                      # 気筒数6以上
df %>% filter(manufacturer == "toyota")      # トヨタのみ

# select：列（変数）を選ぶ
df %>% select(manufacturer, model, year, hwy)

# mutate：新しい変数を作る
df %>% mutate(hwy_km = hwy * 1.609)          # マイル→kmに変換

# summarise：集計する
df %>% summarise(
  mean.hwy = mean(hwy),
  max.hwy = max(hwy),
  n = n()
)

# group_by：グループ別に集計
df %>%
  group_by(manufacturer) %>%
  summarise(mean.hwy = mean(hwy)) %>%
  arrange(desc(mean.hwy))    # 降順に並び替え


# -----------------------------------------------
# 3. 欠損値の確認
# -----------------------------------------------

is.na(df) %>% colSums()    # 各変数の欠損値の数を確認
df %>% filter(is.na(hwy))  # 欠損のある行を表示


# ===============================================
# 第4回｜記述統計・データの可視化
# ===============================================

# -----------------------------------------------
# 1. 記述統計
# -----------------------------------------------

# modelsummaryのインストール（初回のみ）
install.packages("modelsummary")
library(modelsummary)
install.packages("flextable")
library(flextable)

# 基本の記述統計（確認用。手早く全体を把握するのに便利だが、論文に載せる形式ではない）
summary(df)

# datasummary()で論文向けの記述統計表を作成
# 左辺に変数、右辺に統計量を並べて書く
datasummary(displ + hwy + cty ~ Mean + SD + Min + Max + N, data = df)

# Word形式で保存（Filesタブに出力される）
datasummary(displ + hwy + cty ~ Mean + SD + Min + Max + N,
            data = df,
            output = "flextable") %>%
  save_as_docx(path = "descriptive_stats.docx")


# -----------------------------------------------
# 2. ggplot2による可視化
# -----------------------------------------------

# ggplot2のグラフはさまざまなオプションで色・サイズ・テーマなどを
# カスタマイズできます。必要なときにAI等で調べてみてください。

# ヒストグラム（1変数の分布）
ggplot(df, aes(x = hwy)) +
  geom_histogram(binwidth = 2)

# 散布図（2変数の関係）
ggplot(df, aes(x = displ, y = hwy)) +
  geom_point()

# 棒グラフ（グループ別の平均）
# ※ stat = "identity"：summarise()で計算した値をそのまま棒の高さに使う指定
df %>%
  group_by(class) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  ggplot(aes(x = class, y = mean_hwy)) +
  geom_bar(stat = "identity")

# 箱ひげ図（グループ別の分布）
ggplot(df, aes(x = class, y = hwy)) +
  geom_boxplot()

# 最後に表示したグラフを保存
ggsave("my_plot.png")

# グラフに名前を付けて保存
p <- ggplot(df, aes(x = displ, y = hwy)) +
  geom_point()
ggsave("my_plot.png", plot = p)

# ※ width・height（サイズ）やdpi（解像度）も指定できる
#   例：ggsave("my_plot.png", plot = p, width = 8, height = 5, dpi = 150)


# ===============================================
# 第5回｜回帰分析（単回帰・重回帰）
# ===============================================

# modelsummaryとflextableは第4回で読み込み済み。
# セッションを開き直した場合のみ、下の library() を再実行する
library(modelsummary)
library(flextable)

# -----------------------------------------------
# 1. 単回帰分析
# -----------------------------------------------

# モデルの推定
model1 <- lm(hwy ~ displ, data = df)

# 結果の確認
summary(model1)

# 結果の読み方：
#   Estimate（係数）：Xが1増えるとYがどれだけ変化するか
#   Pr(>|t|)（p値）：一般的には0.05未満なら統計的に有意
#   R-squared：モデルの当てはまりの良さ（0〜1）


# -----------------------------------------------
# 2. 重回帰分析
# -----------------------------------------------

model2 <- lm(hwy ~ displ + cyl, data = df)
model3 <- lm(hwy ~ displ + cyl + factor(year), data = df)
# factor()で変数をカテゴリ型に変換すると、lm()が自動的にダミー変数に
# 変換して回帰式に組み込む。年など、数値ではなくカテゴリとして扱いたい
# 変数に使う

summary(model2)
summary(model3)


# -----------------------------------------------
# 3. 結果表の作成（modelsummary）
# -----------------------------------------------

# 複数モデルをまとめて表示
# ※ msummary()という書き方も見かけるが、modelsummary()と同じ関数
modelsummary(
  list("モデル1" = model1,
       "モデル2" = model2,
       "モデル3" = model3),
  stars = TRUE,          # 有意水準の星印を表示
  coef_omit = "factor",  # factor() で作ったダミー変数を出力しない
  gof_omit = "IC|Log|F"  # 不要な適合度指標を省略
)

# Word形式で保存（Filesタブに出力される）
# ※ output = "regression_table.docx"と直接ファイル名を書く方法は
#   modelsummary v2.0以降では動かない。
#   output = "flextable" + flextable::save_as_docx()を使用すること
modelsummary(
  model1,
  output = "flextable"
) %>% flextable::save_as_docx(path = "regression_table.docx")

# Power Point形式で保存するときは最後を save_as_pptx() 関数に変える
modelsummary(
  model1,
  output = "flextable"
) %>% flextable::save_as_pptx(path = "regression_table.pptx")
