library(tidyverse)

# データの読み込み（Shift-JISの場合）
df <- read_csv("labor_force_rate_by_age_sex.csv", locale = locale(encoding = "Shift-JIS"))

# データの構造確認
glimpse(df)

# ロング形式に変換（仮に列名が年になっていると仮定）
df_long <- df %>%
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),  # 年の列
    names_to = "year",
    values_to = "rate"
  ) %>%
  rename(
    age = 年齢階級,  # 年齢列の名前が違う場合は適宜修正
    sex = 性別        # 性別列も同様に
  ) %>%
  mutate(year = as.integer(year))

# 結果の保存（Shift-JISで出力）
write_csv(df_long, "labor_force_rate_long.csv", na = "", append = FALSE)
