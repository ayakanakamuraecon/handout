library(tidyverse)

# 労働力率、国別
# データの読み込み（Shift-JISのCSVファイル）
df <- read_csv("labor_force_by_country_female.csv", locale = locale(encoding = "shift-jis"))

df_long <- df %>%
  select("age_group", "JPN", "USA", "UK", "DEU", "ITA", "SWE", "KOR") %>%
  pivot_longer(
    cols = -age_group,
    names_to = "country",
    values_to = "rate"
  ) %>%
  filter(
    age_group!="65-69" & age_group!="70-74" & age_group!="75+" & age_group!="15-64" & age_group!="15+" 
  )

df_long$age_group <- factor(df$age_group, levels = unique(df$age_group))

# グラフ描画
p <- ggplot(df_long, aes(x = age_group, y = rate, group = country, color = country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "年齢階級別 女性労働力率（2021）",
    x = "年齢階級",
    y = "労働力率（%）",
    color = "国名"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# PNGファイルとして保存（横長）
ggsave(
  filename = "images/labor_workforce.png",
  plot = p,
  width = 12,       # 横幅（inch）
  height = 6,       # 縦幅（inch）
  dpi = 300         # 解像度
)



# 労働力率、男女別
# データの読み込み（Shift-JISの場合）
df <- read_csv("labor_force_by_age_sex.csv", locale = locale(encoding = "Shift-JIS"))

# データの構造確認
glimpse(df)

# 1. 男性データ（列B〜L → 年齢階級は2列目以降の11列）
df_male <- df %>%
  select(年齢階級, starts_with("男")) %>%
  pivot_longer(
    cols = -年齢階級,
    names_to = "age",
    values_to = "rate"
  ) %>%
  rename(year = 年齢階級) %>%
  mutate(
    sex = "男性",
    age = str_remove(age, "^.：")
  )

# 2. 女性データ（列M〜W → 12〜22列目）
df_female <- df %>%
  select(年齢階級, starts_with("女")) %>%
  set_names(c("year", names(df)[13:23])) %>%
  pivot_longer(
    cols = -year,
    names_to = "age",
    values_to = "rate"
  ) %>%
  mutate(
    sex = "女性",
    age = str_remove(age, "^.：")
  )

# 3. 結合
df_long <- bind_rows(df_male, df_female) %>%
  select(year, age, sex, rate)

# 4. グラフ描画
p <- ggplot(df_long, aes(x = age, y = rate, color = factor(year), group = year)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ sex) +
  labs(
    title = "男女別・年齢階級別 労働力率の推移",
    x = "年齢階級",
    y = "労働力率（％）",
    color = "年"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )
print(p)

# PNGファイルとして保存（横長）
ggsave(
  filename = "images/labor_workforce.png",
  plot = p,
  width = 12,       # 横幅（inch）
  height = 6,       # 縦幅（inch）
  dpi = 300         # 解像度
)
