# 記述統計（編集中）

この章では、生のデータを加工し、情報を集約することで、データの特徴を分かりやすく表現する方法を学んでいきます。

このように、手元のデータの特徴や傾向を明らかにすることを**記述統計**といいます。

具体的には、**表やグラフを作成**したり、**統計量を計算**します。

データは、[はじめに](dataintro_intro.html)で説明している**過去の学祭の売り上げデータ**を使っていきます。それぞれの変数が何を表しているのかなどは、そちらを参照してください。

ファイルを開くと分かるように、生のデータは数字の羅列にすぎません。

[https://www.notion.so](https://www.notion.so)

↑データheadの画像

以下では、具体的にどのように加工すると分かりやすくなるのかを学んでいきます。

ちなみに、こちらも[はじめに](dataintro_intro.html)で説明しているように、生のデータファイルをそのまま操作することは避けましょう。ダウンロードしたファイルはコピーを作成し、生のデータと操作用データを別々に管理するか、新しく操作用データを作成して、ブックリンクでデータを参照するようにしましょう。

# 表とグラフ

## 度数分布表

学祭の売り上げ向上を目指すためには、まず現状を知る必要があります。ここでは、過去の学祭で、1回の買い物あたりの売上金額がどのように分布しているかを見てみましょう。売上金額（`sales`変数）を$100$刻みで区切って表にまとめるだけでも見やすくなります。これを**度数分布表**といいます。

[https://www.notion.so](https://www.notion.so)

↑度数分布表を貼り付け

度数分布表は、以下のような項目で構成されています。

##### 階級
まとめる値の範囲（ここでは100円刻みですが、扱うデータの単位によって適切な範囲を決める必要があります）

（例）100以上200未満の階級、200以上300未満の階級など

- 階級値

その階級を代表する値であり、各階級の中央の値のこと

（例）100以上200未満の階級の階級値は150

##### 度数
階級内に所属する観測値の数

##### 相対度数
サンプルサイズに占める度数の割合 $\left(= \frac{度数}{サンプルサイズ}\right)$

##### 累積度数
1番小さい階級からその階級までの相対度数を足し合わせたもの

### Excelで度数分布表を作る

度数分布表は、データの特徴をつかむための第一歩です。

Excelは、セルに「`= 2 + 3`」や「`= SUM(A1:Z100)`」を入力することで、つまり、**「`= 計算式`」や「`= 関数`」を入力する**ことで、計算結果を表示してくれます。
Excelで度数分布表を作成するときは、度数、相対度数、累積相対度数を次のような関数を使って計算していきます。

[https://www.notion.so](https://www.notion.so)

##### 度数
`COUNTIF`関数を使い、**「階級の上限値未満の観測値の個数」**から**「階級の下限値未満の観測値の個数」**を引く

（例）100以上200未満の階級の度数$=$200未満の観測値の個数$-$100未満の観測値の個数$=$100以上200未満の観測値の個数

###### `COUNTIF`関数

`COUNTIF(データの範囲, "条件")` 

（例）100未満の観測値の個数：`= COUNTIF($$:, "<100")`

- データの範囲は、普通に選択しただけだと、他のセルに関数をコピペしたときに、参照元のセルとコピペ先の位置関係に応じてデータの範囲も移動してしまいます。同じ関数を何回か使う必要があり、その際にデータの範囲を固定したいときは、列と行の英数字の前に`$`を付けると便利です。
    - 列と行を固定したいとき：`$A$1:$B$20`
    - 列を固定したいとき：`$A1:$B20`
    - 行を固定したいとき：`A$1:B$20`
    
    関数を入力しているセルを選択している状態でF4を押すと切り替えられます。
    
- 条件には比較演算子（数字の大小を比較する記号）を使います。これらの記号は、他のプログラミング言語でも共通なので、ぜひ覚えてください。
    
    
    | `A < B` | AはBより小さい |
    | --- | --- |
    | `A > B` | AはBより大きい |
    | `A <= B` | AはB以下 |
    | `A >= B` | AはB以上 |
    | `A = B` | AとBは等しい |
    | `A != B` | AとBは等しくない |

##### 相対度数

すでに計算した度数セルと`SUM`関数を使い、 $\frac{度数}{サンプルサイズ}$を計算する

###### `SUM`関数

`SUM(データの範囲)` 
（例）150の階級値の相対度数：`= SUM(:)`

##### 累積相対度数
それまでの累積相対度数とその階級の相対度数を足す
（例）

- 100以上200未満の階級の累積相対度数：`=`
- 200以上300未満の階級の累積相対度数：`=`

すべての階級についてそれぞれの値を計算することで、度数分布表を作成できます。

[https://www.notion.so](https://www.notion.so)

↑出来上がった度数分布表を挿入

これで、売上金額の分布を把握することができました。

## データのビジュアル化

度数分布表はデータの特徴をとらえるのに便利ですが、さらに「見える化」することで、よりデータの特徴をとらえやすくなります。

次は、度数分布表を見える化した**ヒストグラム**と、その他の代表的なグラフを作成していきましょう。

### ヒストグラム

階級値を横軸に、度数を縦軸にとったグラフを**ヒストグラム**といいます。

[https://www.notion.so](https://www.notion.so)

[https://www.notion.so](https://www.notion.so)

←度数分布表　ヒストグラム→

どちらにも階級値と度数の情報は含まれますが、ヒストグラムの方がより視覚的にデータの特徴をつかめることが分かると思います。

> グラフからわかること。。。
> 

> グラフからわかること。。。
> 

といったことが読み取れるのではないでしょうか。

#### Excelでヒストグラムを作る

Excelでは、生のデータから直接ヒストグラムを作ることができます。

[https://www.notion.so](https://www.notion.so)

[https://www.notion.so](https://www.notion.so)

1. データを選択した状態で「挿入」→「グラフ」→「ヒストグラム」を選択する（左図）
2. バー（ビンと呼ぶ）の幅を変えたいときは、グラフを右クリックし、「グラフエリアの書式設定」→「軸のオプション（横 項目軸）」→ヒストグラムアイコンをクリック→「ごみ箱の幅」（ビンのこと）を階級の幅に変更する（右図）

#### ヒストグラムと棒グラフ

ヒストグラムとよく似たグラフに、**棒グラフ**がありますが、実はこの2つはさまざまな点で異なります。

|  | **ヒストグラム** | **棒グラフ** |
| --- | --- | --- |
| **データの種類** | 連続した数値データ | カテゴリデータ |
| **横軸の順番** | 連続した順番で並べる | 決まりはない |
| **縦軸** | 度数 | 度数に限らない |
| **用途** | 分布をみる | 数量の大小を比較する |
| **バーが示すもの** | バーの**面積**が度数（or 相対度数）を示す | バーの高さが数字の大小を示す |
| **棒の間の間隔** | なし | あり |

例えば、同じ人口を表すとしても、年齢階級ごとの人口や所得階級ごとの人口などを表す際にはヒストグラムが適切であるのに対し、地域別人口や誕生月別人口を表す際には棒グラフが適切です。

### 箱ひげ図

ヒストグラムと並んで使いこなせるようになってほしいグラフが**箱ひげ図**です。

[https://www.notion.so](https://www.notion.so)

↑売上金額箱ひげ図

四角い箱とその両端から生えているひげから構成されているグラフで、データのばらつきを示すのに用いられます。

箱ひげ図は四分位数と最大値・最小値を表しています。小さい方から見て

- 最小値：ひげの下端
- 第1四分位数（下位25％の値）：箱の下底
- 第2四分位数（下位50％の値、中央値）：箱の中央の線
- 第3四分位数（下位75％の値）：箱の上底
- 最大値：ひげの上端

で表されています（下図参照）。

[https://www.notion.so](https://www.notion.so)

↑箱ひげ図の図解

ヒストグラムでもデータのばらつきを見ることはできますが、分布の情報を四分位数に集約し、情報を落とすことで、**より分布のばらつきに注目できる点**、**複数のデータの比較が容易な点**がメリットです。

#### Excelで箱ひげ図を作る

[Excelでヒストグラムを作る](#excelでヒストグラムを作る)の手順1で、ヒストグラムの代わりに箱ひげ図を選択すれば、簡単に箱ひげ図を作成できます。

[https://www.notion.so](https://www.notion.so)

↑箱ひげ図を選択する画像

### その他の代表的なグラフ

他に、よく使われる代表的なグラフを紹介します。それぞれのグラフが得意なデータがあるので、データの特性に合わせて適切なグラフを選ぶことが重要です。

ちなみに、データの特性を分かりやすく伝えるためには、余分な装飾（3Dにする、影を付ける、過度にカラフルにするなど）は省くことが望ましいです。できるだけシンプルで色数を抑えたグラフになるよう意識しましょう。

逆に言えば、装飾が多いグラフは、データの特性を覆い隠しているケースもあります。グラフを解釈するときには、そのグラフが何を伝えたいグラフなのかを考えながら解釈するのが良いでしょう。

#### 折れ線グラフ

数的データの**時間的な変化**を示す際に用いられるグラフです。

以下のグラフは、学祭の売り上げデータから作成した、時間当たりの売り上げです。

[https://www.notion.so](https://www.notion.so)

他にも、例えば年別の人口推移、月別の飛行機のチケット代の推移、年別のたまごの価格の推移など、時系列データの表示に適しています。

> **注意点**
> 
> - 複数のデータを重ねるときは、色分けや目盛りを工夫する
> 
> （例）
>     - 線の区別がつきやすいよう、色分けをしたり、実線と破線を使い分けたりする
>     - 縦軸の目盛りが$0$から始まると変化がとらえづらいときは、適宜目盛りを省略する
>     - 数値が大きく異なるグラフを重ねるときは、一方の目盛りを右側に配置する

#### 円グラフ・帯グラフ

カテゴリデータの各項目が全体に占める割合を示す際に用いられるグラフです。

以下のグラフは、学祭の売り上げデータから作成した、購入品の内訳です。

[https://www.notion.so](https://www.notion.so)

[https://www.notion.so](https://www.notion.so)

↑円グラフ＆帯グラフ

他にも、国籍別外国人の割合、血液型、男女比などのデータを表示する際に適しています。

> **注意点**
> 
> - カテゴリは、（円グラフのときは）12時の位置から時計回りに、（帯グラフのときは）左から順に、大きい順で並べる
>     - その他は1番最後
> - 面積が小さいと見づらいので、割合が小さいものはまとめてその他にする
> - データに順序（例えば、「とても満足」「満足」「不満」「とても不満」など）がある場合は、割合順ではなくデータの順序通りに並べる

#### Excelでさまざまなグラフを作る

[Excelでヒストグラムを作る](#excelでヒストグラムを作る)の手順1で、ヒストグラムの代わりに作りたいグラフを選択することで、さまざまなグラフを作ることができます。

[https://www.notion.so](https://www.notion.so)

[https://www.notion.so](https://www.notion.so)

折れ線グラフと円グラフを選択する画像

# 統計量

ビジュアル化は便利ですが、問題点もあります。

##### グラフから受ける印象が人によって異なってしまう

同じヒストグラムを見ても、データがまんべんなく分布していると感じる人もいれば、まとまって分布していると感じる人もいるでしょう。

##### 複数のデータを比較することが難しい

それぞれのデータの特徴はどの程度似通っていて、どの程度異なるのか、その判断も人によって異なるでしょう。

このような問題点を解決するために、グラフとともに**統計量**（データの特徴を数値に集約すること）を計算していくことが大切です。

ここでは、代表的な統計量として、**平均**、**分散**、**標準偏差**、**共分散**、**相関係数**の計算方法を学びます。

## 平均

**平均**は、データの中心（重心）を表す数値です。

$n$個の観測値からなるデータ$(x_{1}, \cdots, x_{n})$の平均$\bar{x}$は、

$$
\bar{x} = \frac{1}{n}(x_{1} + \cdots + x_{n}) = \frac{1}{n}\sum_{i=1}^{n}x_{i}
$$

で求められます。つまり、**観測値の値をすべて足し、それを観測値の個数（データサイズ）で割ったもの**です。

似たような統計量に**中央値**がありますが、両者の間にはいくつかの違いがあります。

##### 平均

- データは平均の周辺に分布する
- 多く現れる値に影響を受ける
- 極端な値に**影響を受ける**

##### 中央値

- データを小さい順に並べたときに真ん中に位置する値
- 極端な値に**影響を受けにくい**

## 分散と標準偏差

## 共分散と相関係数
