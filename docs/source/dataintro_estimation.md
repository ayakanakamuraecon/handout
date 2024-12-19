# 推測統計（編集中）

この章では、手元のデータが調査したい対象（**母集団**）全体の情報を含んでいないときに、**手元のデータから母集団の特徴や性質を推測する方法**を学びます。

このことを**推測統計**といいます。

まずは、推測統計とは何か？を理解してから、具体的な手法を学んでいきましょう。

データはこれまで通り、[はじめに](dataintro_intro.html)で説明している**学食の売り上げデータ**を使っていきます。それぞれの変数が何を表しているのかなどは、そちらを参照してください。

ちなみに、この章の内容を深く理解するためには確率の知識が必要不可欠です。この文章では実践に重きを置いているため確率には触れていませんが、統計学入門の復習をしたり、もっと詳しく理解したい方は**や**といった、学部中級レベルの統計の教科書を読むことをオススメします。

# 推測統計とは？

そもそもデータ分析をする理由に立ち返ってみます。当たり前ですが、**データ分析の目的は、データを使って何かしらの問題を解決したり、現状を改善したりすること**です。

私たちは、現在**学食の売上向上プロジェクト**に参加しています。学食の売上向上という目標のためには、キャンペーンを打ち出すのが良いのか、広告を出すのが良いのか、営業時間を変更するのが良いのか、頻繁に新メニューを開発するのが良いのか、そして、それぞれの施策はどれほど売上向上に寄与するのか、を知りたいのです。

そのためには、**母集団の情報を知る必要があります**。学食の売上向上プロジェクトにおいては、学食を利用する可能性があるすべての人の情報です。母集団全体の情報を知ることができれば、例えば、実際に広告を出したときにどの程度売り上げが変化したのかを測ることができます。

**母集団のすべての対象を調査すること**を**全数調査**と呼びます。

しかし、現実には**母集団全体の情報を手に入れられるケースはほとんどありません**。そこで、多くの場合、**母集団から適切に抽出（サンプリング）した標本（データ）を用いて、母集団の情報を推測しよう**とします。

このように、**標本から母集団の情報を推測すること**を**推測統計**と言います。

推測統計には、大きく分けて**統計的推定**と**仮説検定**の2つの方法があります。

#### 統計的推定

**母集団の統計量**（平均や分散など）**を、データから統計的に推測すること**を指します。

例えば、

- 手元のデータでは1回あたりの売り上げ平均が500円だったので、母集団の1回あたりの売り上げ平均も500円だろうと推測する。
- 手元のデータでは購入者の男性割合が $55\%$ だったので、母集団の男性割合は $54 ~ 56\%$ だろうと推測する。

こういった推測が統計的推定です。

推定には、**1つの値で推定する点推定**と、**ある程度の幅をもって推定する区間推定**があります。

#### 仮説検定

**母集団に関する仮説が成り立つか否かを、統計的な手続きを踏んで、データから判断すること**を指します。

例えば、

- 木曜日より火曜日の方が売り上げが大きいかどうか
- ある広告を出すことによって売り上げが変化するかどうか

こういった仮説を検証する手続きが仮説検定です。

## サンプリングと標本の代表性

推定をするにしても、仮説検定をするにしても、**標本が母集団の情報を正しく反映している（標本に代表性がある）かどうかが重要**なポイントになります。

この重要性は、代表性がないケースを想像すると分かりやすいでしょう。

例えば、学食を利用する可能性があるすべての人を母集団として想定しているにもかかわらず、手元には経済学部の教員の購入データしかないとします。

学食は主に学生が利用することが多いでしょうから、母集団の平均年齢を推定したいときに、手元のデータの平均年齢を母集団の平均年齢と見なすことはできません。

また、学生の行動は時間割に左右されていて、ほとんどの学生が昼休みに学食を利用するのに対し、教員は混雑を避けて昼休み前後に利用時間をずらす傾向があるかもしれません。この場合、手元のデータの利用時間のピークを母集団の利用時間のピークとは見なせません。

さらに、学生と教員では予算にも差があるかもしれません。例えば学生は1食あたり500円以内に収めたいと考えている一方、教員は1食あたり700円までは使って良いと考えているかもしれません。この予算の差は1回あたりの購入金額に表れる可能性があるため、データから得られた平均購入金額を、母集団の平均購入金額と見なすことは難しいでしょう。

このような誤った推測を避けるために、**標本が代表性をもつようにバランスよくサンプリングする必要**があるのです。

逆に言うと、標本が代表性をもっていさえすれば、標本平均を母集団の平均だと見なしたり、標本分散を母集団の分散だと見なすことが可能です。

**母集団の情報を推測するために使う数値**（例えば、母集団の平均を推測するために用いるデータの標本平均のこと）**を推定量と呼びます**。

### ランダムサンプリング

標本が代表性をもつようにバランスよくサンプリングする方法が**ランダムサンプリング（無作為抽出）**です。

ランダムサンプリングは、**母集団に含まれる調査対象のそれぞれが、標本として抽出される可能性を等しくしたうえで標本を選ぶ方法**です。

例えば、学食を利用する可能性があるすべての人に $1/100$ の確率で当たる（標本として抽出される）くじを引いてもらうようなイメージです。

ランダムサンプリングを行うことで、母集団の情報をバランス良く反映した標本を作ることができます。

[https://www.notion.so](https://www.notion.so)

↑ランダムサンプリングのイメージ図

代表性のある標本を作ることができたら、標本から得られた数値を推定量にすることができます。

ただし、母集団の情報をすべて集めているわけではない以上、**推定量には必ず誤差が生じます**。**推測をする際には、この誤差も考慮に入れる必要があります**。

## 標準誤差

母集団の情報を推測する際に用いる**推定量のばらつきを標準誤差（Standard error; SE）と呼びます**。

言い換えると、標準誤差は**推定量の標準偏差**です。一般的には**標本平均の標準偏差**を意味します。

名前は似ていますが、**標準偏差と標準誤差は別物なので注意**しましょう。

標準誤差は**推定の精度を表す指標**で、**標準誤差が小さいほど、推定量はばらつきが小さく推定の精度が高い**ことを示します。

データ$(x_{1}, \cdots, x_{n})$を用いた**標準誤差$SE$は**、

$$
SE = \frac{s}{\sqrt{n}} = \frac{\sqrt{\frac{1}{1-n}\sum_{i=1}^{n}(x_{i} - \bar{x})^2}}{\sqrt{n}}
$$

で求められます。つまり、**不偏分散をサンプルサイズで割ったものの平方根**です。

不偏分散については、[記述統計（ `.P`関数と`.S`関数）](#.P関数と.S関数)を参照してください。

式を見て分かるように、$SE$の分母にはサンプルサイズが入っています。**標本のサンプルサイズが大きくなると**標準誤差は小さくなる、つまり、**精度の高い推定ができる**ということです。

例えば、手元のデータは1万人の母集団からランダムサンプリングしたものであるとし、手元のデータから母集団の平均購入金額を推測するとします。

ランダムサンプリングを複数回行うケースを考えてみると、1回あたりのサンプルサイズが100人のとき、例えば最初の100人の平均購入金額は500円、2回目の100人の平均購入金額は530円、3回目の100人の平均購入金額は490円と、標本ごとに平均購入金額が大きく異なることもあるでしょう。

ところが、1回あたりのサンプルサイズが1000人だとすると、例えば最初の1000人の平均購入金額は501円、2回目の1000人の平均購入金額は502円、3回目の1000人の平均購入金額は499円と、標本ごとの平均購入金額はそこまで大きく変わらないと想像できます。

1回あたりのサンプルサイズが2000人、3000人、…と増えていけば、さらに平均購入金額のばらつきは小さくなっていき、1万人になると母集団の真の平均購入金額をピッタリ推測することができます。

これが、サンプルサイズが大きいと精度高く推測できるというイメージです。

### 標準誤差を考慮した棒グラフ

標準誤差を用いると、手元のデータで得られた統計量の精度はどの程度なのかを知ることができました。

この推定精度を考慮したグラフを作成することで、より深い洞察が得られる場合があります。

例えば、男女間で平均購入金額に差があるかを知りたい、気温によってよく購入される商品の違いがあるのかを知りたいなど、特に**グループ間の平均に統計的に差があるかどうかを知りたいとき**にとても有用です。

[https://www.notion.so](https://www.notion.so)

↑男女別平均購入金額の差

これは、標準誤差を考慮したうえで男女別に平均購入金額を棒グラフにしたものです。

**棒グラフの高さは、男性、女性それぞれの平均購入金額**を表します。

それぞれのバーには、**バーの高さを中心にエラーバーと呼ばれるひげのようなもの**がついています。これが**誤差範囲**を表します。

**データから算出された標本平均がバーの高さにあるならば、母集団の真の平均購入金額は、おおよそこの誤差範囲のどこかに分布しているだろう、という範囲**を示しています。この区間を**信頼区間**と呼びます。

**信頼区間を求めること**は、推測統計のうち**統計的推定**に相当します。

信頼区間は確率をもって設定するため、どの程度の精度で推測したいかによってひげの長さは変わってきますが、**一般的には95%信頼区間**を求めることが多いです。

この95%という数字の解釈には、注意が必要です。

95％信頼区間は、**真の母集団の平均が信頼区間内にある確率が95％だと言っているわけではありません。**

正しくは、**信頼区間が100個あるときに95個の信頼区間には真の母集団の平均が含まれる**ことを表します。

ここまでの話をきちんと理解するためには、確率を勉強する必要がありますが、大事なことは、**エラーバーがあることによって、標本から得られたグループ間の平均の差が、意味のある差なのか、それとも単なる偶然によって得られた差なのかをざっくりと知ることができる**ということです。

2つのグラフの**エラーバーの範囲が重なっていなければ、グループ間の平均には統計的に差がある**と言えますし、**エラーバーの範囲がほとんど重なっていれば、グループ間の平均に統計的な差があるとは言えない**ことになります。

エラーバー付棒グラフを作れるようになると、さまざまな分析が可能になります。例えば

- 昼と夜で売上金額が変化するのかを知ることができる
- 曜日によって利用者の平均年齢が変化するのかを知ることができる
- 打ち出した広告に効果があったのかを検証することができる

ランダムサンプリングされたデータと、標準誤差の知識があれば、工夫次第で大抵の疑問に答えることができます。少し難しい話ではありますが、ぜひ使い方をマスターしましょう。

#### Excelで標準誤差を考慮した棒グラフを作る

Excelでエラーバー付棒グラフを作るには、まず、グループ別にデータを整理する必要があります。

いくつか方法はありますが、グループごとにデータを分割すると分かりやすいでしょう。

ここでは、男女で平均購入金額の差があるかどうかを分析していきます。

**データ分割方法**

[https://www.notion.so](https://www.notion.so)

1. `gender`変数でフィルタをかけ、男性のデータのみを表示
    - グループを規定する変数でフィルタをかけるので、例えば、商品別に分けるなら`goods`変数でフィルタをかける
2. フィルタリングされたデータをすべてコピーし、適当なところにペーストする
3. 女性についても1と2を行う

男女別にデータを分割できたら、以下のような表を作ります。

[https://www.notion.so](https://www.notion.so)

**必要項目**

- 男女別の平均
    - `AVERAGE`関数を使う
- 男女別の不偏分散
    - `STDEV.S`関数を使う
- 男女別のサンプルサイズ
    - `COUNT`関数を使う
        - `COUNT`関数はデータ範囲を指定して、`= COUNT(データ範囲)`のように使う
- 男女別の標準誤差
    - 男女別に計算した平均、不偏分散、サンプルサイズを使って計算する
    - セルに`= 不偏分散のセル番地/SQRT(サンプルサイズのセル番地)`と入力
    - `SQRT()`関数は、（）内に指定した数値（計算式でもOK）の平方根を返す関数
- 男女別の信頼区間
    - 男女別の標準誤差を使って計算
    - 95％信頼区間：`= 1.96*標準誤差のセル番地`と入力
    - かける数字の大きさによってさまざまな信頼区間を計算可能
        - 90％信頼区間：`= 1.64*標準誤差のセル番地`
        - 99％信頼区間：`= 2.58*標準誤差のセル番地`

表ができたら、グループ名と各平均値のセルを選択し、棒グラフを作ります。できたグラフに、以下の手順でエラーバーを追加しましょう。

**エラーバー追加手順**

[https://www.notion.so](https://www.notion.so)

1. グラフを選択し、**グラフのデザイン**→**グラフ要素を追加**→**誤差範囲**→**その他の誤差範囲オプション**の順に選択
2. 誤差範囲の追加ウィンドウでエラーバーを追加したいグループ名を選択しOKをクリック
3. **誤差範囲の書式設定**メニューの**誤差範囲**から**ユーザー設定**を選択
4. ユーザー設定の誤差範囲ウィンドウで、**正の誤差の値**と**負の誤差の値**両方に計算した**信頼区間のセル番地**を指定

これで、エラーバー付棒グラフを作成できました。

[https://www.notion.so](https://www.notion.so)

** グラフに関する考察

# 仮説検定

エラーバー付棒グラフを作成することで、グループ間で平均値に差があるかどうかをざっくりと知ることができました。

しかし、これはあくまでもざっくりとした分析です。

- 男女間で平均購入金額に統計的に意味がある差が存在するのではないか
- 広告によって売り上げは向上したのではないか
- 時間帯によって客層は異なるのではないか

こういった**仮説**にしっかりと答えるためには、何らかの**客観的な基準で評価をする**ことが必要です。

データを利用して、このような**仮説が正しいかどうかを客観的に判断することを仮説検定**と呼びます。

例えば、男女間で平均購入金額に差があるのではないか、という仮説を検討します。

データから得られた平均購入金額は、男性が**円、女性が**でした。

男性の方が**円多く購入しているから、**男女間では平均購入金額に差があると言えるでしょうか？**

仮に言えたとして、**何円以上の差があれば男女間に差があると言えるでしょうか？**

この問いに答えるために、まずは**仮説を立てます**。

統計的手法を用いて仮説検定を行う際には、**仮説の立て方に少し独特な作法があります**。

- **×**　男女間で平均購入金額に差が**ある**
- **〇**　男女間で平均購入金額に差は**ない**

このように、**差はないという仮説を立て**、この仮説を統計的に否定する（**棄却する**）ことで、**男女間で平均購入金額に差があるという仮説を支持**します。

#### 帰無仮説と対立仮説

- **帰無仮説**：「差はない」という**否定したい仮説**のこと
    - 否定することによって無に帰す仮説という意味
    - 上の例では、「男女間で平均購入金額に差は**ない**」という仮説のこと
- **対立仮説**：「差がある」という**支持したい仮説**のこと
    - 帰無仮説に対立するという意味
    - 上の例では、「男女間で平均購入金額に差が**ある**」という仮説のこと
    - 対立仮説を「男性の方が女性より平均購入金額が高い」とすることもできる
        - 対立仮説が「差がある」のとき：男性の方が平均購入金額が高いときと低いときの両方を支持する（**両側検定**という）
        - 対立仮説が「男性の方が女性より平均購入金額が高い」のとき：男性の方が平均購入金額が高いという片側の差のみを支持する（**片側検定**という）

少し回りくどく見えるかもしれませんが、**「差がある」ことを仮説にするのは意外と難しい**のです。

大きな理由は、**どの程度の大きさであれば「差がある」と言えるのか、客観的な指標で判断しづらい**ことです。10円の差を「差がある」と言う人もいれば、100円差があっても「差はない」と言う人もいるかもしれません。

そこで、➀**「差がない」ことが正しいと仮定し、②得られたデータが「差がない」前提の下ではとても珍しい確率でしか得られないものだとしたら、③「差がない」という前提を否定する**、という手順を踏むことで、「差がある」という仮説を支持していくのです。

### Excelで仮説検定（ $t$ 検定）をする

**男女間での平均購入金額に差があるかどうかという仮説を例**に、仮説検定の中でも、特に **$t$ 検定の手順**を見ていきましょう。t 検定は、平均値の差があるかどうかを検定するときに使われる手法です。手順は以下の通りです。

1. 有意水準を決める
2. 仮説を立てる
3. t 検定を実行する
4. 結果を解釈する

1つひとつを詳しく見ていきましょう。

#### 1. 有意水準を決める

まずは、どういう数字が出てきたら帰無仮説を棄却するか、最初に基準となる数字を決めておきます。

具体的には、**男女間での平均購入金額に差がないという仮説が正しいときに、手元のデータの平均の差が $X$ 円以上となる確率が $\alpha$ 以下であれば、差がないという前提が間違っていたと判断する**ときの **$\alpha$** という数字です。

慣例的に **$\alpha$ は $5\%$** とすることが多いです。

以降の手順で**男女間での平均購入金額に差がないという仮説が正しいときに、手元のデータの平均の差が $X$ 円以上となる確率**を求めていくので、判断に主観が入らないよう、先に決めておく必要があります。

#### 2. 仮説を立てる

次に、仮説を立てます。

先に述べたように、男女間で平均購入金額に差があるかどうかを検定するときは、**男女間での平均購入金額に差がない**という帰無仮説を立てます。

男性の平均購入金額を $\overline{sales}_{男}$、女性の平均購入金額を $\overline{sales}_{女}$ とすると、帰無仮説 $\mu_{0}$ は

 

$$
\mu_{0}: \bar{X} \equiv \overline{sales}_{男} - \overline{sales}_{女} = 0
$$

という式で表すことができます。

#### 3. t 検定を実行する

ここからは、Excelのデータ分析ツールを使って検定を行っていきます。

データ分析ツールは、Excelの「データ」タブ内にある「データ分析」をクリックすると使えます。

「データ分析」が見つからないときは、先に「分析ツールアドイン」を追加する必要があります。

##### 分析ツールアドインの追加

1. 「ファイル」タブ→「オプション」を選択
2. 「アドイン」→「設定」を選択
3. 「分析ツール」にチェックを入れ、OKをクリック
4. Excelを再起動する

次に、データ分析ツールに合わせてデータを加工します。

[Excelで標準誤差を考慮した棒グラフを作る](#excelで標準誤差を考慮した棒グラフを作る)と同じように、男性のデータと女性のデータに分割しましょう。

分割ができたら、「分析ツール」を起動します。分析ツールから **t 検定: 等分散を仮定した2標本による検定**を選択し、必要なオプションを入力します。

##### t 検定のオプション

- 変数1の入力範囲
    - 1つ目のデータの範囲
    - ここでは男性のデータの範囲
- 変数2の入力範囲
    - 2つ目のデータの範囲
    - ここでは女性のデータの範囲
- 仮説平均との差異
    - 帰無仮説で設定した差を入力（デフォルトは $0$ ）
    - ここでは空欄または0
- ラベル
    - データ範囲にラベル（列名）が含まれるときはチェックを入れる
- $\alpha$
    - 有意水準（デフォルトは $0.05$）
- 出力オプション
    - 結果を出力したいセルを入力
        - 出力先：データと同じシートに出力したいときに、出力したいセルを入力
        - 新規ワークシート：新しいシートに出力したいときに、ワークシート名を記入（空欄でもOK）
        - 新規ブック：新しいファイルに出力したいときに、ファイル名を記入（空欄でもOK）

以上を入力してOKをクリックすると、指定した出力先に結果が表示されます。

#### 4. 結果を解釈する

オプションが正しく入力されていれば、以下のような結果が表示されるはずです。

##### 結果の表

- ラベル
    - データに設定した列名
- 平均
    - 各データ（ここでは男女それぞれ）の平均値
- 分散
    - 各データ（ここでは男女それぞれ）の不偏分散
- 観測数
    - 各データ（ここでは男女それぞれ）のサンプルサイズ
- プールされた分散
    - 割愛
- 仮説平均との差異
    - 仮説平均との差異に設定した値（ここでは0）
- 自由度
    - 割愛
- t
    - t値と呼ばれ、各データの差（ここでは男女間の差）を評価する値
    - この値が大きいほど各データの差（ここでは男女間の差）が大きいことを示す
- P(T> =t) 片側
    - 片側検定（対立仮説が「男性の方が女性より平均購入金額が高い」あるいは「男性の方が女性より平均購入金額が低い」）の場合の $p$ 値（$p$ 値については後述）
- t 境界値 片側
    - 片側検定の場合の棄却限界値
    - t値がこの値を超えると帰無仮説を棄却できる
- P(T> =t) 両側
    - 両側検定（対立仮説が「男女間で平均購入金額に差がある」）の場合の $p$ 値
- t 境界値 両側
    - 両側検定の場合の棄却限界値

いろいろと数値が出力されてややこしいかもしれませんが、**注目すべきは  P(T> =t)（$p$ 値）** です。

これは、**「男女間の平均購入金額に差がない」という帰無仮説が正しいときに、手元のデータで観察される差が生じるのはどのくらい珍しいことなのかを表す確率**です。

私たちは最初に、**男女間での平均購入金額に差がないという仮説が正しいときに、手元のデータの平均の差が $X$ 円以上となる確率が $\alpha$ 以下であれば、差がないという前提が間違っていたと判断する**と決めました。

よって、t 検定で得られた $p$ 値と最初に決めた $\alpha$ を比較し、**$p< \alpha$ であれば帰無仮説を棄却**し、**$p< \alpha$ であれば帰無仮説を支持する**という判断を行うことになります。

# 回帰分析

## ノンパラメトリック回帰

## 線形回帰

# 因果推論

## ランダム比較化試験（RCT）

[講義資料に戻る](index.html)