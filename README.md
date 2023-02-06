
# juki

住民基本台帳に関するデータを扱うためのRパッケージです。

## インストール

パッケージはremotesパッケージを用いてGitHub経由でインストールします。

```r
 (!requireNamespace("remotes"))
   install.packages("remotes")

 remotes::install_github("uribo/juki")
```

## 使い方

```r
library(juki)
```

### 住民基本台帳人口移動報告

総務省統計局が公開する[住民基本台帳人口移動報告](https://www.stat.go.jp/data/idou/index.html)のうち、都道府県別のファイルを読み込みます。

```r
read_idou_pref("b01036s.xlsx", 
                pref_code = "36")
```

## 関連パッケージ

- https://github.com/uribo/jpops
