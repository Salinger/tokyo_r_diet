##############################################################################
#
#     データの読み込み（dataframe型として読み込み）
#
##############################################################################

training_data_df <- read.table("training_result.csv", header=T, sep=",")
print(head(training_data_df)) # 先頭6行分確認


# 要素名を修正 
# names(x) は 指定した要素の名前を書き換える
# c()はベクトルを生成する関数。
names(training_data_df) <- c(
    "date", "weight", "bodyfat",
    "fat_kg", "run", "cycling",
    "warming_up", "sit_up", "back_extension", 
    "press_up", "squat", "calf",
    "dumblell"
    )

print(head(training_data_df)) # 先頭6行分を再出力。解決。
training_data_df$date <- as.Date(training_data_df$date) # 日付をdate型に変換しておく。


##############################################################################
#
#    データの概要チェック
#
##############################################################################

# summary() で対象データの概要をチェックできる。
#     ・数値データの場合
#         Min.:       最小値
#         1st Qu.:    昇順ソート時 1/4 番目の値
#         Median:     中央値
#         Mean:       平均値
#         3rd Qu.:    昇順ソート時 3/4 番目の値
#         Max.:       最大値
#         NA's:       欠損値の数
#     ・テキストデータ等の場合
#         要素の数を数えてくれる。
print(summary(training_data_df))

# pairs() で 散布図行列 が描ける。各要素同士の関係の雰囲気が視覚的にわかる。
#     今回は2次元グラフだが、オプションを指定し3次元目の要素で色分け等も可能。
pairs(training_data_df)
# "date" 要素の列に着目すると、現状が見えてくる。
#     日付が経つにつれ、"Weight" と "bodyfat" が減少: 線形関係がありそう。
#     "run" と "cycling" はどちらかのみの場合が多そう。
#     筋トレ系は2 ~ 3日に1回のペースっぽい。    


##############################################################################
#
#    体重の変化を確認
#
##############################################################################

# 体重に関わるデータのみ subset() で切り出す。
 subset_weight_df <- subset(
    training_data_df,      # 切り出し元の dataframe 
    !is.na(weight),      # "weight" が NA でないもののみ
    c("date", "weight")  # "date" と "weight" 要素のみ
    )

# とりあえず、グラフにプロット。
plot(weight ~ date,data = subset_weight_df)

# 単回帰分析を行なってみる。 (y = ax + b [y: weight, x: date])
result_weight <- lm(weight ~ date,data = subset_weight_df)
print(result_weight)

# とりあえず分析結果をプロット
abline(result_weight) # 回帰式をプロット

# 回帰分析結果の概要をチェック
#    Residuals: y の予測値と実測値の残差（誤差）の最小値、
#               1/4、中央値、3/4、最大値。
#    Coefficients: 係数について。
#        (Intercept): b の値。切片。
#        date: a の値。傾き。1日あたりの体重の減少量。
#            Estimate: 予測した係数。
#            Std. Error: 標準誤差。
#            t value: t 値。自由度と合わせて、t分布表で
#                     有意かどうか確認できる。
#            Pr(>|t|): p 値。小さければ小さいほどよい。
#                      一般に p < 0.05 が有意かどうかの別れ目。
#    Residual standard error: 残差の分布が理論の仮定通りであるか検定するとき、
#                             使われる場合あり。 
#    Multiple R-sqared: 寄与率（x がどのぐらい y を説明しているか）。
#                       0.5 以上が目安。 
#    Adjusted R-squared: 調整済み寄与率。寄与率は説明変数が多くなるほど、
#                        値が大きくなるので補正してやる必要がある。
#                        単回帰分析では問題になりづらいが、
#                        重回帰分析ではこちらに注意。
#    F-statistic: F検定結果。人に見せられる推定結果なら
#                 F検定はパスして当然なので、たいてい無視される。
#    p-value: p 値。
#
#    重要なのは、p値と係数。
#
print(summary(result_weight))

# 信頼区間を算出
#     「測定・解析を全く同じやり方で100回繰り返したら、
#       95回は回帰直線はこの範囲を通ると考えられる」区間。
result_weight_confidence <- predict(
    result_weight,
    interval="confidence"
    )

# 予測区間を算出
#     「新たな測定を１００回繰り返したら、
#       95回はこの範囲に収まると考えられる」区間。
result_weight_predict <- predict(
    result_weight,
    interval="prediction"
    ) 

# プロットしてみる
#     黒：回帰式
#     赤：信頼区間
#     青：予測区間
date_start = "2013/05/10"
date_last = "2013/07/04"

plot(
    x=subset_weight_df$date,
    y=subset_weight_df$weight,
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(50, 60),
    ylab="Weight [kg]",
    xlab="Date"
)
par(new=T) # 前のグラフに上書き
plot(
    x=subset_weight_df$date,
    y=result_weight_confidence[,1],
    type="l",
    xlim=c(as.Date(date_start),as.Date(date_last)),
    ylim=c(50, 60),
    ylab="",
    xlab=""
)
par(new=T)
plot(
    x=subset_weight_df$date,
    y=result_weight_confidence[,2],
    type="l",
    xlim=c(as.Date(date_start),as.Date(date_last)),
    ylim=c(50, 60),
    ylab="",
    xlab="",
    col="red"
)
par(new=T)
plot(
    x=subset_weight_df$date,
    y=result_weight_confidence[,3],
    type="l",
    xlim=c(as.Date(date_start),as.Date(date_last)),
    ylim=c(50, 60),
    ylab="",
    xlab="",
    col="red"
)
par(new=T)
plot(
    x=subset_weight_df$date,
    y=result_weight_predict[,2],
    type="l",
    xlim=c(as.Date(date_start),as.Date(date_last)),
    ylim=c(50, 60),
    ylab="",
    xlab="",
    col="blue"
)
par(new=T)
plot(
    x=subset_weight_df$date,
    y=result_weight_predict[,3],
    type="l",
    xlim=c(as.Date(date_start),as.Date(date_last)),
    ylim=c(50, 60),
    ylab="",
    xlab="",
    col="blue"
)

# 体重は減少傾向にある！めでたい！
# しかし、本当に脂肪だけが減っているのだろうか？
# 体脂肪率から脂肪を算出して、同様に計算してみる。


##############################################################################
#
#    体脂肪の変化を確認
#
##############################################################################

# 体重・体脂肪に関わるデータのみ subset() で切り出す。 
subset_fat_df <- subset(
    training_data_df,      # 切り出し元の dataframe 
    !is.na(fat_kg),     # "weight" が NA でないもののみ (! は not)
    c("date","fat_kg")  # "date" と "fat_kg"
  )

# "date" と "bodyfat" のデータフレームができた。解析する。
# とりあえず、グラフにプロット。
plot(fat_kg ~ date, data = subset_fat_df)

# 単回帰分析を行なってみる。 (y = ax + b [y: fat_kg, x: date])
result_fat <- lm(fat_kg ~ date,data = subset_fat_df)
print(result_fat)

# とりあえず分析結果をプロット
abline(result_fat) # 回帰式をプロット

# 回帰分析結果の概要をチェック
print(summary(result_fat))

# 信頼区間を算出
result_fat_confidence <- predict(
    result_fat,
    interval="confidence"
    )

# 予測区間を算出
result_fat_predict <- predict(
    result_fat,
    interval="prediction"
    ) 

# プロットしてみる
#     黒：回帰式
#     赤：信頼区間
#     青：予測区間
plot(
    x=subset_fat_df$date,
    y=subset_fat_df$fat_kg,
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0, 15),
    ylab="Body fat [kg]",
    xlab="Date",
)
par(new=T) # 前のグラフに上書き
plot(
    x=subset_fat_df$date,
    y=result_fat_confidence[,1],
    type="l",
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0, 15),
    ylab="",
    xlab="",
)
par(new=T)
plot(
    x=subset_fat_df$date,
    y=result_fat_confidence[,2],
    type="l",
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0,15),
    ylab="",
    xlab="",
    col="red"
)
par(new=T)
plot(
    x=subset_fat_df$date,
    y=result_fat_confidence[,3],
    type="l",
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0, 15),
    ylab="",
    xlab="",
    col="red"
)
par(new=T)
plot(
    x=subset_fat_df$date,
    y=result_fat_predict[,2],
    type="l",
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0, 15),
    ylab="",
    xlab="",
    col="blue"
)
par(new=T)
plot(
    x=subset_fat_df$date,
    y=result_fat_predict[,3],
    type="l",
    xlim=c(as.Date(date_start), as.Date(date_last)),
    ylim=c(0, 15),
    ylab="",
    xlab="",
    col="blue"
)

# 体脂肪も減少傾向にある！めでたい！
