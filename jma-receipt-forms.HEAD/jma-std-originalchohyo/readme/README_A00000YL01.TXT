公開帳票
帳票名称        医薬品名・一般名対応リスト
プログラムＩＤ  A00000YL01

＜更新履歴、変更内容＞
  V01.00  2012/05/01  新規作成
  V01.01  2012/05/18  薬価基準コード１２桁対応、バイト数変更（入力コード）
  V01.02  2012/11/14  monpe-0.6.20対応
  V01.03  2014/10/31  ver4.8.0対応（一時ディレクトリ変更）
  V01.04  2016/11/30  ver5.0.0対応
  V01.05  2018/11/22  ver5.1.0対応

＜プログラムのメニュー登録＞
  「91 マスタ登録」、「101 システム管理マスタ」より
  「3002  統計帳票出力情報(月次)」に対して以下のプログラム登録の処理を行う。

  プログラム登録設定情報
    帳票番号    ： 任意に設定
    帳票名      ： 医薬品名・一般名対応リスト
    プログラム名： A00000YL01
    パラメタ１  ： 項目１     、FREE 、任意
    パラメタ２  ： 項目２     、FREE 、任意
    パラメタ３  ： 項目３     、FREE 、任意
    パラメタ４  ： 項目４     、FREE 、任意
    パラメタ５  ： 項目５     、FREE 、任意
    パラメタ６  ： 薬剤区分   、PSN1 、必須
    パラメタ７  ： 入力コード 、PSN1 、必須
    パラメタ８  ： 一般名区分 、PSN1 、必須
    パラメタ９  ： 改頁区分   、PSN1 、必須

  パラメタ説明
    項目１：入力コード
    項目２：薬剤区分
    項目３：一般名区分
    項目４：医薬品名
    項目５：一般名

      項目１〜５には優先順位を「1、2、3、4、5、空白」で指定します。
      重複は不可です。※空白は可
      指定した優先順位の昇順に印字されます。

      設定例  ： 項目１「1」  項目２「3」  項目３「5」  項目４「4」  項目５「2」
      出力結果：（１）入力コード  （２）一般名  （３）薬剤区分  （４）医薬品名  （５）一般名区分

    薬剤区分    0：すべて  1：内服薬  2：外用薬  3：注射薬

    入力コード  0：あり    1：なし    2：すべて
      診療行為入力画面で表示される「入力コード（表示）」を印字します。

    一般名区分  0：すべて  1：銘柄名  2：一般名  3：処方名

    改頁区分    0：なし    1：あり
      医薬品の区分毎に改頁します。
      ※項目２：薬剤区分が「1」の時のみ有効です。
