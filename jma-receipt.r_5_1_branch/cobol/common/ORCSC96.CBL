      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC96.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : エラーメッセージセット処理
      *                      （Ｋ０２、Ｋ０５、Ｊ０４のみ共通使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/01/09    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.00     MCC-多々納   02/03/20  Ｈ１４．４改正対応済み
      * 01.00.01     NACL-多々納  02/10/03  労災加算なしの追加
      * 01.00.02     NACL-多々納  02/10/18  入院追加
      * 01.00.03     NACL−多々納 02/11/11  ＤＵＭＭＹ診察料追加
      * 01.00.04     NACL−多々納 02/12/09  動脈注射エラー追加
      * 04.07.00     NACL-多々納  12/03/XX  Ｈ２４年４月改正対応
      * 04.07.00     NACL-多々納  15/03/XX  Ｈ２７年４月改正対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    一時領域
      *01  WRK-AREA.
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    パラメタ
           COPY    "CPORCSC96.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSC96AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *    共通エラーメッセージ編集
           PERFORM 100-KYOUTU-ERRMSG-SEC
      *
           IF      SPA-ERRMSG          =   SPACE
      *        プログラム別メッセージ編集
               PERFORM 200-PG-ERRMSG-SEC
           END-IF
      *
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    共通エラーメッセージ編集処理
      *****************************************************************
       100-KYOUTU-ERRMSG-SEC            SECTION.
      *
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当する点数マスターが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "0890"
                   STRING  "診療行為では使用できないコードです。"
                           "フリーコメントコードに変更して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0002"
                   MOVE    "診療行為は数量を入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "医薬品は必ず数量を入力してください"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "きざみ値の数量は整数で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
      **           MOVE    "入力コードの文字エラー"
                   STRING  "入力コードの全角半角混在エラーです。"
                           "正しい入力コードを入力して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   MOVE    "診療種別がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   STRING  "４００行を超えます。"
                           "これ以上挿入はできません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   MOVE    "回数は１回のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "注射薬は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "時間外の入力はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "撮影部位コードは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "数量・回数等の数字・桁数エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "0312"
                   MOVE    "器材の数量は小数点以下３桁までです"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "手技料を入力して下さい" 
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入院のコードです。外来では入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0114"
                   MOVE    "外来のコードです。入院では入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "老人のコードです。入力できません。" 
                                       TO  SPA-ERRMSG
               WHEN    "0115"
                   MOVE    "老人のコードではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。" 
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0016"
                   MOVE    "病院のコードです。診療所では入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0116"
                   MOVE    "診療所のコードです。病院では入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "初診・再診は重複できません"
                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "算定済です。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "手技料は剤内に１つのみです"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "行挿入エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "数量が下限値以下です"
                                       TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "数量が上限値以上です"
                                       TO  SPA-ERRMSG
               WHEN    "K030"
                   MOVE    "警告！数量が上限値以上です。"
                                       TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "剤削除は剤開始からです"
                                       TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "施設基準ではありません。算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "2001"
                   STRING   "埋め込み数値がすべてゼロになります。"
                                       DELIMITED  BY  SIZE
                            "正しい値を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0025"
                   MOVE    "時間入力の診療コードを入力してください"
                                       TO  SPA-ERRMSG
               WHEN    "0026"
               MOVE    "病理判断料を算定できる検査を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   STRING  "病理学的検査判断料が算定済です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0028"
                   MOVE    "検査の包括対象が混在しています。"
                                       TO  SPA-ERRMSG
               WHEN    "0029"
      ****         MOVE    "コメントの入力値エラー"
      **                               TO  SPA-ERRMSG
                   STRING  "コメント入力値のエラーです。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0030"
                   MOVE    "時間加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0089"
                   MOVE    "既に違う時間外区分があります。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0031"
                   MOVE    "乳幼児加算はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   MOVE    "算定できない年齢です。"
                                       TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "上記算定済です。併用算定はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "K033"
                   STRING  "警告！！"
                           "上記算定済です。併用算定はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H25.8
               WHEN    "K035"
                   STRING  "警告！"
                           "上記算定済です。"
                           "労災では併用算定はできません。"
                           "保険の確認をして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0034"
                   MOVE
                   "初診料算定日より、１月以内です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0120"
                   MOVE
                   "退院日より、１月以内です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0121"
                   STRING  "他院退院日より、１月以内です。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0035"
                   MOVE
                   "剤内に手技料は１件です。剤を終了して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "部位コードを入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "数量が上限を超えています"
                                       TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "酸素を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE    "酸素・窒素は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0040"
                   MOVE    "未熟児加算が有ります"
                                       TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "未熟児加算はできません"
                                       TO  SPA-ERRMSG
               WHEN    "0042"
                   STRING  "既に同じ種類の加算があります。"
                           "この加算はできません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0043"
                   MOVE    "時間外加算は１件です"
                                       TO  SPA-ERRMSG
               WHEN    "0044"
                   MOVE    "小児科外来診療料以外です"
                                       TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE    "小児科外来診療料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE    "小児科外来診療料は算定済です"
                                       TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "器材は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0147"
                   MOVE    "薬剤は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "約束セットの時、診療種別を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0049"
                   MOVE    "約束セット内容と診療種別が違います"
                                       TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE    "入力コードがありません"
                                       TO  SPA-ERRMSG
               WHEN    "0150"
                   MOVE    "対象期間の入力セットはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "数量は５桁までです"
                                       TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE
                   "老人慢性疾患外来総合診療料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE
                   "老人慢性疾患外来総合診療料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE
                   "寝たきり老人在宅総合診療料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE    "自費入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0086"
                   STRING  "金額が税込みです。"
                           ".950 では算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0056"
                   MOVE    "検査の逓減ができません。算定日が前後します"
                                       TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE    "点滴は１日１回しか算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0157"
                   MOVE    "動脈注射は１日１回しか算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "K025"
                   STRING  "警告！！注加算コードのある加算はまとめて"
                           "入力して下さい。レセ電エラーになります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0301"
                   STRING  "既に点滴注射・中心静脈注射が算定済みです。"
                           "静脈注射は算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0302"
                   STRING  "既に静脈注射・中心静脈注射が算定済みです。"
                           "点滴注射は算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0303"
                   STRING  "既に点滴注射・静脈注射・中心静脈注射が"
                           "算定済みです。算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0058"
                   MOVE    "外来管理加算は算定済です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE
                   "加算は算定できません。診察料に加算して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0060"
                   MOVE
                   "検査の重複算定エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0061"
                   MOVE
                   "残量廃棄は注射以外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE
                   "粉砕は内服以外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "2062"
                   MOVE    "用量割合は投薬で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE
                   "粉砕の次には錠剤を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE
                   "同一検査があります。削除して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0065"
                   MOVE
                   "初診料算定日より、２月以内です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE
                   "１５才未満です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0067"
                   MOVE
                   "厚生労働大臣が定める患者です。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0068"
                   MOVE
                   "生活習慣病指導管理料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0069"
                   MOVE    "時間外特例医療機関ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "時間外特例は算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1401"
                   MOVE
                   "在宅末期医療総合診察料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "1402"
                   MOVE
                   "在宅時医学総合管理料を算定時、算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "0070"
                   MOVE    "同時併施分です。入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "K070"
                   MOVE    "警告！同時併施のコードです。"
                                       TO  SPA-ERRMSG
               WHEN    "0071"
      ************ MOVE    "撮影料が違います。"
      ***********                      TO  SPA-ERRMSG
                   STRING  "剤内に撮影料が算定されています。"
                           "剤を分けて下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0072"
                   MOVE
               "撮影料はこれ以上入力できません。剤を変えて下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE    "施設基準不適合の入力がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "逓減コードは入力できません"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0173"
                   MOVE    "施設基準不適合の手術ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "逓減・加算コードは入力できません"
                                       TO  SPA-ERRMSG(37:)
      *H27.4
                   IF      SPA-SRYYMD      >=  "20150401"
                       MOVE    SPACE           TO  SPA-ERRMSG
                       STRING  "施設基準不適合の手術ではありません。"
                               "逓減コードは入力できません"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                       END-STRING
                   END-IF
               WHEN    "0074"
                   MOVE
               "この種別は投薬料が算定できない時のみ入力できます。"
                                       TO  SPA-ERRMSG
               WHEN    "0075"
                   MOVE
                   "算定可能な病床数ではありません。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0076"
                   MOVE
                   "１日に３部位まで算定できます。既に算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "0077"
                   MOVE
                   "電話再診時には、算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0078"
                   MOVE    "ＣＴ，ＭＲＩは既に算定済みです。"
                                       TO  SPA-ERRMSG
                   MOVE    "２回目以降のコードを入力して下さい"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0079"
                   MOVE    "撮影料がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0080"
                   MOVE    "名称は全角で入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0081"
                   MOVE    "個別療法が１０単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "分散して入力して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "K081"
                   MOVE    "警告！個別療法が１０単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "分散して入力して下さい。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "K082"
                   MOVE    "警告！個別療法が１０単位以下です。"
                                       TO  SPA-ERRMSG
               WHEN    "0082"
                   MOVE    "初診料算定日より、６ヶ月以降です。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0083"
                   MOVE    "この加算は算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0084"
                   MOVE    "集団療法が８単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0085"
                   MOVE    "集団療法が１２単位を超えます。"
                                       TO  SPA-ERRMSG
                   MOVE    "入力できません。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0091"
                   MOVE
                   "算定上限回数を超えています。算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "リハビリの１日の上限単位を超えています。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "1093"
                   MOVE    "中途終了登録エラー。"
                                       TO  SPA-ERRMSG
               WHEN    "K092"
                   MOVE    "警告！"
                                       TO  SPA-ERRMSG
                   MOVE    "リハビリの１日単位が４単位を超えています。"
                                       TO  SPA-ERRMSG(7:)
               WHEN    "0093"
                   MOVE    "個別療法と集団療法の混在はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "0094"
                   MOVE    "既にこの労災では算定済です。算定できません"
                                       TO  SPA-ERRMSG
      *H25.10
               WHEN    "K094"
                   STRING  "警告！"
                           "既にこの労災では算定済です。算定できません"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0095"
                   MOVE    "個別療法と集団療法の混在している日が"
                                       TO  SPA-ERRMSG
                   MOVE    "あります。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "0096"
                   STRING  "検査逓減になります。剤を分けて入力"
                           "して下さい"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K097"
                   MOVE    "警告！早期リハビリ加算は"
                                       TO  SPA-ERRMSG
                   MOVE    "急性発症日から９０日以内に算定できます。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "K098"
                   STRING  "警告！早期リハビリテーション加算の"
                                       DELIMITED   BY  SIZE
                           "期間が違います。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K101"
                   STRING  "警告！急性発症日終了日以降の日です。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0110"
                   MOVE    "他保険にて診察料算定済のコメントは"
                                       TO  SPA-ERRMSG
                   MOVE    "診察料と同時に算定できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0112"
                   MOVE    "小児科特例時間外区分は算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0113"
                   MOVE    "小児科特例時間外は算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0130"
                   STRING  "特定薬剤治療管理料が３月目以内です。"
                           "算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0131"
                   STRING  "この特定薬剤治療管理料は算定済みです。"
                           "算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0132"
                   STRING  "既に特定薬剤治療管理料は当月算定済です。"
                           "初回算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0133"
                   MOVE    "初回算定日には診療年月を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0134"
                   STRING  "既に同じ診療コードが入力されています。"
                           "算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0135"
                   MOVE    "初回算定日は診療年月以前を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0136"
                   STRING  "診察料のＤＵＭＭＹ算定用です。"
                           "初診と再診の診療コードを設定して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0137"
                   MOVE    "診察料のＤＵＭＭＹが既に入力されています。"
                                       TO  SPA-ERRMSG
               WHEN    "0138"
                   STRING  "既に当日算定済です。"
                           "再診料を入力して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0139"
                   MOVE    "初診料が算定されていません。"
                                       TO  SPA-ERRMSG
                   MOVE    "初診料を入力して下さい。"
                                       TO  SPA-ERRMSG(29:)
               WHEN    "0140"
                   MOVE    "労災の読み替え加算ができません。"
                                       TO  SPA-ERRMSG
                   MOVE    "回数を１回にして分けて入力して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0141"
                   MOVE    "まとめ入力連番が正しくありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "対象外とします。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0142"
                   MOVE
                   "寝たきり老人在宅総合診察料の算定がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "算定終了は算定できません。"
                                       TO  SPA-ERRMSG(47:)
               WHEN    "7007"
                   STRING  "自動算定で４００行を超えます。"
                           "分けて入力して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7001"
                   STRING  "４００行を超えた行です。"
                           "これ以上入力できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7002"
                   STRING  "最終ページです。"
                           "これ以上入力できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7003"
                   STRING  "次は最終ページです。これ以上入力すると"
                           "登録時にエラーとなることがあります。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7004"
                   STRING  "４００行をオーバーします。"
                           "４００行までを対象とします。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7006"
                   STRING  "剤の明細数は５０明細までです。"
                           "剤を分けて下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7008"
                   STRING  "訂正中です。受診履歴一覧は変更できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7009"
                   STRING  "受診履歴の次頁はありません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "7010"
                   STRING  "受診履歴の前頁はありません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8000"
                   MOVE    "労災保険の診療コードです。入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "8001"
                   MOVE    "労災保険の診療コードを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "8002"
                   STRING  "この労災保険の診察は既に入力済みです。"
                           "初診料は算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8003"
                   STRING  "救急医療管理加算は既に算定済みです。"
                           "算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8004"
                   STRING  "四肢加算ができないコードです。"
                           "労災加算を削除して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.6
               WHEN    "K804"
                   STRING  "警告！"
                           "四肢加算ができないコードです。"
                           "労災加算を削除して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8005"
                   MOVE    "労災加算は１．５倍のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "8006"
                   STRING  "アフターケアの時、継続管理加算は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8007"
                   STRING  "アフターケアの時、外来管理加算は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8008"
                   STRING  "アフターケアの時、在宅自己導尿指導管理料は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8009"
                   STRING  "アフターケアの時、てんかん指導料は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8010"
                   STRING  "救急医療管理加算は初診料算定時にのみ"
                           "算定できます。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8011"
                   STRING  "療養の給付請求書取扱料は既に算定済みです。"
                           "算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8012"
                   STRING  "アフターケアの時、再診時療養指導管理料は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8013"
                   STRING  "再診時療養指導管理料は初診料算定時には"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8014"
                   STRING  "初診時ブラッシング料は初診料算定時にのみ"
                           "算定できます。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8015"
                   STRING  "アフターケアの時、救急医療管理加算は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8016"
                   STRING  "アフターケアの時、労災の指導管理等は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8017"
                   MOVE    "アフターケアの時、在宅料は算定できません"
                                       TO  SPA-ERRMSG
               WHEN    "8018"
                   STRING  "アフターケアの時、初診時ブラッシング料は"
                           "算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8019"
                   STRING  "手指の創傷に係る機能回指導加算は"
                           "既に算定済です。算定できません"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8020"
                   MOVE    "この診療種別には入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "8021"
                   MOVE    "労災保険の診療コードを入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "8022"
                   MOVE    "救急医療管理加算は入院日のみに"
                                       TO  SPA-ERRMSG
                   MOVE    "算定可能です。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "8023"
                   MOVE    "脳血管急性発生日と発生終了日を同日に"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "8024"
                   MOVE    "脳血管急性発生日が設定されていません。"
                                       TO  SPA-ERRMSG
               WHEN    "8025"
                   MOVE    "脳血管急性発生終了日は労災・自賠責では"
                                       TO  SPA-ERRMSG
                   MOVE    "算定できません。"
                                       TO  SPA-ERRMSG(39:)
               WHEN    "8026"
                   MOVE    "労災加算の倍率が違います。"
                                       TO  SPA-ERRMSG
                   MOVE    "同じ倍率毎に剤を分けて下さい。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "8050"
                   MOVE    "外来診察料算定時は電話再診の算定は"
                                       TO  SPA-ERRMSG
                   MOVE    "できません。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "8051"
                   MOVE    "継続管理加算、外来管理加算は算定"
                                       TO  SPA-ERRMSG
                   MOVE    "できません。"
                                       TO  SPA-ERRMSG(31:)
      *H25.4
               WHEN    "8052"
                   STRING  "外来管理加算（読み替え加算）は"
                           "入力できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "8100"
                   MOVE    "公害保険の診療コードです。入力できません。"
                                       TO  SPA-ERRMSG
      *H25.11
               WHEN    "8101"
                   STRING  "労災保険【公務災害】の診療コードです。"
                           "入力できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K811"
                   STRING  "警告！"
                           "京都府の労災保険【公務災害】の"
                           "診療コードです。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "S001"
                   STRING  "約束セット内に使用できないコードがあります"
                           "。この約束セットは使用できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "A001"
                   MOVE    "数量ゼロは入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "A002"
                   MOVE    "数量ゼロは入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9001"
      *            MOVE    "診療日付では入院は存在しません。"
      *                                TO  SPA-ERRMSG
      *            MOVE    "診療日付を変更して下さい。"
      *                                TO  SPA-ERRMSG(33:)
                   STRING   "診療日が入院期間ではありません。"
                            "登録はできません。"
                                        DELIMITED  BY  SIZE
                                        INTO      SPA-ERRMSG
                   END-STRING
               WHEN    "9002"
                   MOVE    "時間外区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "入院の日付は行を変えて入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "回数の行は入院の時のみできます。"
                                       TO  SPA-ERRMSG
               WHEN    "9005"
                   MOVE    "回数は剤の最後に入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9006"
                   MOVE    "日付は３１までです。"
                                       TO  SPA-ERRMSG
               WHEN    "9007"
                   MOVE    "入院中の日ではありません。"
                                       TO  SPA-ERRMSG
               WHEN    "9008"
                   MOVE    "訂正の時は訂正日のみの入力です。"
                                       TO  SPA-ERRMSG
               WHEN    "9009"
                   MOVE    "入院回数の／を入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "9010"
                   MOVE    "入院回数の／は１回のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "9012"
                   MOVE    "日付は２桁までです。"
                                       TO  SPA-ERRMSG
               WHEN    "9013"
                   MOVE    "日付−日付の期間が違います。"
                                       TO  SPA-ERRMSG
               WHEN    "9014"
                   STRING  "消炎鎮痛等処理などは逓減になります。"
                           "分けて入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9015"
                   STRING  "点滴手技料の算定できない液量です。"
                           "削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9016"
                   STRING  "手術が算定されています。"
                           "点滴手技料は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9017"
                   STRING  "生物学的製剤注射は加算できません。"
                           "削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9018"
                   STRING  "麻毒注射は加算できません。"
                           "削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9019"
                   STRING  "既に他の点滴手技が算定済みです。"
                           "削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9020"
                   STRING  "入院の点滴手技と薬剤は剤を分けて"
                           "入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9021"
                   STRING  "検査が逓減になります。剤を分けて"
                           "入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9022"
                   STRING  "既に点滴手技が算定済みです。"
                           "造影剤注入の点滴は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9023"
                   STRING  "点滴手技料が算定ができます。"
                           "造影剤注入の点滴手技は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9024"
                   MOVE    "この診療種別は入院のみの使用です。"
                                       TO  SPA-ERRMSG
               WHEN    "9025"
                   STRING  "中心静脈注射料を算定します。"
                           "点滴手技料は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9026"
                   STRING  "既に中心静脈注射料が算定済です。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9027"
                   STRING  "手術が算定されています。"
                           "中心静脈注射料は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9028"
                   MOVE    "この診療種別は外来のみの使用です。"
                                       TO  SPA-ERRMSG
               WHEN    "9029"
                   MOVE    "アフターケアは入院では選択できません。"
                                       TO  SPA-ERRMSG
               WHEN    "9036"
                   STRING  "入院中です。"
                           "アフターケア・自費保険以外は入力できません"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H24.9
               WHEN    "9041"
                   STRING  "アフターケアは入院の登録はできません。"
                           "保険変更または中途データ削除を行って下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H24.9
               WHEN    "9030"
                   STRING  "時間加算区分が選択中の時間加算区分と"
                           "違います。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9031"
                   MOVE    "入院患者初回加算は既に算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "9032"
                   STRING  "入院期間外の診療日です。"
                           "内容の展開ができませんので"
                           "会計照会で変更して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9033"
                   STRING  "入院期間外の診療日です。"
                           "診療内容の登録はできません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9034"
                   STRING  "既に入院期間中に算定済みです。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9035"
                   STRING  "入院中に１回のみの算定です。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9037"
                   STRING  "剤内の注加算コードが異なります。"
                           "注加算コード毎に剤を分けて下さい。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9038"
                   STRING  "保険組合せ有効期間外の日が指定されました。"
                           "日を修正して下さい。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!! 2011.11.15
               WHEN    "9040"
                   STRING  "入院の中心静脈注射の薬剤は"
                           ".350等で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H27.1
               WHEN    "9042"
                   STRING  "入院の中心静脈注射の器材は別剤で"
                           "入力してください。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
               WHEN    "9100"
                   STRING  "診療内容が４００行を超えます。"
                           "会計照会で訂正して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9200"
                   MOVE    "入院・食事は外来では入力できません。"
                                       TO  SPA-ERRMSG
      *
               WHEN    "1055"
                   MOVE    "複数科、複数保険は入力はできません。"
                                       TO  SPA-ERRMSG
      *
               WHEN    "1060"
                   MOVE    "既に内分泌負荷試験の当月点数が上限を"
                                       TO  SPA-ERRMSG
                   MOVE    "超えています。算定できません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "1061"
                   MOVE    "内分泌負荷試験点数エラー"
                                       TO  SPA-ERRMSG
               WHEN    "K060"
                   MOVE    "警告！内分泌負荷試験の訂正が必要です。"
                                       TO  SPA-ERRMSG
               WHEN    "K999"
                   STRING  "警告！！登録時と診療内容が違います。"
                           "必ず登録をして点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1062"
                   STRING  "包括まとめ対象の検査に日付入力があります。"
                           "検査のまとめはできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *        コンタクトレンズ検査
               WHEN    "K600"
                   STRING  "警告！コンタクトレンズ検査料"
                           "は既に算定されています。"
                           "初回装用者は算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0600"
                   STRING  "コンタクトレンズ検査料算定時に算定できない"
                           "検査です。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.4
               WHEN    "0604"
                   STRING  "コンタクトレンズ検査料算定時に"
                           "算定できません"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9300"
                   STRING  "患者登録の業務権限はありません。"
                           "患者登録はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9301"
                   MOVE    "明細がありません。登録できません。"
                                                TO  SPA-ERRMSG
      *
               WHEN    "K140"
                   STRING  "警告！手術が算定されています。"
                           "注射料は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K130"
                   STRING  "警告！特定薬剤治療管理料が３月目以内です。"
                           "算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K131"
                   STRING  "この特定薬剤治療管理料は算定済みです。"
                           "算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K132"
                   STRING  "既に特定薬剤治療管理料は当月算定済です。"
                           "初回算定できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K133"
                   MOVE    "初回算定日には診療年月を入力して下さい。"
                                       TO  SPA-ERRMSG
      *Ｈ１８年度
               WHEN    "K138"
                   MOVE    "警告！前回の初回算定日から１年未満です。"
                                       TO  SPA-ERRMSG
               WHEN    "K139"
                   STRING  "警告！初回算定日から"
                           "１２週以上経過しています。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1300"
                   STRING  "初回が算定してありません。"
                           "初回を算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1301"
                   STRING  "既に４回算定しています。"
                           "５回目を算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1302"
                   STRING  "２回目から４回目の算定回数が足りません。"
                           "３回算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1303"
                   STRING  "既に５回目の算定をしています。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K401"
                   STRING  "警告！処置の手技が剤内に複数あります。"
                           "手技毎に剤分離して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0401"
                   STRING  "処置の手技に通則加算があります。"
                           "手技毎に剤分離して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H25.7   から
               WHEN    "K402"
                   STRING  "警告！労災の合成コードで"
                           "算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0402"
                   STRING  "手技が複数あります。"
                           "労災の合成コードで算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0404"
                   STRING  "手技と薬剤が混在しています。"
                           "手技毎に剤分離するか、"
                           "薬剤をまとめて下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0405"
                   STRING  "手技と器材が混在しています。"
                           "手技毎に剤分離するか、"
                           "器材をまとめて下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K404"
                   STRING  "警告！"
                           "手技と薬剤が混在しています。"
                           "手技毎に剤分離するか、"
                           "薬剤をまとめて下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K405"
                   STRING  "警告！"
                           "手技と器材が混在しています。"
                           "手技毎に剤分離するか、"
                           "器材をまとめて下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K406"
                   STRING  "警告！"
                           "複数の手技と時間外加算があります。"
                           "労災の合成コードで算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
               WHEN    "K501"
                   STRING  "警告！手術の手技が剤内に複数あります。"
                           "手技毎に剤分離して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K502"
                   STRING  "警告！手術の手技が２つあります。"
                           "従たる手術の逓減を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H18.4改正追加
               WHEN    "2063"
                   STRING  "後発医薬品への変更不可は投薬の"
                           "薬剤の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2064"
                   STRING  "後発医薬品への変更不可は"
                           "薬剤の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2065"
                   STRING  "上の薬剤は後発医薬品です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2066"
                   STRING  "上の薬剤に後発医薬品はありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2067"
                   STRING  "初診・再診と同日初診の診療科が同じです。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K267"
                   STRING  "警告！！"
                           "初診・再診と同日初診の診療科が同じです。"
                           "保険を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2070"
                   STRING  "診察料を削除後、算定してください。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2069"
                   STRING  "当日に初診・再診が算定されていません。"
                           "同日初診は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2068"
                   STRING  "極低体重出生時加算の算定できない手術です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8027"
                   STRING  "リハビリテーション料が"
                           "１日９単位以上になります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K827"
                   STRING  "警告！１日６単位以上になります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K831"
                   STRING  "警告！心大血管疾患リハビリテーション"
                           "発症日から１５０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K832"
                   STRING  "警告！脳血管疾患リハビリテーション"
                           "発症日から１８０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K833"
                   STRING  "警告！運動器リハビリテーション"
                           "発症日から１５０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K834"
                   STRING  "警告！呼吸器リハビリテーション"
                           "発症日から９０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8028"
                   STRING  "救急医療管理加算は入院日から７日間のみ"
                           "算定可能です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8029"
                   STRING  "心大血管疾患リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8030"
                   STRING  "心大血管疾患リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8031"
                   STRING  "脳血管疾患リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8032"
                   STRING  "脳血管疾患リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8033"
                   STRING  "運動器リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8034"
                   STRING  "運動器リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8035"
                   STRING  "呼吸器リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8036"
                   STRING  "呼吸器リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8037"
                   STRING  "摂食機能療法開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8038"
                   STRING  "摂食機能療法終了日は開始日と"
                           "同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8039"
                   STRING  "難病患者リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8040"
                   STRING  "難病患者リハビリテーション開始日と"
                       "終了日は同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8041"
                   STRING  "障害児リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8042"
                   STRING  "障害児リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8043"
                   STRING  "リハビリテーション料は逓減になります。"
                       "逓減を分けて入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8044"
                   STRING  "労災・自賠責ではリハビリテーション料の"
                           "逓減はありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8045"
                   STRING  "労災・自賠責ではリハビリテーション"
                           "医学管理料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *投薬附加情報エラー
               WHEN    "0200"
                   STRING  "長期投与日数以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K200"
                   STRING  "警告！長期投与日数以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0201"
                   STRING  "投与量以上です。数量を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K201"
                   STRING  "警告！"
                           "投与量以上です。数量を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!
               WHEN    "K202"
                   STRING  "警告！"
                           "剤内に包括診療コード以外の算定があります。"
                           "剤分離をして下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!
               WHEN    "K210"
                   STRING  "警告！！"
                           "セット内に患者禁忌薬剤があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K211"
                   STRING  "警告！！"
                           "患者禁忌薬剤です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K212"
                   STRING  "警告！！"
                           "経過措置前が患者禁忌薬剤です。"
                           "患者禁忌薬剤を登録して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K213"
                   STRING  "警告！！"
                           "セット内の患者禁忌薬剤は経過措置前"
                           "です。"
                           "患者禁忌薬剤を登録して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *Ｈ２０．４から
               WHEN    "K835"
                   STRING  "警告！心大血管疾患リハビリ"
                           "発症日から１５０日以上です。"
                           "１３単位以上算定できません。"
                           "月１３単位を超えました。"
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K836"
                   STRING  "警告！脳血管疾患リハビリ"
                           "発症日から１８０日以上です。"
                           "月１３単位を超えました。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K837"
                   STRING  "警告！運動器リハビリ"
                           "発症日から１５０日以上です。"
                           "月１３単位を超えました。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K838"
                   STRING  "警告！呼吸器リハビリ"
                           "発症日から９０日以上です。"
                           "月１３単位を超えました。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8047"
                   STRING  "早期リハビリテーション加算を"
                           "算定できる期間ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K839"
                   STRING  "警告！！"
                           "該当のリハビリテーション発症日が"
                           "ありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.5
               WHEN    "K840"
                   STRING  "警告！！"
                           "入院日から６月以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K841"
                   STRING  "警告！！"
                           "入院日から３月以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K842"
                   STRING  "警告！！"
                           "初回入院日から６月以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K843"
                   STRING  "警告！！"
                           "初回入院日から３月以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K844"
                   STRING  "警告！！"
                           "継続入院です。"
                           "家族入院精神療法は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!
               WHEN    "0210"
                   STRING  "夜間・早朝加算の施設基準ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0212"
                   STRING  "入院中の上限回数を超えています。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "K214"
                   STRING  "警告！"
                           "初回入院日からの回数が上限回数を"
                           "超えています。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0213"
                   STRING  "画像等手術支援加算の算定できない手術です。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0214"
                   STRING  "この画像等手術支援加算は算定できない"
                           "手術です。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0215"
                   STRING  "初診料算定日より１年以上経過しています。"
                           "算定できません。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K216"
                   STRING  "警告！閉鎖循環式全身麻酔と他の手技は"
                           "剤を分けて下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8046"
                   STRING  "健保のリハビリテーション料です。"
                           "労災のコードで算定して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!
      *        入院料包括
               WHEN    "0217"
                   STRING  "当月の入院料が算定されていません。"
                           "入院会計照会で作成して下さい。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0218"
                   STRING  "当日の入院料以外が算定されている日が"
                           "指定されました。"
                           "算定できません。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0219"
                   STRING  "出来高算定コードは剤の最初に入力して下さい"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9210"
                   STRING  "切替できる医療機関番号ではありません。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.5
               WHEN    "0202"
                   STRING   "換算値×数量の桁数がオーバーします。"
                            "数量を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0203"
                   STRING   "種類数指示は外来の内服薬剤のみ入力可能"
                            "です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0204"
                   STRING   "数量換算値の設定がありません。"
                            "換算値入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0205"
                   STRING   "数量を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0206"
                   STRING   "換算値入力、種類数指示、関係コメント指示は"
                            "Ｈ２０年４月からの機能です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0207"
                   STRING   "換算値入力は薬剤のみ入力できます。"
                            "換算値入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0208"
                   STRING   "関係コメントの前に関係するコードが"
                            "ありません。入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0209"
                   STRING   "入院・食事には関係コメントは"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0220"
                   STRING   "診療科の受診履歴は最大件数登録済みです。"
                            "この診療科での入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0221"
                   STRING   "薬評・器評・加評・体評は診療種別の直下に"
                            "入力して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0222"
                   STRING   "減点分は再診・指導・在宅・入院の診療種別の"
                            "直下に入力して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0223"
                   STRING   "合計点数がマイナスとなります。"
                            "減点点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0224"
                   STRING   "減点分の診療区分の合計点数がマイナスと"
                            "なります。"
                            "減点点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0225"
                   STRING   "薬評・器評・加評での入力はできません。"
                            "剤分離して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0226"
                   STRING   "減点分の入力はできません。"
                            "剤分離して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K227"
                   STRING   "警告！！"
                            "月上限回数を超えています。"
                            "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K228"
                   STRING   "警告！！"
                            "月総投与量を超えています。"
                            "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0501"
                   STRING   "この診療種別は平成２０年４月から有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0502"
                   STRING   "この診療種別は外来の労災・自賠責での対象"
                            "です。入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.6
               WHEN    "0503"
                   STRING   "この診療種別は平成２２年４月から有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "0504"
                   STRING   "この診療種別は平成２６年４月から有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0505"
                   STRING   "この診療種別は平成２６年１０月から"
                            "有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.1
               WHEN    "0506"
                   STRING   "この診療種別は平成２７年４月から"
                            "有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *Ｈ２２．４から
               WHEN    "0270"
                   STRING  "ＣＴ、ＭＲＩが逓減になります。"
                           "剤を分けて入力して下さい"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0260"
                   STRING  "組織診断料を算定できる検査を入力して"
                           "下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0261"
                   STRING  "細胞診断料を算定できる検査を入力して"
                           "下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K262"
                   STRING  "警告！！退院日より１月超です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K263"
                   STRING  "警告！！退院日より３月超です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K264"
                   STRING  "警告！！退院日の登録がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0266"
                   STRING  "認知療法・認知行動療法と他の精神科専門"
                           "療法は同時算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0267"
                   STRING  "含量規格変更不可・剤形変更不可は"
                           "薬剤の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0268"
                   STRING  "含量規格変更不可・剤形変更不可は"
                           "投薬の薬剤の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K269"
                   STRING  "警告！！"
                           "医療観察法専用コードです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "K270"
                   STRING  "警告！！"
                           "精神科ケア早期加算開始日から１年超です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0280"
                   STRING  "同時撮影の撮影料です。"
                           "撮影料の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8048"
                   STRING  "がん患者リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8049"
                   STRING  "がん患者リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.6
               WHEN    "0601"
                   STRING  "上記検査は重複算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0602"
                   STRING  "上記検査は重複算定できません。"
                           "２項目算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.7
               WHEN    "K601"
                   STRING  "警告！"
                           "上記検査は重複算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0603"
                   STRING  "時間外加算と初・再診料が違います。"
                           "正しい時間外加算を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K845"
                   STRING  "警告！"
                           "早期リハビリテーション加算を"
                           "算定できる期間ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0229"
                   STRING  "包括算定コードは剤の最初に入力して下さい"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0129"
                   STRING  "コメントの入力値は８文字までです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K029"
                   STRING  "警告！"
                           "入力値変換後の名称が表示できません。"
                           "コメントの入力値を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
               WHEN    "K901"
                   STRING  "警告！！"
                           "救急搬送診療料が入院期間中に"
                           "算定されています。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
      *H24.4改正追加
               WHEN    "2071"
                   STRING  "一般名記載・銘柄名記載のコードは投薬の"
                           "薬剤の下に入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8070"
                   STRING  "初期加算（リハビリテーション料）を"
                           "算定できる期間ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K873"
      ********     STRING  "警告！"
      *                    "初診料算定日より１年以上経過しています。"
      *                    "算定できません。"
      *                                DELIMITED  BY  SPACE
      *                                INTO    SPA-ERRMSG
      ********     END-STRING
                   STRING  "警告！"
                           "初診料算定日からの算定期間を"
                           "経過しています。"
                           "算定できません。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H26.4
               WHEN    "K874"
                   STRING  "警告！"
                           "通院・在宅精神療法開始日からの"
                           "算定期間を経過しています。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K870"
                   STRING  "警告！"
                           "初期加算（リハビリテーション料）を"
                           "算定できる期間ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K871"
                   STRING  "警告！脳血管疾患等リハビリテーション"
                           "発症日の１８０日超から算定して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K872"
                   STRING  "警告！運動器リハビリテーション"
                           "発症日の１５０日超から算定して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K272"
                   STRING  "警告！"
                           "組織診断料の入力がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K273"
                   STRING  "警告！"
                           "細胞診断料の入力がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K244"
                   STRING  "警告！！"
                           "外来緩和ケア管理料を算定しています。"
                           "麻薬の算定がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1421"
                   MOVE
                   "在宅がん医療総合診察料を算定時、算定できません"
                                       TO  SPA-ERRMSG
      *ver.4.7
               WHEN    "0240"
                   STRING   "減点は診療種別の直下に入力して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0241"
                   STRING   "薬剤料減点（合算薬剤料上限超）は"
                            "その他注射で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0242"
                   STRING   "薬剤料減点（合算薬剤料上限超）は"
                            "入院で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K119"
                   STRING   "警告！"
                            "内服滴剤の薬剤の日数が１以上です。"
                            "総量で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1105"
                   STRING  "この初診の初診算定日は平成２８年４月以降"
                           "にして下さい。"
                                           DELIMITED   BY  SIZE
                                           INTO    SPA-ERRMSG
                   END-STRING
      *H25.2
               WHEN    "7011"
                   STRING  "レセプト電算データから移行したデータです。"
                           "訂正できません。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H26.4 から
               WHEN    "0511"
                   STRING  "副鼻腔手術用内視鏡加算の算定できない"
                           "手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0512"
                   STRING  "副鼻腔手術軟骨部組織切除器加算の"
                           "算定できない手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0513"
                   STRING  "この脊髄誘発電位測定等加算の算定できない"
                           "手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0514"
                   STRING  "脊髄誘発電位測定等加算は算定できない"
                           "手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0541"
                   STRING  "長時間麻酔管理加算の算定できない"
                           "麻酔です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K990"
                   STRING  "警告！！"
                           "４月改定の点数ではありません。"
                           "至急、点数マスタをチェックしてください。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *R01.10 改正
               WHEN    "K991"
                   STRING  "警告！ "
                           "１０月改定の点数ではありません。"
                           "至急、点数マスタをチェックしてください。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H27.1 改正から
               WHEN    "K111"
                   STRING  "警告！"
                           "妥結率５割以下の設定ではありません。"
                           "確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K112"
                   STRING  "警告！"
                           "妥結率５割以下の設定です。"
                           "妥結率５割以下のコードを入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1104"
                   STRING  "この初診の初診算定日は平成２７年１月以降"
                           "にして下さい。"
                                           DELIMITED   BY  SIZE
                                           INTO    SPA-ERRMSG
                   END-STRING
      *!!!
      *H27.6
               WHEN    "2072"
                   STRING  "後発品変更不可（処方単位）・"
                           "後発品変更可（処方単位）"
                           "が重複します。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2073"
                   STRING  "後発品変更不可（処方単位）は"
                           "入力できません。"
                           "システム管理が変更不可です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2074"
                   STRING  "後発品変更可（処方単位）は入力できません。"
                           "システム管理が変更可です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H27.4
               WHEN    "K116"
                   STRING  "警告！"
                           "低紹介率の設定ではありません。"
                           "投薬の逓減は行いません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H29.9
               WHEN    "0542"
                   STRING  "加算の所定点数に該当しない項目があります。"
                           "別剤として入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      *H30.2
               WHEN    "0701"
                   STRING  "撮影料の合計数量と違います。"
                           "同じ数量を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K701"
                   STRING  "警告！撮影料の合計数量と違います。"
                           "同じ数量を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      *H30.4
               WHEN    "0181"
                   STRING  "妊婦加算と初・再診料が違います。"
                           "正しい加算を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0182"
                   STRING  "６歳未満です。妊婦加算の入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0183"
                   STRING  "男性です。妊婦加算の入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0184"
                   STRING  "既に妊婦加算の入力があります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0185"
                   STRING  "小児・産・産婦人科特例時間外は"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0186"
                   STRING  "産・産婦人科特例時間外は"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0187"
                   STRING  "妊婦加算の入力できる剤ではありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0281"
                   STRING  "分割調剤は院外投薬で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0282"
                   STRING  "分割調剤は１剤に１つ入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0283"
                   STRING  "分割調剤は３０日以上で入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0284"
                   STRING  "分割調剤の日数入力が違います。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0285"
                   STRING  "分割調剤の合計日数と投薬日数が違います。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0286"
                   STRING  "分割調剤の内容がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0287"
                   STRING  "注加算コードのある加算はまとめて"
                           "入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K120"
                   STRING  "警告！"
                           "血行促進・皮膚保湿剤の入力があります。"
                           "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K274"
                   STRING  "警告！"
                           "悪性腫瘍病理組織標本加算の"
                           "算定できる手術の算定がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K503"
                   STRING  "警告！術中脳灌流モニタリング加算の"
                           "算定できる手術の算定がありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.5
               WHEN    "K088"
                   STRING  "警告！"
                           "回数は１回のみです。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.6
               WHEN    "1088"
                   STRING  "保険外金額が７桁を超えます。"
                           "桁数オーバーとなります。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1089"
                   STRING  "保険外金額の合計が７桁を超えます。"
                           "桁数オーバーとなります。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1090"
                   STRING  "点数合計が７桁を超えます。"
                           "桁数オーバーとなります。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0702"
                   STRING  "ＭＲＩ撮影料と小児鎮静下ＭＲＩ撮影加算"
                           "が違います。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *H30.9
               WHEN    "1106"
                   STRING  "剤内に選択式コメントの必要な"
                           "診療はありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1107"
                   STRING  "選択式コメントは、平成３０年４月から"
                            "有効です。"
                            "入力できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K117"
                   STRING  "警告！"
                           "選択式コメントの前行で剤終了しています。"
                           "回数行を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *H31.3
               WHEN    "0248"
                   STRING   "外用薬の総量が桁数オーバーします。"
                            "数量と回数を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0249"
                   STRING   "点数が桁数オーバーします。"
                            "数量と回数を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K229"
                   STRING   "警告！"
                            "向精神薬長期処方減算対象の"
                            "投薬がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *H31.4
               WHEN    "0820"
                   STRING   "平成は３１年４月までです。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0821"
                   STRING   "令和は１年５月からです。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K822"
                   STRING   "警告！"
                            "コメントの元号と年を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0143"
                   STRING   "既に特定薬剤治療管理料初回算定の"
                            "コメントが入力されています。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *R02.4   改正
               WHEN    "0851"
                   STRING  "年は元号（４または５）＋年の"
                           "数値３桁で入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0852"
                   STRING  "入力値が暦日ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K851"
                   STRING  "警告！入力値の日付が平成以前です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K852"
                   STRING  "警告！入力値の日付が診療日付より後です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K853"
                   STRING  "警告！令和２年４月からは日の入力が"
                           "必要です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0853"
                   STRING  "時刻は時分（HH MM）で入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0831"
                   STRING  "入力コードの検索ができません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0832"
                   STRING  "点数マスタに存在しません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0833"
                   STRING  "入力値の数値エラーです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0144"
                   STRING  "重複できない特定薬剤治療管理加算の"
                           "入力があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0516"
                   STRING  "創外固定器加算の"
                           "算定できない手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0517"
                   STRING  "超音波切削機器加算の"
                           "算定できない手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K512"
                   STRING  "警告！"
                           "左心耳閉鎖術併施の手術の算定がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K992"
                   STRING  "警告！"
                           "４月改定の点数マスタではありません。"
                           "至急、点数マスタをチェックして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *TEST
      *TEST
      *
           END-EVALUATE
           .
       100-KYOUTU-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラム別メッセージ編集処理
      *****************************************************************
       200-PG-ERRMSG-SEC            SECTION.
      *
           EVALUATE    LNK-SC96-PG
               WHEN    "K02"
                   PERFORM 2001-K02-ERRMSG-SEC
               WHEN    "KT01"
                   PERFORM 2001-K02-ERRMSG-SEC
               WHEN    "K05"
                   PERFORM 2002-K05-ERRMSG-SEC
               WHEN    "J04"
                   PERFORM 2003-J04-ERRMSG-SEC
           END-EVALUATE
           .
       200-PG-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為エラーメッセージ編集処理
      *****************************************************************
       2001-K02-ERRMSG-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1000"
                   MOVE    "ＤＯ選択エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "処理エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "数量・金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "該当の患者が存在しません。" 
                                       TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "患者マスタがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号を入力して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "保険組合がありません。処理できません"
                                       TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "保険組合がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "訂正の時、診療科の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1009"
                   MOVE    "訂正の時、診療日の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE    "診療日入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "誕生日前の診療日は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "対象のデータがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "訂正で継続確定済みの時、"
                                       TO  SPA-ERRMSG(1:24)
                   MOVE    "保険の変更はできません"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1014"
                   STRING  "処方せん料を算定しません。"
                           "特定疾患処方管理加算（処方せん料）は"
                           "算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1015"
                   STRING  "処方料を算定しません。"
                           "特定疾患処方管理加算（処方料）は"
                           "算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *VER.4.3.0 から警告へ
               WHEN    "K114"
                   STRING  "警告！処方せん料を算定しません。"
                           "特定疾患処方管理加算のみの算定となります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K115"
                   STRING  "警告！処方料を算定しません。"
                           "特定疾患処方管理加算のみの算定となります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1016"
                   MOVE    "初診料への自動変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1046"
                   STRING  "再診料への自動変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1101"
                   MOVE    "初診算定日＜診療日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "1102"
                   MOVE    "初診算定日が暦日ではありません。"
                                       TO  SPA-ERRMSG
      *H25.4
               WHEN    "1103"
                   STRING  "この初診の初診算定日は平成２５年４月以降"
                           "にして下さい。"
                                           DELIMITED   BY  SIZE
                                           INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1017"
      *?           MOVE
      *            "初診算定日を登録して、再診料自動算定をして下さい"
      *?                               TO  SPA-ERRMSG
                   STRING  "初診算定日を初診料の横に入力して初診算定日"
                           "を登録後、再診料の自動算定をして下さい"
                                           DELIMITED   BY  SIZE
                                           INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1058"
                   STRING  "初診算定日がありません。"
                           "初診算定日を登録して下さい。"
                                           DELIMITED   BY  SIZE
                                           INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1018"
                   MOVE
                   "対象患者の中途終了データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1019"
                   MOVE
                   "中途終了データはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   STRING  "初診料が算定されてません。"
                           "初診料を算定後、算定して下さい"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1040"
                   MOVE    "往診料は算定できません。"
                                       TO  SPA-ERRMSG
               WHEN    "1042"
                   STRING  "初診料が算定されています。"
                           "同時算定できません。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1041"
                   STRING  "初診料と寝たきり老人退院時共同指導料は"
                           "同時算定できません。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1043"
                   STRING  "初診料が算定されていません。"
                           "ＤＵＭＭＹの初診料を算定して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1044"
                   STRING  "初診料を複数科・保険組合せで"
                           "算定しないで下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "K144"
                   STRING  "警告！！診察料を複数科・保険組合せで"
                           "算定しないで下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1021"
                   MOVE    "処方料・処方せん料は１つのみの算定です。"
                                       TO  SPA-ERRMSG
               WHEN    "1022"
                   MOVE    "消炎鎮痛等処置などは既に同日算定済みです。"
                                       TO  SPA-ERRMSG
               WHEN    "1023"
                   STRING  "初診算定日が自動算定されています。"
                           "中途終了はできません。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1024"
                   MOVE    "訂正中です。中途終了はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   STRING  "初診料算定のできない算定があります。"
                           "初診料への変換はできません。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1026"
                   MOVE    "診療室入力の時は、訂正はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1027"
                   STRING  "老人の保険割合が設定されていません。"
                           "保険を登録し直して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1028"
                   STRING  "保険組合に矛盾があります。"
                           "患者登録で保険を登録し直して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1029"
                   STRING  "訂正で継続確定済みの時、"
                           "診療科の変更はできません"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1030"
                   STRING  "処方せんプログラムがありません。"
                           "システム管理を確認して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1031"
                   STRING  "薬剤情報印刷プログラムがありません。"
                           "システム管理を確認して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1032"
                   STRING  "公費の期間に矛盾があります。"
                           "患者登録で保険組合せを登録し直して下さい。"
                                                DELIMITED  BY  SIZE
                                       INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "1033"
                   MOVE    "訂正で包括分入力の変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1034"
                   MOVE    "保険組合せがありません。選択し直して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "K034"
                   MOVE    "警告！！地方公費のみの保険組合せです。"
                                       TO  SPA-ERRMSG
               WHEN    "1036"
                   STRING  "受診履歴の伝票番号がゼロです。"
                           "訂正できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1037"
                   STRING  "処方料が算定されません。"
                           "長期投薬加算（処方料）は算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                       STRING  "処方料が算定されません。"
                               "特定疾患処方管理加算２（処方料）"
                               "は算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
               WHEN    "1038"
                   STRING  "処方せん料が算定されません。"
                           "長期投薬加算（処方せん料）は算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                       STRING  "処方箋料が算定されません。"
                               "特定疾患処方管理加算２（処方箋料）"
                               "は算定できません"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                       END-STRING
                   END-IF
      *VER.4.3.0 から警告追加
               WHEN    "K038"
                   STRING  "警告！処方せん料を算定しません。"
                           "長期投薬加算（処方せん料）のみの算定と"
                           "なります"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "警告！処方箋料を算定しません。"
                           "特定疾患処方管理加算２（処方箋料）"
                           "のみの算定となります"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
                   END-IF
               WHEN    "K037"
                   STRING  "警告！処方料を算定しません。"
                           "長期投薬加算（処方料）のみの算定となります"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "警告！処方料を算定しません。"
                           "特定疾患処方管理加算２（処方料）"
                           "のみの算定となります"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
                   END-IF
               WHEN    "1039"
                   STRING  "既に処方せん料が算定済みです。"
                           "調剤技術基本料は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *        Ｈ２０．４改正から
               WHEN    "1076"
                   STRING  "後期高齢者の年齢です。"
                           "後期高齢者の保険を登録して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K076"
                   STRING  "警告！！後期高齢者の年齢です。"
                           "後期高齢者の保険を登録して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1077"
                   STRING  "６５歳以上です。"
                           "退職国保は対象ではありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *        Ｈ１８．１０改正から
               WHEN    "1070"
      *            STRING  "老人の保険割合が２割です。"
      *                    "３割に保険を登録し直して下さい。"
      *                                DELIMITED  BY  SIZE
      *                                INTO    SPA-ERRMSG
      *            END-STRING
      *H26.5 から　２割も対象なるので、割合削除
                   STRING  "老人の保険割合が違います。"
                           "保険を登録し直して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1071"
                   STRING  "国保の前期高齢者到達日以降の確認が"
                           "ありません。保険を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K171"
                   STRING  "警告！！前期高齢者到達日以降に保険確認が"
                           "してありません。確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K172"
                   STRING  "警告！！前回と保険組合せが違います。"
                           "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K173"
                   STRING  "警告！！"
                           "前回の保険組合せは削除になっています。"
                           "保険組合せを確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K174"
                   STRING  "警告！！"
                           "当月に初診料と併用算定できないコードが登録"
                           "されています。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K175"
                   STRING  "警告！！"
                           "初診料は併用算定エラーです。"
      **************       "初診料の自動発生はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K177"
                   STRING  "警告！！"
                          "当月に生活習慣病管理料を算定しています。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *!
               WHEN    "1050"
                   STRING  "労災・自賠責・公害、包括分入力の"
                           "複数保険入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1051"
                   MOVE    "保険組合せがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1052"
                   MOVE    "主保険と老人・一般の判定が違います。"
                                       TO  SPA-ERRMSG
                   MOVE    "複数保険入力はできません。"
                                       TO  SPA-ERRMSG(37:)
               WHEN    "1053"
                   MOVE    "診療科がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1054"
                   MOVE    "ドクターがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "1056"
                   STRING  "複数科、複数保険は１行目には"
                           "入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1057"
                   STRING  "継続入力・複数科まとめで入力されています。"
                           "訂正での複数科・保険入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1200"
                   MOVE    "入院帳票オーダマスタ更新エラー。"
                                       TO  SPA-ERRMSG
               WHEN    "1201"
                   MOVE    "入院帳票が印刷エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "1202"
                   MOVE    "入院処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1203"
                   MOVE    "注射処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1204"
                   MOVE    "指示箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(31:)
      *!
               WHEN    "1205"
                   MOVE    "初回算定日＜診療日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "1206"
                   MOVE    "初回算定日≦最終来院日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "1207"
                   STRING   "既に初診算定日が登録されています。"
                           "算定日の登録はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1208"
                   STRING  "既に算定履歴が登録されています。"
                           "初回算定日の登録はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1209"
                   STRING  "Ｈ１８年４月１日以降にして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!
               WHEN    "K001"
                   MOVE    "警告！時間外算定がありません"
                                       TO  SPA-ERRMSG
               WHEN    "K002"
                   MOVE    "警告！初診算定日より１月以内です"
                                       TO  SPA-ERRMSG
               WHEN    "K022"
                   MOVE    "警告！退院日より１月以内です"
                                       TO  SPA-ERRMSG
               WHEN    "K032"
                   MOVE    "警告！他院退院日より１月以内です"
                                       TO  SPA-ERRMSG
               WHEN    "K003"
                   MOVE    "警告！撮影料は１回目の手技料が算定できます"
                                       TO  SPA-ERRMSG
               WHEN    "K004"
                   MOVE    "警告！既に今月算定済みです。"
                                       TO  SPA-ERRMSG
      *        WHEN    "K018"
      *            MOVE    "警告！既に今月算定上限回数を超えています。"
      *                                TO  SPA-ERRMSG
               WHEN    "K005"
                   STRING  "警告！既に診察料を算定してます。"
                           "小児科外来診療料の自動発生はしません"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K006"
                   MOVE    "警告！既に点滴手技料を算定しています。"
                                       TO  SPA-ERRMSG
               WHEN    "K009"
                   STRING  "警告！当日の点滴合計液量と点滴手技料が"
                           "違います。まとめて算定して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K007"
                   STRING  "警告！特定薬剤治療管理料は４月目以降の"
                           "算定に変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K008"
      ***********  MOVE    "警告！７５歳に到達しました。"
                   STRING  "警告！！老人年齢に到達しました。"
                           "老人公費を入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K010"
                   STRING  "警告！手術と点滴手技料が同日に"
                           "算定されています。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K011"
                   STRING  "警告！既に違う手技料が同日に"
                           "算定されています。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K012"
                   STRING  "警告！点滴手技料の算定できる"
                           "液量ではありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K013"
                   STRING  "警告！既に造影剤注入点滴手技料が"
                           "算定済です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K014"
                   STRING  "警告！既に点滴手技料が算定済です。"
                           "中心静脈注射は自動算定しません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K015"
                   STRING  "警告！手術と中心静脈注射料が同日に"
                           "算定されています。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K020"
                   MOVE    "警告！再診料の受診回数が違います。"
                                       TO  SPA-ERRMSG
               WHEN    "K016"
                   STRING  "警告！診療情報提供料が"
                           "２回以上の算定になります。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K017"
                   STRING  "警告！既に外来で調剤技術基本料が算定済み"
                           "です。自動算定しません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K023"
                   STRING  "警告！既に処方せん料が算定済みです。"
                           "調基の自動算定はしません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K024"
                   STRING  "警告！既に調剤技術基本料が算定済みです。"
                           "処方せん料を算定しますので削除して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K019"
                   STRING  "警告！既に特処加算を算定済みです。"
                         "長期投薬加算を算定する場合は削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
      *R01.6
      **           STRING  "警告！既に特処加算１を算定済みです。"
      *                    "特処加算２"
      *                    "を算定する場合は削除して下さい。"
      *                                DELIMITED  BY  SIZE
      *                                INTO    SPA-ERRMSG
      **           END-STRING
                   MOVE    SPACE       TO  SPA-ERRMSG
                   STRING  "警告！上記算定済みです。"
                           "特処加算２"
                           "を算定する場合は削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
                   END-IF
               WHEN    "K021"
                   MOVE    "警告！２８日以上の投薬がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "K540"
                   STRING  "警告！神経ブロック・トリガーポイント注射・"
                           "神経幹内注射は同時算定できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K091"
                   MOVE    "警告！既に当日算定回数を超えています。"
                                       TO  SPA-ERRMSG
      *Ｈ２０．４から
               WHEN    "K240"
                   STRING  "警告！！"
                           "初回算定から２年以上経過しています。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0241"
                   MOVE    "後期高齢者診療料を算定時、算定できません"
                                       TO      SPA-ERRMSG
               WHEN    "K241"
                   STRING  "警告！！"
                           "後期高齢者診療料を算定してます。"
                           "急性増悪時のみ算定可能です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K242"
                   STRING  "警告！！"
                           "人工腎臓の当月の上限回数を超えています。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K243"
                   STRING  "警告！！"
                           "がん性疼痛緩和指導管理料を算定しています。"
                           "麻薬の算定がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K026"
                   STRING  "警告！！"
                           "初診算定日より１月以内です。"
                           "初診算定日した保険を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K134"
                   STRING  "警告！悪性腫瘍特異物資治療管理料は"
                           "すでに算定済みです。"
                           "初回加算は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K176"
                   STRING  "警告！"
                           "有効な保険組合せに変更しました。"
                           "必ず登録して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.5
               WHEN    "K178"
                   STRING  "警告！"
                           "会計照会で受診履歴に変更がありました。"
                           "前回患者ボタンを押下して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0178"
                   STRING  "会計照会で受診履歴に変更がありました。"
                           "前回患者ボタンを押下して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *!!!!!
               WHEN    "0179"
                   STRING  "患者登録で変更がありました。"
                           "前回患者ボタンを押下して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *!!!!!
      *Ｈ２２．４から
               WHEN    "0230"
                   STRING  "処方せん料が算定されません。"
                           "抗悪性腫瘍剤処方管理加算（処方せん料）は"
                           "算定不可です"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K230"
                   STRING  "警告！処方せん料を算定しません。"
                           "抗悪性腫瘍剤処方管理加算（処方せん料）"
                           "のみの算定"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0231"
                   STRING  "処方料が算定されません。"
                           "抗悪性腫瘍剤処方管理加算（処方料）は"
                           "算定不可です"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K231"
                   STRING  "警告！処方料を算定しません。"
                           "抗悪性腫瘍剤処方管理加算（処方料）"
                           "のみの算定です"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.6
               WHEN    "0901"
                   STRING  "週の算定上限回数を超えています。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K903"
                   MOVE    "警告！既に週の算定上限回数を超えています。"
                                       TO  SPA-ERRMSG
               WHEN    "0902"
                   STRING  "１日の算定上限回数を超えています。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K902"
                   STRING  "警告！"
                           "既に１日の算定上限回数を超えています。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1059"
                   STRING  "入院中です。自費保険以外の"
                           "複数保険入力はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "0170"
                   STRING  "診療日に同日再入院はありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0171"
                   STRING  "同日再入院日ではありません。"
                           "内容をクリアして受診履歴を削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0172"
                   STRING  "該当の中途終了データの内容に置換えました。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H24.7
      *        WHEN    "0122"
      *            STRING  "院内投薬のみの算定です。"
      *                    "処方せん料は算定できません。"
      *                                DELIMITED  BY  SIZE
      *                                INTO  SPA-ERRMSG
      *****        END-STRING
               WHEN    "K122"
                   STRING  "警告！"
                           "院内投薬のみの算定です。"
                           "処方せん料を削除して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K232"
                   STRING  "警告！"
                           "内服薬剤が７種類以上となります。"
                           "処方せん料を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K233"
                   STRING  "警告！"
                           "内服薬剤が７種類未満です。"
                           "処方せん料を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *!!!
      *H24.4改正追加
               WHEN    "2100"
                   STRING  "再診と同一２科目の診療科が同じです。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2101"
                   STRING  "当日に再診が算定されていません。"
                           "再診料（同一２科目）は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2102"
                   STRING  "初診料が算定されています。"
                           "再診料（同一２科目）は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2103"
                   STRING  "電話再診の算定がありません。"
                           "電話再診（同一２科目）は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2106"
                   STRING  "電話再診が算定されています。"
                           "再診料（同一２科目）は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2104"
                   STRING  "外来リハビリテーション診療料が算定中です。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2105"
                   STRING  "外来放射線照射診療料が算定中です。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K104"
                   STRING  "警告！"
                           "外来リハビリテーション診療料が算定中です。"
                           "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K105"
                   STRING  "警告！"
                           "外来放射線照射診療料が算定中です。"
                           "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0234"
                   STRING  "処方せん料が算定されません。"
                           "一般名処方加算（処方せん料）は"
                           "算定不可です"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K234"
                   STRING  "警告！処方せん料を算定しません。"
                           "一般名処方加算（処方せん料）のみの算定です"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H24.6
               WHEN    "K235"
                   STRING  "警告！"
                           "一般名処方加算（処方せん料）の対象薬剤が"
                           "ありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K236"
                   STRING  "警告！"
                           "一般名処方加算（処方せん料）の算定が可能"
                           "です。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K237"
                   STRING  "警告！"
                           "処方せん料が包括されます。"
                           "算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "K239"
                   STRING  "警告！"
                           "用法まとめでは"
                           "内服薬剤が７種類以上となります。"
                           "処方せん料を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K238"
                   STRING  "警告！"
                           "用法まとめでは"
                           "内服薬剤が７種類未満です。"
                           "処方せん料を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K160"
                   STRING  "警告！"
                           "時間外です。"
                           "時間外緊急院内検査加算の算定を"
                           "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K161"
                   STRING  "警告！"
                           "時間外です。"
                           "時間外緊急院内画像診断加算の算定を"
                           "確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      *H26.4　改正から
               WHEN    "K106"
                   STRING  "警告！"
                           "地域包括診療料が算定済みです。"
                           "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K250"
                   STRING  "警告！"
                           "うがい薬のみの投薬となります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K541"
                   STRING  "警告！長時間麻酔管理加算の算定できる"
                           "手術の算定がありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *H26.10　改正から
               WHEN    "K252"
                   STRING  "警告！"
                           "向精神薬多剤投与ではありません。"
                           "処方せん料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K251"
                   STRING  "警告！"
                           "向精神薬多剤投与となります。"
                           "処方せん料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K265"
                   STRING  "警告！"
                           "向精神薬多剤投与となります。"
                           "精神科継続外来支援・指導料は算定できません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H27.4
               WHEN    "K253"
                   STRING  "警告！"
                           "３０日以上の投薬があります。"
                           "処方せんの３０日以上投薬逓減の算定"
                           "がありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K254"
                   STRING  "警告！"
                           "３０日以上の投薬がありません。"
                           "処方せんの３０日以上投薬逓減の算定"
                           "があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K256"
                   STRING  "警告！"
                           "低紹介率の設定ではありません。"
                           "処方せんの３０日以上投薬逓減の算定"
                           "があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.9
               WHEN    "K257"
                   STRING  "警告！"
                           "認知症地域包括診療の算定があります。"
                           "投薬種類数が算定上限を超えています。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *==
      *
               WHEN    "1316"
                   MOVE    "初診料への自動変更はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "1317"
                   STRING  "再診料への自動変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1318"
                   STRING  "小児科外来診療料の入力がありません。"
                           "自動変更はできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      *H28.4　改正から
               WHEN    "K107"
                   STRING  "警告！"
                           "認知症地域包括診療料が算定済みです。"
                           "診察料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K108"
                   STRING  "警告！"
                           "湿布薬が７０枚を超えています。"
                           "減点対象となります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K109"
                   STRING  "警告！"
                           "湿布薬の投薬があります。"
                           "全量・１日量のコメントを入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K110"
                   STRING  "警告！"
                           "湿布薬が７０枚を超えています。"
                           "投与理由のコメントを入力して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0101"
                   STRING  "処方せん無交付加算の在宅料ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K222"
                   STRING  "警告！"
                           "一般名処方加算１（処方せん料）の対象では"
                           "ありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K223"
                   STRING  "警告！"
                           "一般名処方加算２（処方せん料）の対象では"
                           "ありません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0236"
                   STRING  "外用薬院内投薬がゼロ点になります。"
                           "診療種別を.231に変更して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8053"
                   STRING  "廃用症候群リハビリテーション開始日が"
                       "算定されていません。終了日の入力はできません"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8054"
                   STRING  "廃用症候群リハビリテーション終了日は"
                       "開始日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K855"
                   STRING  "警告！廃用症候群リハビリテーション"
                           "発症日から１２０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K856"
                   STRING  "警告！廃用症候群リハビリ"
                           "発症日から１２０日以上です。"
                           "月１３単位を超えました。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K857"
                   STRING  "警告！廃用症候群リハビリテーション"
                           "発症日から１８０日以上です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K858"
                   STRING  "警告！廃用症候群リハビリ"
                           "発症日から１８０日以上です。"
                           "月１３単位を超えました。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K859"
                   STRING  "警告！廃用症候群リハビリテーション"
                           "発症日の１２０日超から算定して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K860"
                   STRING  "警告！廃用症候群リハビリテーション"
                           "発症日の１８０日超から算定して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K542"
                   STRING  "警告！非侵襲的血行動態モニタリング加算の"
                           "算定できる手術の算定がありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K543"
                   STRING  "警告！非侵襲的血行動態モニタリング加算の"
                           "算定できる麻酔の算定がありません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0515"
                   STRING  "凍結保存同種組織加算の"
                           "算定できない手術です。"
                                           DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K876"
                   STRING  "警告！既に"
                           "児童思春期精神科専門管理加算（２０歳未満）"
                           "の算定があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.7 から
               WHEN    "K877"
                   STRING  "警告！"
                           "通院・在宅精神療法等の減算規定に"
                           "該当しません。"
                           "算定コードを確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K878"
                   STRING  "警告！"
                           "通院・在宅精神療法等の減算規定に"
                           "該当します。"
                           "算定コードを確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.7
               WHEN    "8055"
                   STRING  "労災加算は１．５倍のみです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8056"
                   STRING  "労災加算は２倍のみです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K805"
                   STRING  "警告！"
                           "労災加算は１．５倍のみです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K806"
                   STRING  "警告！"
                           "労災加算は２倍のみです。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K407"
                   STRING  "警告！"
                           "労災加算は算定できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
      *H28.9
               WHEN    "K511"
                   STRING  "警告！"
                           "多椎間又は多椎弓実施加算の"
                           "下に手術の入力があります。"
                           "数量まとめは行いません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0227"
                   STRING  "持参薬予約コードは"
                           "投薬剤の最初に入力して下さい。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0228"
                   STRING  "持参薬予約コードは"
                           "「包括分入力」での入力はできません。"
                                       DELIMITED  BY  SPACE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H28.9　改正から
               WHEN    "K810"
                   STRING  "警告！"
                           "目標設定等支援・管理料（初回）の算定が"
                           "ありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K816"
                   STRING  "警告！"
                           "前回が目標設定等支援・管理料（初回）の算定"
                           "です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K812"
                   STRING  "警告！"
                           "前回の目標設定等支援・管理料算定日が"
                           "３月以内です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K813"
                   STRING  "警告！"
                           "目標設定等支援・管理料の算定が３月以内に"
                           "ありません。減点対象です。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K814"
                   STRING  "警告！"
                           "目標設定等支援・管理料の算定があります。"
                           "減点対象ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K815"
                   STRING  "警告！"
                           "減点対象開始日付未満です。"
                           "減点対象ではありません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!!!
      *H31.3
               WHEN    "K818"
                   STRING  "警告！"
                           "維持期リハ経過措置終了による特別対応の"
                           "要件を満たしていますか？"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *R01.7
               WHEN    "K823"
                   STRING  "警告！"
                           "心大血管疾患リハビリ開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K824"
                   STRING  "警告！"
                           "脳血管疾患リハビリ開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K825"
                   STRING  "警告！"
                           "運動器リハビリ開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K826"
                   STRING  "警告！"
                           "呼吸器リハビリ開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K828"
                   STRING  "警告！"
                           "摂食機能療法開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K829"
                   STRING  "警告！"
                           "廃用症候群リハビリ開始日が複数あります。"
                           "個別終了は履歴登録で行います。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *R01.8
               WHEN    "8061"
                   STRING  "心大血管疾患リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8062"
                   STRING  "脳血管疾患リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8063"
                   STRING  "運動器リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8064"
                   STRING  "呼吸器リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8065"
                   STRING  "摂食機能療法開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8066"
                   STRING  "難病患者リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8067"
                   STRING  "障害児リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8068"
                   STRING  "がん患者リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8069"
                   STRING  "廃用症候群リハビリテーション開始日は"
                           "終了日と同日に入力できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
      *!!!
      *H29.1
               WHEN    "K190"
                   STRING  "警告！"
                           "翌月の入院料算定がありません。"
                           "翌月の入院調剤料は出来高で作成します。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      **
      *!!!
      *H29.3
               WHEN    "K258"
                   STRING  "警告！"
                           "向精神薬多剤投与となります。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K259"
                   STRING  "警告！"
                           "内服薬剤が７種類以上です。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K260"
                   STRING  "警告！"
                           "向精神薬多剤投与ではありません。"
                           "逓減ができません。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K261"
                   STRING  "警告！"
                           "内服薬剤が７種類以上ではありません。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0235"
                   STRING  "院内投薬がありません。"
                           "処方料の算定はできません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K266"
                   STRING  "警告！"
                           "地域包括診療の算定があります。"
                           "逓減対象外です。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0238"
                   STRING  "処方料（麻・向・覚・毒）の算定は"
                           "できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0239"
                   STRING  "処方料が算定できません。"
                           "外来後発医薬品使用体制加算は"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K262"
                   STRING  "警告！"
                           "処方料が算定できません。"
                           "外来後発医薬品使用体制加算は"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0243"
                   STRING  "処方料減算の対象ではありません。"
                           "算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0244"
                   STRING  "湿布薬が７０枚を超えています。"
                           "処方料は算定できません。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H29.4
               WHEN    "K268"
                   STRING  "確認！"
                           "長期投薬のため理由の記載が"
                           "必要となる場合があります。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.4 改正
               WHEN    "0245"
                   STRING  "処方せん料が算定されません。"
                           "向精神薬調整連携加算（処方箋料）は"
                           "算定不可です"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K245"
                   STRING  "警告！処方せん料を算定しません。"
                           "向精神薬調整連携加算（処方箋料）"
                           "のみの算定"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0246"
                   STRING  "処方料が算定されません。"
                           "向精神薬調整連携加算（処方料）は"
                           "算定不可です"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K246"
                   STRING  "警告！処方料を算定しません。"
                           "向精神薬調整連携加算（処方料）"
                           "のみの算定です"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H31.4
               WHEN    "K277"
                   STRING  "警告！"
                           "向精神薬長期処方減算対象です。"
                           "処方料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K278"
                   STRING  "警告！"
                           "向精神薬長期処方減算対象です。"
                           "処方箋料を確認して下さい。"
                                               DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H29.8
               WHEN    "1310"
                   STRING  "保険組合せの選択件数が上限を超えています。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1311"
                   STRING  "診療科の選択件数が上限を超えています。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1312"
                   STRING  "保険組合せ・診療科の組合せ件数が"
                           "上限件数を超えています。"
                           "選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H29.9
               WHEN    "K300"
                   STRING  "警告！当月に受診があります。"
                           "登録済みの診療内容の"
                           "包括確認をして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      **
               WHEN    "K910"
                   STRING  "警告！"
                           "入院会計照会を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1999"
                   STRING  "中途終了データの更新に失敗しました"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2001-K02-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録エラーメッセージ編集処理
      *****************************************************************
       2002-K05-ERRMSG-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "処理エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "数量・金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5001"
                   MOVE    "セットコード入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5100"
                   STRING  "対象のセットが存在しません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "5002"
                   MOVE    "すでに入力済コードです"
                                       TO  SPA-ERRMSG
               WHEN    "5003"
                   MOVE    "セットコードが存在しません"
                                       TO  SPA-ERRMSG
               WHEN    "5004"
                   MOVE    "セットコードとセット名は必須入力です"
                                       TO  SPA-ERRMSG
               WHEN    "5005"
                   MOVE    "追加エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5006"
                   MOVE    "更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5007"
                   MOVE    "削除できません"
                                       TO  SPA-ERRMSG
               WHEN    "5008"
                   MOVE    "保険外は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5009"
                   MOVE    "約束セットの時、セットは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "5010"
                   MOVE    "約束セットの時、１剤のみ入力です"
                                       TO  SPA-ERRMSG
               WHEN    "5011"
               MOVE    "約束セットの時、診療種別の入力はできません。"
                                       TO  SPA-ERRMSG
               WHEN    "5012"
                   MOVE    "修正時のみ複写ができます。"
                                       TO  SPA-ERRMSG
               WHEN    "5020"
                   MOVE    "入外区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "5021"
                   MOVE    "老人一般区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "5022"
                   MOVE    "有効開始日暦日エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5023"
                   MOVE    "有効終了日暦日エラー"
                                       TO  SPA-ERRMSG
               WHEN    "5024"
                   MOVE    "有効開始日＜有効終了日で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "5025"
                   MOVE    "選択番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "5026"
                   MOVE    "期間設定は約束セットの時のみです。"
                                       TO  SPA-ERRMSG
               WHEN    "5027"
                   MOVE    "期間が重複しています。"
                                       TO  SPA-ERRMSG
               WHEN    "5028"
                   STRING  "約束セットに器材商品コードの算定は"
                           "できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "5029"
                   STRING  "約束セットに減点分コードの入力は"
                           "できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "5030"
                   STRING  "約束セットに薬評・器評コードの入力は"
                           "できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "5031"
                   STRING  "履歴は１０件までの登録となります。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K050"
                   STRING  "警告！！"
                           "有効終了日が設定されています。"
                           "終了日で期間変更をして下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2002-K05-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会エラーメッセージ編集処理
      *****************************************************************
       2003-J04-ERRMSG-SEC              SECTION.
      *
      *
           EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "処理エラーです"
                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "数量・金額は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1020"
                   MOVE    "診区は変更できません"
                                       TO  SPA-ERRMSG
               WHEN    "1021"
                   MOVE    "回数は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1022"
                   MOVE    "セットは入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1023"
                   MOVE    "剤は１剤のみです"
                                       TO  SPA-ERRMSG
               WHEN    "1024"
                   MOVE    "診療行為マスタが更新できません"
                                       TO  SPA-ERRMSG
               WHEN    "1025"
                   STRING  "既に同じ診療行為の剤が存在します。"
                           "回数の変更で修正して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1026"
                   MOVE    "診療会計マスタが追加できません"
                                       TO  SPA-ERRMSG
               WHEN    "1027"
                   MOVE    "診療会計マスタ読込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1028"
                   MOVE    "剤番号取り込みエラー"
                                       TO  SPA-ERRMSG
               WHEN    "1029"
                   MOVE    "コメントマスター更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1032"
                   MOVE    "診療会計附加更新エラー"
                                       TO  SPA-ERRMSG
               WHEN    "1030"
                   MOVE    "剤削除は入力できません"
                                       TO  SPA-ERRMSG
               WHEN    "1031"
                   MOVE    "明細がありません。変更できません。"
                                       TO  SPA-ERRMSG
      *        剤追加・保険変更
               WHEN    "1033"
                   MOVE    "保険組合せ選択エラー。"
                                       TO  SPA-ERRMSG
               WHEN    "1036"
                   STRING  "保険組合せの有効期間外に回数設定が"
                           "あります。選択できません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1034"
                   STRING  "包括検査が包括数以下です。"
                           "１検査のみ入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1035"
                   STRING  "検査が違います。剤分離になりますので"
                           "１検査のみ入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1036"
                   STRING  "包括診療区分がありません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1099"
                   MOVE    "ＤＢ更新エラー。"
                                       TO  SPA-ERRMSG
      *
               WHEN    "K001"
                   MOVE    "警告！！時間外算定がありません"
                                       TO  SPA-ERRMSG
               WHEN    "K102"
                   STRING  "警告！！内服逓減対象の剤です。更新後、"
                           "必ず訂正で逓減点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K103"
                   STRING  "警告！！登録時と診療内容が違います。"
                           "必ず登録をして点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K104"
                   STRING  "警告！"
                           "登録時と点数が違います。"
                           "湿布薬減点対象であれば、"
                           "訂正で点数を変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       2003-J04-ERRMSG-EXT.
           EXIT.
      *
      *
