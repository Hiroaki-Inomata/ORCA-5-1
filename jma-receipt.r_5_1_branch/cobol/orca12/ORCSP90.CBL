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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSP90.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者登録エラーメッセージ編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/04/05    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/12/XX  指導料等自動算定対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    添字領域
      *01  IDX-AREA.
      *    03  IDX                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-IDOYMD-H            PIC X(22).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-P02KYOUTU
           SPA-P02SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    エラーメッセージ編集処理
           PERFORM 100-ERRMSG-HEN-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       100-ERRMSG-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "K101"
                   STRING  "警告！カナ氏名にカナ以外の文字があります。"
                           "確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0002"
                   MOVE    "日付変換エラー"    TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "日付重複エラー"    TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "未登録予約者は存在していません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象住所が見つかりません"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "削除対象の明細が変更されています"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "必須入力エラー"    TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "これ以上登録できません"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "未入力項目があります。登録できません" 
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "作成可能な組合せ数を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "組合せ番号の上限を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "ファイル読込エラー"    TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "他で使用しているので更新できません"
                                               TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "保険番号マスタ確認エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "組合せ作成中、テーブルオーバーしました"
                                               TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "生年月日が未入力です"
                                               TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "変更年月日が重複しています"  TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "正しく選択されていません"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "減免事由を正しく選択してください"
                                               TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD START --------------------------------
               WHEN    "0020"
                   MOVE    "生年月日に未来日が入力されました"
                                               TO  SPA-ERRMSG
      *----(01.00.01) LINE ADD END   --------------------------------
               WHEN    "0021"
                   MOVE    "性別は１：男２：女で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "労災・自賠責保険ではありません。"
                                               TO  SPA-ERRMSG
                   MOVE    "労災・自賠責を表示後、選択して下さい。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0023"
                   MOVE    "労災・自賠責保険です。選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "0024"
                   MOVE    "労災・自賠責保険が保険番号マスタ"
                                               TO  SPA-ERRMSG
                   MOVE    "に登録されていません。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0025"
                   MOVE    "カナ氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "漢字氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "通称名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "生年月日の暦日エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "カルテ発行区分が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0901"
                   MOVE    "ユーザプログラム起動表示行区分が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "すべて全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "警告！郵便番号は存在しません。"
                                               TO  SPA-ERRMSG
      *ver.4.7
               WHEN    "0131"
                   MOVE    "郵便番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0132"
                   MOVE    "電話番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
      *ver.4.7
               WHEN    "0032"
                   MOVE    "住所は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0033"
                   MOVE    "世帯主は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0034"
                   MOVE    "続柄は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0035"
                   MOVE    "割引率入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0166"
                   MOVE    "入金方法入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0167"
                   MOVE    "請求書発行区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0036"
                   MOVE    "状態入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "保険組合せがありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0137"
                   STRING  "非表示の保険組合せです。"
                           "選択できません。"  DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0038"
                   MOVE    "診療科がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0039"
                   MOVE
                    "カルテを発行する時は、診療科を選択して下さい。"
                                               TO  SPA-ERRMSG
      *    保険エラー
               WHEN    "0040"
                   MOVE    "保険者番号検証チェックエラー"
                                               TO  SPA-ERRMSG
               WHEN    "0041"
                   MOVE    "保険者番号桁数エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0042"
                   MOVE    "保険の種別は変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "新しく保険を追加して下さい"
                                               TO  SPA-ERRMSG(29:)
               WHEN    "0043"
               MOVE    "労災・自賠責保険はこの画面では変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "労災・自賠ボタンを選択して下さい"
                                               TO  SPA-ERRMSG(47:)
               WHEN    "0044"
                   MOVE    "保険の種別がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0045"
                   MOVE    "本人家族区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0046"
                   MOVE    "補助区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0047"
                   MOVE    "継続区分がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0048"
                   MOVE    "保険の記号は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "K048"
                   STRING  "警告！！"
                           "記号に全角空白があります。"
                           "レセ電でエラーになります。"
                                               DELIMITED  BY  SIZE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "K050"
                   STRING  "警告！！"
                           "後期高齢者は記号に入力しないで下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0049"
                   MOVE    "保険の番号は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "K049"
                   STRING  "警告！！"
                           "番号に全角空白があります。"
                           "レセ電でエラーになります。"
                                               DELIMITED  BY  SIZE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0050"
                   MOVE    "資格取得日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "保険有効開始日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "保険有効終了日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0052"
                   MOVE    "継続の時保険有効終了日を入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE    "継続時の適用終了日以降の終了日です"
                                               TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE    "被保険者名は全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE    "保険有効開始日は必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0056"
                   MOVE    "保険の確認年月日が入力エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0057"
                   STRING  "警告！"
                           "保険の確認年月日に未来日が入力されました"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0058"
                   MOVE
                   "保険有効開始日＜有効終了日で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0059"
                   MOVE    "保険の種別を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0060"
                   STRING   "国保・自費・治験の場合、"
                            "補助区分は必須入力です"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0061"
                   MOVE     "補助区分は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0062"
                   MOVE    "保険有効開始日は必須入力です。"
                                               TO  SPA-ERRMSG
               WHEN    "0063"
                   MOVE    "適用期間がマスタの範囲を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0064"
                   MOVE    "保険期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0065"
                   MOVE    "保険番号マスタが存在していません。"
                                               TO  SPA-ERRMSG
               WHEN    "0066"
                   MOVE    "本人のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "1065"
                   MOVE    "補助区分は本人の時のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "1066"
                   MOVE    "補助区分は老人割合のみ入力可能です。"
                                               TO  SPA-ERRMSG
               WHEN    "1067"
                   STRING  "補助区分２割は前期高齢者の補助区分では"
                           "ありません。変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *H26.4
               WHEN    "1068"
                   STRING  "補助区分１割は前期高齢者の補助区分では"
                           "ありません。変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0067"
                   MOVE    "保険者番号と法別番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0068"
                   MOVE    "老人補助区分は変更できません。"
                                               TO  SPA-ERRMSG
                   MOVE    "新しい保険を入力して下さい。"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "0069"
                   MOVE    "５年（２年）後日付取得エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1047"
                   MOVE    "下船３月の時継続区分は選択できません。"
                                               TO  SPA-ERRMSG
               WHEN    "1048"
                   MOVE    "特別は日雇特例のみ選択できます。"
                                               TO  SPA-ERRMSG
               WHEN    "1049"
                   MOVE    "特別はＨ１５年４月からの選択です。"
                                               TO  SPA-ERRMSG
               WHEN    "1050"
                   MOVE    "継続はＨ１５年３月３１日までの有効です。"
                                               TO  SPA-ERRMSG
               WHEN    "1051"
                   STRING  "６５歳以上です。"
                           "退職国保は有効期間外となります。"
                           "期間変更して下さい。"
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K151"
                   STRING  "警告！！"
                           "前期高齢者となりました。"
                           "保険を確認して下さい。"
                                               INTO    SPA-ERRMSG
                   END-STRING
      *R02.4
               WHEN    "1040"
                   STRING  "枝番は数値で入力して下さい。"
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1041"
                   STRING  "枝番を設定できない保険です。"
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1042"
                   STRING  "記号番号の入力がありません。"
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
      *    公費エラー
               WHEN    "0070"
                   MOVE    "負担者番号・受給者番号が入力されています"
                                               TO  SPA-ERRMSG
               WHEN    "0071"
                   MOVE    "負担者番号が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0072"
                   MOVE    "公費の種類がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0073"
                   MOVE    "公費の種類は必須入力です。入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0074"
                   MOVE    "公費の種別は変更できません"
                                               TO  SPA-ERRMSG
      *!
      *        WHEN    "2074"
      *            STRING  "961 減額（円）は現在 使用できません。"
      *                    "削除して下さい。"
      *                                        DELIMITED  BY  SIZE
      *                                        INTO  SPA-ERRMSG
      *            END-STRING
      *!H25.12 から
               WHEN    "2074"
                   STRING  "961 減額（円）は平成２４年４月以降を"
                           "開始日として入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *!
               WHEN    "0075"
                   MOVE    "受給者番号は全角・半角の混在はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0076"
                   MOVE    "受給者番号が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0077"
                   MOVE    "公費の適用開始日が入力エラーです"
                                               TO  SPA-ERRMSG
      *Ｈ２１．１から
               WHEN    "0177"
                   MOVE    "公費開始日に１日は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0078"
                   MOVE    "公費の適用終了日が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0079"
                   MOVE    "公費の確認年月日が入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0080"
                   STRING  "警告！"
                           "公費の確認年月日に未来日が入力されました"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0081"
                   MOVE
                   "保険有効開始日＜有効終了日で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0082"
                   MOVE    "負担者番号・受給者番号は入力できません"
                                               TO  SPA-ERRMSG
               WHEN    "0083"
                   MOVE    "負担者番号検証番号エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "K083"
                   MOVE    "警告！特定疾患の負担者番号検証番号エラー"
                                               TO  SPA-ERRMSG
                   MOVE    "です。"
                                               TO  SPA-ERRMSG(41:)
               WHEN    "0084"
                   MOVE    "受給者番号検証番号エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "K084"
                   MOVE    "警告！受給者番号検証番号がエラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "0085"
                   STRING  "公費に適用年齢の指定があります。"
                           "適用終了日を入力してください。"
                                       DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0086"
                   MOVE    "公費の年齢適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "K005"
                   MOVE    "警告！公費の年齢開始日適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "K006"
                   MOVE    "警告！公費の年齢終了日適用エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "0087"
                   MOVE    "適用開始日時点での年齢が６５歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "0088"
                   MOVE    "公費期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "0089"
                   MOVE    "国保・生活保護の期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "K001"
                   MOVE
                   "警告！適用開始日時点での年齢が７０歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "K002"
                   MOVE
                   "警告！適用開始日時点での年齢が７５歳未満です"
                                               TO  SPA-ERRMSG
               WHEN    "K003"
      *            保険者マスタ期限切れ
                   MOVE    SPA-HKNJA-IDOYMD    TO  WRK-SYMD
                   PERFORM 5101-IDOYMD-ERR-SEC
               WHEN    "K004"
      *            保険者マスタ期限切れ（公費）
                   PERFORM 5102-IDOYMD-KOHERR-SEC
               WHEN    "0090"
                   MOVE    "公費の空いている行に追加して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0091"
                   MOVE    "適用期間がマスタの範囲を超えています"
                                               TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "負担者番号との法別番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0093"
                   MOVE    "適用開始日が保険適用の終了日以降です。"
                                               TO  SPA-ERRMSG
               WHEN    "0094"
                   MOVE    "適用開始日が保険適用の開始日以前です。"
                                               TO  SPA-ERRMSG
               WHEN    "0095"
                   MOVE    "老人公費の場合、全角での入力はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0097"
                   MOVE    "公費負担額対象が６０件を超えました。"
                                               TO  SPA-ERRMSG
      *Ｈ１８．１０から
               WHEN    "0098"
                   STRING  "老人公費２割はＨ１８年１０月から廃止"
                           "されています。３割に変更して下さい。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0112"
                   STRING  "老人保健は徴収猶予及び減免の対象ではありま"
                           "せん。確認して下さい。"
                                       DELIMITED  BY  SIZE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0099"
                   MOVE    "日付変換エラーです。"
                                               TO  SPA-ERRMSG
               WHEN    "K010"
                   MOVE
                   "警告！地方公費単独の保険組合せがあります。"
                                               TO  SPA-ERRMSG
               WHEN    "K011"
                   STRING
                   "警告！退職国保と老人公費の保険組合せがあります。"
                                               DELIMITED   BY  SPACE
                   "保険を確認して下さい。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "K120"
                   MOVE    "警告！労災・自賠責の患者情報が未登録です。"
                                               TO  SPA-ERRMSG
                   MOVE    "登録して下さい。"
                                               TO  SPA-ERRMSG(37:)
      *Ｈ２０．４から
               WHEN    "0120"
                   STRING  "後期高齢者に到達しました。"
                           "後期高齢者の保険を登録して下さい。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0121"
                   STRING  "後期高齢者です。"
                           "後期高齢者医療を登録するか"
                           "前の主保険の有効期間を設定して下さい"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0128"
                   STRING  "後期高齢非該当者です。"
                           "保険の補助区分を前期高齢者に変更して下さい"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *H26,8
                WHEN    "0129"
                   STRING  "後期高齢非該当者です。"
                           "保険の補助区分の１割はＨ２６年５月から"
                           "２割に変更して下さい"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *
              WHEN    "K122"
                   STRING  "警告！！"
                           "適用開始日が７５歳以上です。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0122"
                   STRING  "適用開始日が７５歳以上です。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "K123"
                   STRING  "警告！！Ｈ２０年３月まで"
                           "老人公費が適用されていました。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0124"
                   MOVE    "後期高齢者・生活保護の期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0125"
                   STRING  "後期高齢者・残留邦人等の期間が"
                           "重複しています。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0126"
                   MOVE    "国保・残留邦人等の期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0127"
                   MOVE    "生活保護・残留邦人等の期間が重複しています"
                                               TO  SPA-ERRMSG
               WHEN    "0128"
                   STRING  "前期高齢者の補助区分を選択して下さい"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *VER.4.6.0　から
               WHEN    "K129"
                   STRING  "警告！"
                           "本人・家族区分変更です。"
                           "保険組合せを削除して、"
                           "新規番号で追加を行います。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "K130"
                   STRING  "警告！"
                           "補助区分変更です。"
                           "保険組合せを削除して、"
                           "新規番号で追加を行います。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "0135"
      *H30.4 ****  STRING  "０７入所中・０８入所中の期間が"
      ***********          "重複しています"
                   STRING  "入所中公費の期間が重複しています"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *H24.11
               WHEN    "0136"
                   STRING  "長期・長期（上位）の期間が"
                           "重複しています"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *H25.11
               WHEN    "K136"
                   STRING  "警告！"
                           "生活保護の期間が重複しています。"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *H25.XX
               WHEN    "0143"
                   STRING  "第三者行為はこの画面では変更できません。"
                           "労災・自賠ボタンを選択して下さい"
                                               DELIMITED   BY  SPACE
                                               INTO   SPA-ERRMSG
                   END-STRING
      *    更新前エラー
               WHEN    "0100"
                   MOVE    "カナ氏名は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "漢字氏名は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0102"
                   MOVE    "生年月日は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "性別は必ず入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0104"
                   MOVE    "保険の老人割合が入力されていません。"
                                               TO  SPA-ERRMSG
                   MOVE    "保険を再作成して下さい。"
                                               TO  SPA-ERRMSG(37:)
               WHEN    "0105"
                   MOVE    "老人割合が入力されていません。"
                                               TO  SPA-ERRMSG
                   MOVE    "公費を作成して下さい。"
                                               TO  SPA-ERRMSG(31:)
               WHEN    "0106"
                   STRING   "公費の登録は６０件までです。"
                            "これ以上追加できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0107"
                   STRING   "保険の登録は４０件までです。"
                            "これ以上追加できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *    キーエラー
               WHEN    "0110"
                   MOVE    "この画面では選択できません。"
                                               TO  SPA-ERRMSG
      *ver.4.7.0
               WHEN    "0111"
                   MOVE    "保険の前頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0113"
                   MOVE    "公費の前頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0114"
                   MOVE    "保険の次頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0115"
                   MOVE    "公費の次頁はありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0116"
                   MOVE    "確認年月日の履歴はありません"
                                               TO  SPA-ERRMSG
      *    保険組合せ
               WHEN    "2701"
                   MOVE    "保険組合せがありません。"
                                               TO  SPA-ERRMSG
      *R01.6
               WHEN    "K271"
                   STRING   "警告！"
                            "この設定は主保険のない難病等＋生保の"
                            "組合せを作成する設定です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K272"
                   STRING   "警告！"
                            "この設定を解除すると主保険のない"
                            "難病等＋生保の組合せがすべて"
                            "削除されます。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *    連絡先
               WHEN    "2101"
      *****        MOVE    "連絡先名称は全角で入力して下さい"
                   MOVE    "連絡先名称入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2102"
      ****         MOVE    "続柄は全角で入力して下さい"
                   MOVE    "続柄入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2103"
                   MOVE    "連絡先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2104"
      ****         MOVE    "連絡先住所は全角で入力して下さい"
                   MOVE    "連絡先住所入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2105"
      ****         MOVE    "勤務先名称は全角で入力して下さい"
                   MOVE    "勤務先名称入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2106"
                   MOVE    "勤務先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2107"
      *****        MOVE    "勤務先住所は全角で入力して下さい"
                   MOVE    "勤務先住所入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2108"
      ****         MOVE    "帰省先名称は全角で入力して下さい"
                   MOVE    "帰省先名称入力エラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2109"
                   MOVE    "帰省先郵便番号はありません"
                                               TO  SPA-ERRMSG
               WHEN    "2110"
      ****         MOVE    "帰省先住所は全角で入力して下さい"
                   MOVE    "帰省先住所入力エラーです"
                                               TO  SPA-ERRMSG
      *ver.4.7
               WHEN    "2111"
                   MOVE    "連絡先郵便番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2112"
                   MOVE    "勤務先郵便番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2113"
                   MOVE    "帰省先郵便番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2114"
                   MOVE    "連絡先電話番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2115"
                   MOVE    "勤務先電話番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2116"
                   MOVE    "帰省先電話番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2117"
                   MOVE    "ＦＡＸは半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2118"
                   MOVE    "携帯番号は半角で入力して下さい"
                                               TO  SPA-ERRMSG
      *ver.4.7
      *    受付エラー
               WHEN    "2001"
                   MOVE    "受付選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "保険組合せ番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2003"
                   MOVE    "カルテ選択は１です。再入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2005"
                   MOVE    "処方箋選択は１です。再入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2004"
                   MOVE    "未登録受付者は存在していません"
                                               TO  SPA-ERRMSG
               WHEN    "2008"
                   MOVE    "１０件以上登録できません"
                                               TO  SPA-ERRMSG
               WHEN    "2009"
                   MOVE    "登録できません"    TO  SPA-ERRMSG
               WHEN    "2010"
                   MOVE    "診療科を入力して下さい。" 
                                               TO  SPA-ERRMSG
               WHEN    "2011"
                   MOVE
           "カルテ、処方せんを入力した時は、保険組合せを入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2012"
                   MOVE    "ドウターコードがありません"
                                               TO  SPA-ERRMSG
               WHEN    "2013"
                   MOVE    "診療内容コードがありません"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "患者を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "自由構成です。患者番号を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "患者マスターが存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "新患ではありません。再発行はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "患者番号の桁数が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "患者番号の構成が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "患者番号の検証番号が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "患者番号の連番は管理連番以下にして下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1009"
                   MOVE    "新規の患者番号を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE
                   "既に存在する患者番号です。"
                                               TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "患者番号が登録済みです。再度入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "他画面からの遷移中です。"
                                               TO  SPA-ERRMSG
                   MOVE    "患者番号の再発行はできません。"
                                               TO  SPA-ERRMSG(25:)
      *H30.9
               WHEN    "1013"
                   MOVE    "患者番号に使用できない文字があります。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "1020"
                   MOVE    "患者ＩＤが取得できません。"
                                               TO  SPA-ERRMSG
               WHEN    "1021"
                   MOVE    "患者番号を入力して下さい。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "1025"
                   STRING  "登録済みの患者ではありません。"
                           "患者登録後、禁忌薬剤を登録して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1023"
                   MOVE    "ＱＲデータはありません。"
                                               TO  SPA-ERRMSG
               WHEN    "1024"
                   STRING   "ＱＲデータが展開中です。"
                            "患者番号をクリアして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1022"
                   MOVE    "複写は新規患者の時のみできます。"
                                               TO  SPA-ERRMSG
               WHEN    "2501"
                   MOVE    "選択番号が入力されていません"
                                               TO  SPA-ERRMSG
      *        所得者情報
               WHEN    "2401"
                   MOVE    "低所得者２の履歴番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "2402"
                   MOVE    "低所得者２の認定開始日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2403"
                   MOVE    "低所得者２の認定終了日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2404"
                   MOVE
                   "低所得者２の認定開始日≦終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2405"
                   MOVE
                   "低所得者２の認定期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2406"
      ************ MOVE    "老齢福祉年金履歴番号がありません"
                   MOVE    "低所得者１の履歴番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "2407"
      ************ MOVE    "老齢福祉年金開始日がエラーです"
                   MOVE    "低所得者１の認定開始日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2408"
      *************MOVE    "老齢福祉年金終了日がエラーです"
                   MOVE    "低所得者１の認定終了日がエラーです"
                                               TO  SPA-ERRMSG
               WHEN    "2409"
                   MOVE
      *************"老齢福祉年金開始日＜終了日で入力して下さい"
                   "低所得者１の認定開始日≦終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2410"
                   MOVE
      ************ "老齢福祉年金期間が重複しています。"
                   "低所得者１の認定期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2411"
      ************ MOVE    "低所得者１の認定日を入力して下さい"
                   MOVE    "低所得者２の認定日を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2412"
      ************ MOVE    "老齢福祉年金履歴日を入力して下さい"
      ************ MOVE    "低所得者２の認定日を入力して下さい"
                   MOVE    "低所得者１の認定日を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2413"
                   MOVE    "長期入院該当年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2414"
                   MOVE    "長期入院該当年月日は低所得２の"
                                               TO  SPA-ERRMSG
                   MOVE    "範囲内で入力して下さい"
                                               TO  SPA-ERRMSG(31:)
               WHEN    "2415"
                   MOVE    "老齢福祉年金受給者証区分を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2416"
                   MOVE    "老齢福祉年金受給者証区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2417"
                   STRING  "低所得者２ 認定範囲入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2418"
                   STRING  "低所得者２ 標準負担額減額開始日エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2419"
                   STRING  "標準負担額減額開始日は低所得２の"
                           "範囲内で入力して下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2420"
                   STRING  "低所得者１ 認定範囲入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2421"
                   STRING  "低所得者１ 標準負担額減額開始日エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2422"
                   STRING  "標準負担額減額開始日は低所得１の"
                           "範囲内で入力して下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2423"
                   STRING  "標準負担額減額開始日≦長期入院該当年月日で"
                           "入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2424"
                   STRING  "低所得者２ 境界層該当入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2425"
                   STRING  "低所得者１ 境界層該当入力エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "2430"
                   MOVE    "公費負担選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2431"
                   MOVE    "公費負担選択番号２入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2432"
                   MOVE    "公費負担適用開始日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2433"
                   MOVE    "公費負担適用終了日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2434"
                   MOVE    "公費負担適用期間が重複しています。"
                                               TO  SPA-ERRMSG
               WHEN    "2435"
                   MOVE    "公費負担の外来上限額入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2436"
                   MOVE    "公費負担の入院上限額入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2437"
                   MOVE    "公費負担適用期間を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2438"
                   MOVE    "公費負担適用開始日＜終了日で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2439"
                   MOVE    "公費負担は１００％までです。"
                                               TO  SPA-ERRMSG
               WHEN    "2499"
                   MOVE    "公費負担額対象公費が６０件以上です。"
                                               TO  SPA-ERRMSG
      *        公費負担額複写
               WHEN    "2440"
                   MOVE    "次月複写対象外公費です。"
                                               TO  SPA-ERRMSG
               WHEN    "2441"
                   MOVE    "複写する上限額履歴がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "2442"
                   MOVE
                   "上限額最新履歴の適用終了日が入力されていません。"
                                               TO  SPA-ERRMSG
               WHEN    "2443"
                   MOVE    "公費適用終了日以降分は複写できません。"
                                               TO  SPA-ERRMSG
               WHEN    "2444"
                   STRING  "上限額履歴が３００件以上となります。"
                           "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "2445"
                   MOVE    "他一部負担入力対象外公費です。"
                                               TO  SPA-ERRMSG
               WHEN    "2446"
                   MOVE    "上限額履歴を選択して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "2447"
                   STRING  "他一部負担額が入力されています。"
                           "開始日変更前に他一部負担を削除して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K240"
                   STRING  "警告！！"
                           "公費負担額の設定がない公費があります。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *        入力履歴
               WHEN    "2502"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "2503"
                   MOVE    "変更年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2504"
                   MOVE    "旧姓履歴を選択して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2506"
                   MOVE    "削除対象の明細が変更されています"
                                               TO  SPA-ERRMSG
               WHEN    "2507"
                   MOVE    "氏名を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2508"
                   MOVE    "受診科履歴選択番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2509"
                   MOVE    "登録できません"    TO  SPA-ERRMSG
               WHEN    "2510"
                   STRING  "旧姓履歴が２０件以上です。"
                           "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2530"
                   MOVE    "全角で入力して下さい"    TO  SPA-ERRMSG
               WHEN    "2517"
                   MOVE    "変更年月日が重複しています"  TO  SPA-ERRMSG
      *
      *        特記事項
               WHEN    "2601"
                   MOVE    "選択番号エラー"    TO  SPA-ERRMSG
               WHEN    "2602"
                   MOVE    "入外区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "2603"
                   MOVE    "レセ電は数値で入力して下さい"
                                                   TO  SPA-ERRMSG
               WHEN    "2604"
                   MOVE    "紙レセプト入力エラー" 
                                                   TO  SPA-ERRMSG
               WHEN    "2605"
                   MOVE    "開始日入力エラー" 
                                                   TO  SPA-ERRMSG
               WHEN    "2606"
                   MOVE    "終了日入力エラー" 
                                                   TO  SPA-ERRMSG
               WHEN    "2607"
                   MOVE    "特記内容選択エラー" 
                                                   TO  SPA-ERRMSG
               WHEN    "2608"
                   MOVE    "紙レセプトを入力して下さい" 
                                                   TO  SPA-ERRMSG
               WHEN    "2609"
                   MOVE    "有効期間を入力して下さい" 
                                                   TO  SPA-ERRMSG
               WHEN    "2610"
                   MOVE    "開始年月≦終了年月で入力して下さい" 
                                                   TO  SPA-ERRMSG
               WHEN    "2611"
                   MOVE    "レセ電コードで重複があります。" 
                                                   TO  SPA-ERRMSG
               WHEN    "2612"
                   STRING  "この特記事項の開始は"
                           "平成２１年５月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2613"
                   STRING  "この特記事項の開始は"
                           "平成２１年１０月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2614"
                   STRING  "この特記事項の開始は"
                           "平成２７年１月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2615"
                   STRING  "この特記事項の開始は"
                           "平成２６年１１月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2616"
                   STRING  "この特記事項の開始は"
                           "平成２８年４月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2617"
                   STRING  "この特記事項の開始は"
                           "平成３０年４月からです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2620"
                   STRING  "特記事項は５０件までの登録です。"
                           "これ以上追加できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *        レセ分割
               WHEN    "2630"
                   STRING  "レセプト分割の選択番号エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2631"
                   STRING  "対象保険組合せが変更となります。"
                           "確認ボタン押下で最新の保険組合せに"
                           "更新して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2639"
                   STRING  "レセプト分割の保険組合せが変更となります。"
                           "最新の保険組合せに変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2632"
                   STRING  "対象年月エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2633"
                   STRING  "振分日エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2634"
                   STRING  "入外区分エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2635"
                   STRING  "レセ区分エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2636"
                   STRING  "レセプト分割が重複します。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2637"
                   STRING  "振分日で期間変更の保険がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2638"
                   STRING  "対象の保険組合せがありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2640"
                   STRING  "振分日で期間変更の保険が"
                           "同一保険者でありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2641"
                   STRING  "振分日で振り分け出来ない保険組合せが"
                           "あります。"
                           "レセプト分割はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2642"
                   STRING  "削除番号選択エラー"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2643"
                   STRING  "レセプト分割は５０件登録済みです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2644"
                   STRING  "同月のレセプト分割は１０件までです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
      *        個別設定
               WHEN    "2801"
                   MOVE    "地域連携ＩＤは半角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "2802"
                   MOVE    "情報提供区分入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "2803"
                   MOVE    "お薬手帳ＱＲコード入力エラー"
                                                   TO  SPA-ERRMSG
               WHEN    "2804"
                   MOVE    "お薬手帳データフォーマット入力エラー"
                                                   TO  SPA-ERRMSG
               WHEN    "2805"
                   MOVE    "予備１データフォーマット入力エラー"
                                                   TO  SPA-ERRMSG
      *H26.2
      *        介護認定
               WHEN    "2810"
                   MOVE    "介護保険者選択番号がありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "2811"
                   MOVE    "介護保険者番号は半角で入力して下さい"
                                                   TO  SPA-ERRMSG
               WHEN    "2812"
                   MOVE    "介護被保険者番号は半角で入力して下さい"
                                                   TO  SPA-ERRMSG
               WHEN    "2813"
                   STRING  "介護保険者番号か介護被保険者番号を"
                           "入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2814"
                   STRING  "介護保険者情報は１０件登録済みです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2815"
                   MOVE    "有効開始日暦日エラーです"
                                                   TO  SPA-ERRMSG
               WHEN    "2816"
                   MOVE    "有効終了日暦日エラーです"
                                                   TO  SPA-ERRMSG
               WHEN    "2817"
                   MOVE    "有効開始日＜有効終了日で入力して下さい"
                                                   TO  SPA-ERRMSG
               WHEN    "2818"
                   MOVE    "有効開始日が未入力です"
                                                   TO  SPA-ERRMSG
               WHEN    "2819"
                   MOVE    "有効終了日が未入力です"
                                                   TO  SPA-ERRMSG
               WHEN    "2820"
                   MOVE    "有効期間が重複しています。"
                                                   TO  SPA-ERRMSG
               WHEN    "2825"
                   MOVE    "介護認定選択番号がありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "2821"
                   MOVE    "要介護状態区分がありません。"
                                                   TO  SPA-ERRMSG
               WHEN    "2822"
                   MOVE    "認定日暦日エラーです"
                                                   TO  SPA-ERRMSG
               WHEN    "2823"
                   STRING  "介護認定情報は５０件登録済みです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2824"
                   STRING  "選択番号を入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *H28.12
               WHEN    "2840"
                   STRING  "選択番号がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2841"
                   STRING  "ＩＤ識別は半角入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2842"
                   STRING  "個人番号エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2843"
                   STRING  "備考エラーです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2844"
                   STRING  "ＩＤ識別を入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2845"
                   STRING  "個人番号を入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2846"
                   STRING  "ＩＤ識別が重複しています。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2847"
                   STRING  "個人番号情報は２０件登録済みです。"
                           "これ以上登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "2850"
                   STRING  "点数マスタに存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2851"
                   STRING  "設定できないコードです。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "2852"
                   STRING  "システム日付で有効期間外です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K285"
                   STRING  "警告！"
                           "システム日付で有効期間外です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *H30.4
               WHEN    "2853"
                   STRING  "性別が男性です。"
                           "妊婦の設定は解除しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K287"
                   STRING  "警告！"
                           "１６歳未満です。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
      *        患者番号取得エラー
               WHEN    "8000"
                   STRING  "自動採番の患者番号が最大値です。"
                           "初期番号から採番しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8001"
                   STRING  "患者ＩＤが最大値です。"
                           "初期番号から採番しました。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8002"
                   STRING  "自動採番の患者番号が最大値になりました。"
                           "連番号桁数を変更して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "8003"
                   MOVE    "患者ＩＤが登録済です。登録できません。"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9000"
                   MOVE    "ＤＢ更新エラー"
                                               TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "受付ＩＤが９９９９９を超えます。"
                                                TO  SPA-ERRMSG
                   MOVE    "これ以上受付はできません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "9002"
                   MOVE    "受付更新エラー"
                                                TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "保険組合更新エラー"
                                                TO  SPA-ERRMSG
      *
               WHEN    "9103"
                   STRING  "保険組合せで診療行為が登録されています。"
                           "非表示にできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9104"
                   STRING  "保険組合せで病名が登録されています。"
                           "非表示にできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9105"
                   STRING  "保険組合せで診療・病名が登録されています。"
                           "非表示にできません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.6
               WHEN    "9107"
                   STRING  "非表示区分がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *!!
               WHEN    "9106"
                   STRING  "保険組合せ番号と最大連番のＤＢで不整合が"
                           "発生しています。"
                           "必ず登録をして下さい"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *H26.4
               WHEN    "9108"
                   STRING  "労災保険情報に不整合なデータが存在します。"
                           "登録で削除します。"
                           "登録押下して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9109"
                   STRING  "第三者行為の公費更新対象がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      * 
               WHEN    "9100"
                   STRING  "保険組合の変更で保険期間外の診療が"
                           "存在します。診療を保険変更して下さい。"
                                       DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9101"
                   STRING  "保険組合の変更で保険期間外の病名が"
                           "存在します。病名を保険変更して下さい。"
                                       DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "9102"
                   STRING  "保険組合の変更で保険期間外の診療と病名が"
                           "あります。"
                           "診療と病名を保険変更して下さい。"
                                       DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           .
       100-HAKKOU-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタ期限切れメッセージ表示理
      *****************************************************************
       5101-IDOYMD-ERR-SEC              SECTION.
      *
      *    日本語変換
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    SPACE               TO  WRK-IDOYMD-H
               STRING      LNK-DAY2-EDTYMD2-G  DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-Y  DELIMITED  BY  SIZE
                           "年"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-M  DELIMITED  BY  SIZE
                           "月"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-D  DELIMITED  BY  SIZE
                           "日"                DELIMITED  BY  SIZE
                                       INTO    WRK-IDOYMD-H
               END-STRING
           ELSE
               MOVE    SPACE               TO  WRK-IDOYMD-H
           END-IF
           MOVE    SPACE               TO  SPA-ERRMSG
           STRING  "この保険者は、"        DELIMITED  BY  SIZE
                   WRK-IDOYMD-H            DELIMITED  BY  SIZE
                   "に廃止となりました。"  DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
           END-STRING
           .
       5101-IDOYMD-ERR-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタ期限切れ（公費）メッセージ表示理
      *****************************************************************
       5102-IDOYMD-KOHERR-SEC              SECTION.
      *
           EVALUATE   SPA-GMN-CUR
               WHEN   20
                   MOVE    SPA-PTKOH-IDOYMD (1)    TO  WRK-SYMD
               WHEN   26
                   MOVE    SPA-PTKOH-IDOYMD (2)    TO  WRK-SYMD
               WHEN   32
                   MOVE    SPA-PTKOH-IDOYMD (3)    TO  WRK-SYMD
           END-EVALUATE
      *
           PERFORM 5101-IDOYMD-ERR-SEC
           .
       5102-IDOYMD-KOHERR-EXT.
           EXIT.
      *
      *
