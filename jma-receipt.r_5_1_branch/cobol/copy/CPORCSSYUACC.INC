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
      *    主情報アクセス用パラメタ      COPY (CPORCSSYUACC.INC)
      *                                  使用プログラム (ORCSSYUACC)
      *    設定項目
      *      処理区分
      *      診療年月
      *      入外区分
      *      患者ＩＤ
      *      入力科
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     kohid 追加
      *  04.06.02    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      ************************************************************
      *
           03  LNK-SYUACC-IN.
      *        処理区分
      *        1:当月の情報取得
               05  LNK-SYUACC-I-KBN       PIC X(02).
      *        診療年月
               05  LNK-SYUACC-I-SRYYM     PIC 9(06).
      *        入外区分
               05  LNK-SYUACC-I-NYUGAIKBN PIC X(01).
      *        患者ＩＤ
               05  LNK-SYUACC-I-PTID      PIC 9(10).
      *
           03  LNK-SYUACC-IO.
      *        診療科(手入力情報)
               05  LNK-SYUACC-IO-H-SRYKA   PIC X(02).
      *        レセプト情報
               05  LNK-SYUACC-IO-OCC               OCCURS  100.
      *            入外区分
                   07  LNK-SYUACC-IO-NYUGAIKBN
                                       PIC X(01).
      *            審査支払機関
                   07  LNK-SYUACC-IO-TEISYUTUSAKI
                                       PIC 9(01).
      *            レセプト種別
                   07  LNK-SYUACC-IO-RECESYUBETU
                                       PIC X(04).
      *            保険者番号
                   07  LNK-SYUACC-IO-HKNJANUM
                                       PIC X(08).
      *            補助区分
                   07  LNK-SYUACC-IO-HOJOKBN
                                       PIC X(01).
      *            公費ＩＤ（公費単独のみ）
                   07  LNK-SYUACC-IO-KOHID
                                       PIC 9(10).
      *            分割区分
                   07  LNK-SYUACC-IO-RECEIPT-KBN
                                       PIC X(10).
      *            保険番号（主保険）
                   07  LNK-SYUACC-IO-HKNNUM
                                       PIC X(03).
      *            システム主科
                   07  LNK-SYUACC-IO-SYUKA-AUTO
                                       PIC X(02).
      *            手入力主科
                   07  LNK-SYUACC-IO-SYUKA-INPUT
                                       PIC X(02).
      *            主科別情報
                   07  LNK-SYUACC-IO-SYUKA-AREA.
                       09  LNK-SYUACC-IO-SRYKATBL      OCCURS  50.
      *                    主科
                           11  LNK-SYUACC-IO-SRYKA
                                       PIC X(02).
      *                    保険組合せ番号
                           11  LNK-SYUACC-IO-HKNCOMBI-AREA.
                               13  LNK-SYUACC-IO-HKNCOMBI
                                       PIC 9(04)   OCCURS  10.
      *                    請求点数合計
                           11  LNK-SYUACC-IO-TENSU
                                       PIC S9(09).
      *                    受診回数合計(入院外)
                           11  LNK-SYUACC-IO-KAISU    
                                       PIC 9(03).
      *                    通算日数合計(入院)
                           11  LNK-SYUACC-IO-NISSU
                                       PIC 9(03).
      *                    終了日
                           11  LNK-SYUACC-IO-EDDAY
                                       PIC 9(02).
      *
           03  LNK-SYUACC-IO2.
      *      主科データと同じレイアウト
             05  LNK-SYUACC-IO2-OCC               OCCURS  100.
               07  LNK-IO2-SYU-KEY.
      *医療機関ＩＤ
                 09  LNK-IO2-SYU-HOSPNUM       PIC 9(2).
      *入外区分
                 09  LNK-IO2-SYU-NYUGAIKBN     PIC X(01).
      *患者ＩＤ
                 09  LNK-IO2-SYU-PTID          PIC 9(10).
      *会計区分
                 09  LNK-IO2-SYU-TEN-KBN       PIC X(01).
      *診療年月
                 09  LNK-IO2-SYU-SRYYM         PIC 9(06).
      *審査支払機関
                 09  LNK-IO2-SYU-TEISYUTUSAKI  PIC 9(01).
      *保険者番号
                 09  LNK-IO2-SYU-HKNJANUM      PIC X(08).
      *補助区分
                 09  LNK-IO2-SYU-HOJOKBN       PIC X(01).
      *公費ＩＤ（公費単独のみ）
                 09  LNK-IO2-SYU-KOHID         PIC 9(10).
      *分割区分
                 09  LNK-IO2-SYU-RECEIPT-KBN   PIC X(10).
      *レセプト種別
                 09  LNK-IO2-SYU-RECESYUBETU   PIC X(04).
      *診療科
                 09  LNK-IO2-SYU-SRYKA         PIC X(02).
      *保険番号（主保険）
               07  LNK-IO2-SYU-HKNNUM          PIC X(03).
      *診療科（入力）
               07  LNK-IO2-SYU-SRYKA-INPUT     PIC X(02).
      *診療科（システム）
               07  LNK-IO2-SYU-SRYKA-AUTO      PIC X(02).
      *保険組合せ番号
               07  LNK-IO2-SYU-HKNCOMBI-AREA.
                 09  LNK-IO2-SYU-HKNCOMBI      PIC 9(04)
                                               OCCURS  10.
      *請求点数合計
               07  LNK-IO2-SYU-TENSU           PIC S9(09).
      *受診回数合計(入院外)
               07  LNK-IO2-SYU-KAISU           PIC 9(03).
      *通算日数合計(入院)
               07  LNK-IO2-SYU-NISSU           PIC 9(03).
      *終了日
               07  LNK-IO2-SYU-EDDAY           PIC 9(02).
      *未請求レセで使う予定
               07  LNK-IO2-SYU-MISEIKYU-FLG    PIC X(02).
      *削除フラグ
               07  LNK-IO2-SYU-DLT-FLG         PIC X(01).
      *端末ＩＤ
               07  LNK-IO2-SYU-TERMID          PIC X(16).
      *オペーレータＩＤ
               07  LNK-IO2-SYU-OPID            PIC X(16).
      *作成年月日
               07  LNK-IO2-SYU-CREYMD          PIC X(08).
      *更新年月日
               07  LNK-IO2-SYU-UPYMD           PIC X(08).
      *更新時間
               07  LNK-IO2-SYU-UPHMS           PIC X(06).
      *    復帰コード
           03  LNK-SYUACC-RC           PIC S9(9) BINARY.
