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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2007/10/03   システム管理マスタ「1901」の
      *                                     医療機関情報を参照
      *  01.00.02    萩野      2018/09/10   修正履歴コメント削除
      ************************************************************
       01  SC03.
      *市町村名
              03  SC03-CITYNAME        PIC  X(22).
      *公費負担者番号
              03  SC03-FTNJANUM        PIC  X(16).
      *印刷日付
              03  SC03-PRTYM           PIC  X(16).
      *請求日付
              03  SC03-SEIYMD          PIC  X(22).
      *ページ
              03  SC03-PAGE            PIC  X(4).
      *医療機関コード
              03  SC03-HOSPCD          PIC  X(14).
      *住所
              03  SC03-ADRS            PIC  X(100).
      *医療機関名
              03  SC03-HOSPNAME        PIC  X(100).
      *管理者名
              03  SC03-KAISETUNAME     PIC  X(40).
      *入外区分
              03  SC03-NYUGAIKBN-TBL OCCURS  2.
                  05  SC03-NYUGAIKBN   PIC  X(2).
      *受給者番号
              03  SC03-JKYSNUM-TBL   OCCURS  10.
                  05  SC03-JKYSNUM     PIC  X(20).
      *受給者氏名
              03  SC03-NAME-TBL      OCCURS  10.
                  05  SC03-NAME        PIC  X(22).
      *生年月日
              03  SC03-BIRTHDAY-TBL  OCCURS  10.
                  05  SC03-BIRTHDAY    PIC  X(12).
      *一部負担割合
              03  SC03-FTNRATE-TBL   OCCURS  10.
                  05  SC03-FTNRATE     PIC  X(1).
      *診療日数
              03  SC03-NISSU-TBL     OCCURS  10.
                  05  SC03-NISSU       PIC  X(2).
      *保険総点数
              03  SC03-TENSU-TBL     OCCURS  10.
                  05  SC03-TENSU       PIC  X(7).
      *窓口徴収額
              03  SC03-ITBFTN-TBL    OCCURS  10.
                  05  SC03-ITBFTN      PIC  X(7).
      *入院食事療養費−基準額
              03  SC03-SKINGK-TBL    OCCURS  10.
                  05  SC03-SKINGK      PIC  X(7).
      *入院食事療養費−標準負担額
              03  SC03-SFTNGK-TBL OCCURS  10.
                  05  SC03-SFTNGK      PIC  X(7).
      *備考
              03  SC03-BIKO-TBL      OCCURS  10.
                  05  SC03-BIKO        PIC  X(20).
      *小計
              03  SC03-SYOKEI-TBL.
      *小計−件数
                  05  SC03-S-KENSU      PIC  X(2).
      *小計−診療日数
                  05  SC03-S-NISSU      PIC  X(3).
      *小計−保険総点数
                  05  SC03-S-TENSU      PIC  X(9).
      *小計−窓口徴収額
                  05  SC03-S-ITBFTN     PIC  X(9).
      *小計−入院時食事療養費−基準額
                  05  SC03-S-SKINGK     PIC  X(9).
      *小計−入院時食事療養費−標準負担額
                  05  SC03-S-SFTNGK     PIC  X(9).
      *合計
              03  SC03-GOKEI-TBL.
      *合計−件数
                  05  SC03-G-KENSU      PIC  X(5).
      *合計−診療日数
                  05  SC03-G-NISSU      PIC  X(5).
      *合計−保険総点数
                  05  SC03-G-TENSU      PIC  X(9).
      *合計−窓口徴収額
                  05  SC03-G-ITBFTN    PIC  X(9).
      *合計−入院時食事療養費−基準額
                  05  SC03-G-SKINGK    PIC  X(9).
      *合計−入院時食事療養費−標準負担額
                  05  SC03-G-SFTNGK    PIC  X(9).
