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
      *  01.00.01    萩野      2018/09/10   修正履歴コメント削除
      ************************************************************
       01  SC05.
      *公費負担者番号
              03  SC05-FTNJANUM        PIC  X(16).
      *ページ
              03  SC05-PAGE            PIC  X(04).
      *印刷日付
              03  SC05-PRTYM           PIC  X(16).
      *請求日付
              03  SC05-SEIYMD          PIC  X(22).
      *市町村名
              03  SC05-CITYNAME        PIC  X(22).
      *医療機関コード
              03  SC05-HOSPCD          PIC  X(14).
      *住所
              03  SC05-ADRS            PIC  X(100).
      *医療機関名
              03  SC05-HOSPNAME        PIC  X(100).
      *開設者名
              03  SC05-KAISETUNAME     PIC  X(40).
      *公費名称
              03  SC05-KOHNAME         PIC  X(10).
      *入外区分
              03  SC05-NYUGAIKBN-TBL OCCURS  2.
                  05  SC05-NYUGAIKBN   PIC  X(02).
      *受給者番号
              03  SC05-JKYSNUM-TBL   OCCURS  10.
                  05  SC05-JKYSNUM     PIC  X(20).
      *受給者氏名
              03  SC05-NAME-TBL      OCCURS  10.
                  05  SC05-NAME        PIC  X(22).
      *生年月日
              03  SC05-TANJOY-TBL  OCCURS  10.
                  05  SC05-TANJOY      PIC  X(04).
              03  SC05-TANJOM-TBL  OCCURS  10.
                  05  SC05-TANJOM      PIC  X(04).
              03  SC05-TANJOD-TBL  OCCURS  10.
                  05  SC05-TANJOD      PIC  X(04).
      *一部負担割合
              03  SC05-FTNRATE-TBL   OCCURS  10.
                  05  SC05-FTNRATE     PIC  X(01).
      *診療日数
              03  SC05-NISSU-TBL     OCCURS  10.
                  05  SC05-NISSU       PIC  X(02).
      *保険総点数
              03  SC05-TENSU-TBL     OCCURS  10.
                  05  SC05-TENSU       PIC  X(07).
      *窓口徴収額
              03  SC05-ITBFTN-TBL    OCCURS  10.
                  05  SC05-ITBFTN      PIC  X(07).
      *入院食事療養費−基準額
              03  SC05-SKINGK-TBL OCCURS  10.
                  05  SC05-SKINGK      PIC  X(07).
      *入院食事療養費−標準負担額
              03  SC05-SFTNGK-TBL OCCURS  10.
                  05  SC05-SFTNGK      PIC  X(07).
      *備考(月後れ用)
              03  SC05-BIKO-TBL OCCURS  10.
                  05  SC05-BIKO        PIC  X(20).
      *小計
              03  SC05-SYOKEI-TBL.
      *小計−件数
                  05  SC05-S-KENSU     PIC  X(02).
      *小計−診療日数
                  05  SC05-S-NISSU     PIC  X(03).
      *小計−保険総点数
                  05  SC05-S-TENSU     PIC  X(09).
      *小計−窓口徴収額
                  05  SC05-S-ITBFTN    PIC  X(09).
      *小計−入院時食事療養費−基準額
                  05  SC05-S-SKINGK    PIC  X(09).
      *小計−入院時食事療養費−標準負担額
                  05  SC05-S-SFTNGK    PIC  X(09).
      *合計
              03  SC05-GOKEI-TBL.
      *合計−件数
                  05  SC05-G-KENSU     PIC  X(05).
      *合計−診療日数
                  05  SC05-G-NISSU     PIC  X(05).
      *合計−保険総点数
                  05  SC05-G-TENSU     PIC  X(09).
      *合計−窓口徴収額
                  05  SC05-G-ITBFTN    PIC  X(09).
      *合計−入院時食事療養費−基準額
                  05  SC05-G-SKINGK    PIC  X(09).
      *合計−入院時食事療養費−標準負担額
                  05  SC05-G-SFTNGK    PIC  X(09).
