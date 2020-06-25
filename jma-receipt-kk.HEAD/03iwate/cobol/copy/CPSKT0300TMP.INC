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
      *    診療報酬請求書     COPY (CPSKT0300TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01     中川     2006/08/29   請求先名(県内)追加63→64
      *  01.00.02     吉川     2010/03/11   請求先名(県外)追加8→9
      *  01.00.03     吉川     2011/04/04   東北地方太平洋沖地震対応
      *  01.00.04     井上     2018/10/11   修正履歴コメント削除
      ************************************************************
       01  SKT0300TMP-REC.
           03  SKT0300TMP-KEY.
      *減免区分
               05  SKT0300TMP-GENMEN               PIC  X(01).
      *請求テーブル
           03  SKT0300TMP-SEIKYU-TBL     OCCURS  12.
               05  SKT0300TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT0300TMP-PREF-TBL  OCCURS  2.
                       09  SKT0300TMP-KENSU        PIC  9(06).
                       09  SKT0300TMP-NISSU        PIC  9(06).
                       09  SKT0300TMP-TENSU        PIC  9(10).
      *在宅テーブル
           03  SKT0300TMP-ZAI-TBL      OCCURS  6.
               05  SKT0300TMP-ZPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-ZKENSU           PIC  9(06).
                   07  SKT0300TMP-ZNISSU           PIC  9(06).
                   07  SKT0300TMP-ZTENSU           PIC  9(10).
      *食事療養費テーブル
           03  SKT0300TMP-SKJ-TBL      OCCURS  3.
               05  SKT0300TMP-SPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-SKENSU           PIC  9(06).
                   07  SKT0300TMP-SNISSU           PIC  9(06).
                   07  SKT0300TMP-STENSU           PIC  9(10).
      *合計テーブル
           03  SKT0300TMP-GOKEI-TBL      OCCURS  5.
               05  SKT0300TMP-GPREF-TBL  OCCURS  2.
                   07  SKT0300TMP-GKENSU           PIC  9(06).
                   07  SKT0300TMP-GNISSU           PIC  9(06).
                   07  SKT0300TMP-GTENSU           PIC  9(10).
      *給付割合
           03  SKT0300TMP-KYURATE                  PIC  9(02).
      *県内保険者
           03  SKT0300TMP-NHKNJA-TBL  OCCURS  40.
               05  SKT0300TMP-KENNAI-FLG           PIC  9(01).
      *県名
           03  SKT0300TMP-KENMEI-TBL  OCCURS  50.
               05  SKT0300TMP-KENMEI               PIC  9(02).
      *県外保険者
           03  SKT0300TMP-GHKNJA-TBL   OCCURS  9.
               05  SKT0300TMP-KENGAI-KENSU         PIC  9(02).
      *県外保険者２
           03  SKT0300TMP-GHKNJA-TBL2  OCCURS  50.
               05  SKT0300TMP-KENGAI-KENSU2         PIC  9(02).
      *合計保険者
           03  SKT0300TMP-GOKEIHKNJA-TBL  OCCURS  2.
               05  SKT0300TMP-GOKEIHKNJA           PIC  9(03).
