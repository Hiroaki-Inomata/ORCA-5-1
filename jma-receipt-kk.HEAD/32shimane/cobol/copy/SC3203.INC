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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         15/07/06  医療機関コードのバイト数変更
      *                                     不要な文字削除
      ********************************************************************
       01  SC03.
      *診療年月
              03  SC03-PRTYM            PIC  X(16).
      *市町村番号
              03  SC03-TOWNNUM          PIC  X(8).
      *公費負担者番号
              03  SC03-KOHFTNJANUM      PIC  X(8).
      *医療機関コード
              03  SC03-HOSPCD.
                  05 SC03-HOSPCD1       PIC  X(02).
                  05 SC03-HOSPCD2       PIC  X(02).
                  05 SC03-HOSPCD3       PIC  X(02).
                  05 SC03-HOSPCD4       PIC  X(02).
                  05 SC03-HOSPCD5       PIC  X(02).
                  05 SC03-HOSPCD6       PIC  X(02).
                  05 SC03-HOSPCD7       PIC  X(02).
      *点数区分表：１医科
              03  SC03-TENHYOKBN1       PIC  X(2).
      *点数区分表：３歯科
              03  SC03-TENHYOKBN2       PIC  X(2).
      *点数区分表：４調剤
              03  SC03-TENHYOKBN3       PIC  X(2).
      *市町村名
              03  SC03-TOWNNAME         PIC  X(14).
      *請求年月日
              03  SC03-SEIYMD           PIC  X(22).
      *住所
              03  SC03-ADRS             PIC  X(100).
      *医療機関名
              03  SC03-HOSPNAME         PIC  X(100).
      *管理者名
              03  SC03-KANRINAME        PIC  X(24).
      *電話番号
              03  SC03-TEL              PIC  X(30).
      *合計
              03  SC03-GOUKEI-TBL       OCCURS  2.
                  05  SC03-KENSU        PIC  X(5).
                  05  SC03-GNISSU       PIC  X(5).
                  05  SC03-GTENSU       PIC  X(9).
                  05  SC03-GTYO         PIC  X(2).
                  05  SC03-GKOUJO       PIC  X(9).
      *公費"90"合計
              03  SC03-KOHISAIKEI90-TBL.
                  05  SC03-GKOHNUM           PIC  X(2).
                  05  SC03-GKKENSU           PIC  X(5).
                  05  SC03-GKTENSU           PIC  X(9).
                  05  SC03-GKFTNKG           PIC  X(9).
      *公費再掲合計
              03  SC03-KOHISAIKEI-TBL   OCCURS  3.
                  05  SC03-KOHNUM           PIC  X(2).
                  05  SC03-KKENSU           PIC  X(5).
                  05  SC03-KTENSU           PIC  X(9).
                  05  SC03-KFTNKG           PIC  X(9).
      *総合計
              03  SC03-GKENSU           PIC  X(5).
      *個別
              03  SC03-KOBETU-TBL       OCCURS  14.
                  05  SC03-JKYSNUM      PIC  X(7).
                  05  SC03-NAME	        PIC  X(18).
                  05  SC03-HKNJANUM     PIC  X(8).
                  05  SC03-KIGONUM      PIC  X(36).
                  05  SC03-KOH1FTNJANUM  PIC  X(8).
                  05  SC03-KOH1JKYJANUM  PIC  X(7).
                  05  SC03-BIRTHDAY     PIC  X(9).
                  05  SC03-SRYYM        PIC  X(4).
                  05  SC03-NYUGAIKBN    PIC  X(1).
                  05  SC03-KYUFURATE    PIC  X(1).
                  05  SC03-TYOKI        PIC  X(2).
                  05  SC03-NISSU        PIC  X(2).
                  05  SC03-TENSU        PIC  X(7).
                  05  SC03-KOH1TEN      PIC  X(7).
                  05  SC03-NYUYOJITEN   PIC  X(7).
                  05  SC03-KOH1FTNKG    PIC  X(7).
                  05  SC03-KOUJO        PIC  X(7).
