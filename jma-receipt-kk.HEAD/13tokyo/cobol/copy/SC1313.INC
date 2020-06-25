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
      ********************************************************************
       01  SC13.
      *医療機関コード
           03  SC13-HOSPCD             PIC X(14).
      *診療年月
           03  SC13-PRTYM              PIC X(16).
      *請求年月日
           03  SC13-SEIYMD             PIC X(22).
      *点数区分表：１医科 ２歯科 ４調剤 ６看護
           03  SC13-TENHYOKBN          PIC X(02) OCCURS 4.
      *住所
           03  SC13-ADRS               PIC X(100).
      *医療機関名
           03  SC13-HOSPNAME           PIC X(100).
      *電話番号
           03  SC13-TEL                PIC X(15).
      *開設者名
           03  SC13-KAISETUNAME        PIC X(40).
      *NO
           03  SC13-NO                 PIC X(03).
      *負担者番号
           03  SC13-FTNJA-TBL.
               05  SC13-FTNJANUM       PIC X(08) OCCURS 10.
      *受給者番号
           03  SC13-JKYSNUM-TBL.
               05  SC13-JKYSNUM        PIC X(14) OCCURS 10.
      *保険者番号
           03  SC13-HKNJANUM-TBL.
               05  SC13-HKNJANUM       PIC X(16) OCCURS 10.
      *患者カナ氏名
           03  SC13-KANANAME-TBL.
               05  SC13-KANANAME       PIC X(20) OCCURS 10.
      *患者氏名
           03  SC13-NAME-TBL.
               05  SC13-NAME           PIC X(20) OCCURS 10.
      *負担割合
           03  SC13-FTNRATE-TBL.
               05  SC13-FTNRATE-G OCCURS 10.
                   07  SC13-FTNRATE    PIC X(02) OCCURS 3.
      *入院・外来
           03  SC13-NYUGAI-TBL.
               05  SC13-NYUGAI-G OCCURS 10.
                   07  SC13-NYUGAIKBN  PIC X(02) OCCURS 2.
      *診療日数
           03  SC13-NISSU-TBL.
               05  SC13-NISSU          PIC X(02) OCCURS 10.
      *請求額
           03  SC13-SEIKYUGK-TBL.
               05  SC13-SEIKYUGK       PIC X(07) OCCURS 10.
      *食事・生活療養標準負担額
           03  SC13-SFTNGK-TBL.
               05  SC13-SFTNGK         PIC X(07) OCCURS 10.
      *食事・生活区分
           03  SC13-SKJKBN-TBL OCCURS 10.
               05  SC13-SKJKBN         PIC X(02) OCCURS 2.
      *公費分点数
           03  SC13-TENSU-TBL.
               05  SC13-TENSU          PIC X(07) OCCURS 10.
      *一部負担金相当額
           03  SC13-ITBFTN-TBL.
               05  SC13-ITBFTN         PIC X(07) OCCURS 10.
      *備考欄
           03  SC13-BIKO-TBL OCCURS 10.
               05  SC13-BIKO1          PIC X(02) OCCURS 2.
               05  SC13-BIKO2          PIC X(02) OCCURS 2.
      *合計
           03  SC13-GOKEI-TBL.
      *合計件数
               05  SC13-GKENSU         PIC X(02).
      *合計請求額
               05  SC13-GSEIKYUGK      PIC X(09).
