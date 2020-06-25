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
      *    診療報酬請求書     COPY (CPSKT2810TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    内田    09/09/29    平成21年台風第9号対応（対象者別集計）
      *  01.00.02    萩野    19/03/08    修正履歴コメント削除
      ************************************************************
       01  SKT2810TMP-REC.
           03    SKT2810TMP-KEY.
               05    SKT2810TMP-SYUTURYOKU    PIC X(01).
               05    SKT2810TMP-HKNJANUM      PIC X(06).
               05    SKT2810TMP-ZAITAKU       PIC X(01).
               05    SKT2810TMP-SRYYM         PIC X(06).
      *    平成21年台風第9号対応
               05    SKT2810TMP-GENMEN        PIC X(01).
      *
           03    SKT2810TMP-KOH-TBL           OCCURS 2.
               05    SKT2810TMP-KOHKENSU      PIC 9(05).
               05    SKT2810TMP-KOHITBFTN     PIC 9(07).
      *
           03    SKT2810TMP-SEIKYU-TBL        OCCURS 12.
               05    SKT2810TMP-NYUGAI-TBL    OCCURS 2.
                 07    SKT2810TMP-KENSU       PIC 9(05).
                 07    SKT2810TMP-NISSU       PIC 9(05).
                 07    SKT2810TMP-TENSU       PIC 9(09).
                 07    SKT2810TMP-ITBFTN      PIC 9(09).
               05    SKT2810TMP-SKENSU        PIC 9(05).
               05    SKT2810TMP-SNISSU        PIC 9(05).
               05    SKT2810TMP-SKINGK        PIC 9(09).
               05    SKT2810TMP-SFTNGK        PIC 9(09).
