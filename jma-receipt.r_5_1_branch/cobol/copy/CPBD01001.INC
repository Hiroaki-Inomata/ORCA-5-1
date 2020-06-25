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
      *****************************************************************
      *    住所                COPY (CPBD01001.INC)        *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  BD01001-REC.
           03  BD01001-KEY.
               05  BD01001-SRT-NYUGAIKBN   PIC X(01).
               05  BD01001-SRT-SRYKA       PIC X(02).
               05  BD01001-DENPNUM         PIC 9(07).
               05  BD01001-SYURRKNUM-G.
                   07  BD01001-SYURRKNUM   PIC 9(9).
                   07  BD01001-SYUEDANUM   PIC 9(9).
           03  BD01001-HOKENTEN            PIC S9(10).
           03  BD01001-SKYMONEY            PIC S9(10).
           03  BD01001-FTNMONEY            PIC S9(10).
           03  BD01001-JIHIMONEY           PIC S9(10).
           03  BD01001-CHOSEI              PIC S9(10).
           03  BD01001-GENMEN              PIC S9(10).
           03  BD01001-ETCJIHI             PIC S9(10)
                                           OCCURS 10.
           03  BD01001-MISYU               PIC S9(10).
           03  BD01001-PTNUM               PIC X(20).
           03  BD01001-NAME                PIC X(100).
           03  BD01001-SRYYMD              PIC X(08).
           03  BD01001-DENPPRTYMD          PIC X(08).
           03  BD01001-HOKEN               PIC 9(02).
           03  BD01001-SRYKA               PIC X(02).
           03  BD01001-SYOSAIKBN           PIC X(01).
           03  BD01001-SKYNYUKBN           PIC X(01).
           03  BD01001-MAX-SYURRKNUM-G.
               07  BD01001-MAX-SYURRKNUM   PIC 9(9).
               07  BD01001-MAX-SYUEDANUM   PIC 9(9).
