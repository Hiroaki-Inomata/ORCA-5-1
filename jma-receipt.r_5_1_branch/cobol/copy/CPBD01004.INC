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
      *    住所                COPY (CPBD01004.INC)        *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  BD01004-REC.
           03  BD01004-KEY.
               05  BD01004-SRT-NYUGAIKBN   PIC X(01).
               05  BD01004-SRT-SRYKA       PIC X(02).
               05  BD01004-DENPNUM         PIC 9(07).
               05  BD01004-DENPEDANUM      PIC 9(02).
               05  BD01004-SYURRKNUM       PIC 9(02).
               05  BD01004-SYUEDANUM       PIC 9(02).
               05  BD01004-RENNUM          PIC 9(09).
               05  BD01004-RENNUM2         PIC 9(03).
               05  BD01004-PTID            PIC 9(10).
           03  BD01004-SKYNYUKBN           PIC X(01).
           03  BD01004-HOKENTEN            PIC S9(10).
           03  BD01004-SKYMONEY            PIC S9(10).
           03  BD01004-FTNMONEY            PIC S9(10).
           03  BD01004-JIHIMONEY           PIC S9(10).
           03  BD01004-CHOSEI              PIC S9(10).
           03  BD01004-CHOSEI1             PIC S9(10).
           03  BD01004-CHOSEI2             PIC S9(10).
           03  BD01004-GENMEN              PIC S9(10).
           03  BD01004-ZENMISYU            PIC S9(10).
           03  BD01004-ETCJIHI             PIC S9(10)
                                           OCCURS 10.
           03  BD01004-DSPMISYUKBN         PIC X(01).
           03  BD01004-MISYU               PIC S9(10).
           03  BD01004-TTLMISYU            PIC S9(10).
           03  BD01004-PTNUM               PIC X(20).
           03  BD01004-NAME                PIC X(100).
           03  BD01004-SRYYMD              PIC X(08).
           03  BD01004-DENPPRTYMD          PIC X(08).
           03  BD01004-HOKEN               PIC 9(02).
           03  BD01004-SRYKA               PIC X(02).
           03  BD01004-SYOSAIKBN           PIC X(01).
           03  BD01004-HOHO                PIC X(02).
           03  BD01004-NYUKIN              PIC S9(9).
           03  BD01004-KONNYUKIN           PIC S9(9).
