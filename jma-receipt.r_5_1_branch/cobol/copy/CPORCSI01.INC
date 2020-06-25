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
      *    入院料加算算定可否チェック COPY (CPORCSI01.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ************************************************************
       01  SI01-AREA.
           03  SI01-PTID                   PIC 9(10).
           03  SI01-SRYYMD                 PIC X(08).
           03  SI01-NYUINYMD               PIC X(08).
           03  SI01-BTUNUM                 PIC X(02).
           03  SI01-BTUSBT                 PIC X(02).
           03  SI01-SNTNYUINCD             PIC X(09).
           03  SI01-JOKUSOUYMD             PIC X(08).
           03  SI01-JOKUSOUKBN             PIC X(01).
           03  SI01-KSN-G.
               05  SI01-KSN-MAX            PIC 9(03).
               05  SI01-KSN-OCC            OCCURS 3.
                   07  SI01-KSN-NAME       PIC X(100).
                   07  SI01-KSN-SRYCD      PIC X(09).
           03  SI01-KSNLST-G.
               05  SI01-KSNLST-MAX         PIC 9(03).
               05  SI01-KSNLST-OCC         OCCURS 20.
                   07  SI01-KSNLST-NAME    PIC X(100).
                   07  SI01-KSNLST-SRYCD   PIC X(09).
