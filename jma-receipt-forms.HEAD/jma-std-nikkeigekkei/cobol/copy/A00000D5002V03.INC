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
      *  プログラム修正履歴
      *  作成日付    作業者       記述
      *  15/01/27    @@@@-門間    新規作成
      ********************************************************************
       01  D5002V03.
           03  D5002V03-SYSYMD      PIC X(22).
           03  D5002V03-SRYYMD      PIC X(22).
           03  D5002V03-PAGE        PIC X(3).
           03  D5002V03-MIDASI      PIC X(10).
           03  D5002V03-JOBMSG      PIC X(60).
           03  D5002V03-NYUGAIKBN   PIC X(4).
           03  D5002V03-SRYKA       PIC X(20).
           03  D5002V03-TJIHI-G     OCCURS  10 TIMES.
               05  D5002V03-TJIHI     PIC X(8).
           03  D5002V03-HKNTEN      PIC X(09).
           03  D5002V03-SYUEKI      PIC X(10).
           03  D5002V03-MONEY-TBL   OCCURS  20 TIMES.
               05  D5002V03-MONEY     PIC X(08).
           03  D5002V03-SEIKYU      PIC X(10).
           03  D5002V03-RYOSYU      PIC X(10).
           03  D5002V03-JIHIKEI     PIC X(10).
