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
      * 01.00.01     @@@@-田中    10/12/13  コメント履歴削除
      ********************************************************************
       01  D1311.
           03  D1311-TITLE.
             05  D1311-SUBTITLE       PIC X(30).
             05  D1311-JOBMSG         PIC X(60).
             05  D1311-PAGE           PIC X(3).
             05  D1311-SYSYMD         PIC X(22).
             05  D1311-FRYMD          PIC X(22).
             05  D1311-TOYMD          PIC X(22).
             05  D1311-SRYKA          PIC X(20).
             05  D1311-DRNAME         PIC X(30).
             05  D1311-TJIHI-G        OCCURS 10 TIMES.
               07  D1311-TJIHI        PIC X(12).
           03  D1311-DATA.
             05  D1311-TTIKU-G        OCCURS 10 TIMES.
               07  D1311-TTIKU        PIC X(20).
             05  D1311-YOSO-G         OCCURS 10 TIMES.
               07  D1311-YOSO-OCC     OCCURS 11 TIMES.
                 09  D1311-YOSO       PIC X(11).
