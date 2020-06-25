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
        02  K04.
      **** 02  K04-FIXED31.
              03  K04-PTNUM.
                04  K04-VALUE   PIC X(20).
              03  K04-KANANAME.
                04  K04-VALUE   PIC X(30).
              03  K04-NAME.
                04  K04-VALUE   PIC X(30).
              03  K04-BIRTHDAY.
                04  K04-VALUE   PIC X(9).
              03  K04-SEX.
                04  K04-VALUE   PIC X(2).
              03  K04-SRYYM.
                04  K04-VALUE   PIC X(9).
              03  K04-SRYKA.
                04  K04-VALUE   PIC X(10).
              03  K04-SANTEILIST.
                04  K04-SANTEILIST-COUNT   PIC S9(9)   BINARY.
                04  K04-SANTEILIST-ROW     PIC S9(9)   BINARY.
                04  K04-SANTEILIST-ROWATTR PIC S9(9)   BINARY.
                04  K04-SANTEILIST-ITEM        OCCURS  30 TIMES.
                  06  K04-SANTEINAME  PIC X(50).
                  06  K04-SRYKAMEI    PIC X(8).
                  06  K04-SYOKAIYMD   PIC X(9).
                  06  K04-SANTEIYMD   PIC X(9).
                  06  K04-KAISU       PIC X(3).
                  06  K04-YY          PIC X(4).
                  06  K04-MM          PIC X(2).
                  06  K04-DD          PIC X(2).
                04  K04-SANTEILIST-SELECT  PIC X       OCCURS  30 TIMES.
              03  K04-NENREI.
                04  K04-VALUE   PIC X(6).
