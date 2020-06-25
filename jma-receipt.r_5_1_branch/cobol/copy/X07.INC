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
          02  X07.
              03  X07-B04-STATE PIC S9(9)   BINARY.
              03  X07-B06-STATE PIC S9(9)   BINARY.
              03  X07-B07-STATE PIC S9(9)   BINARY.
              03  X07-B12-STATE PIC S9(9)   BINARY.
              03  X07-TITLE     PIC X(10).
              03  X07-LBLBYOMEI PIC X(20).
              03  X07-SELNUM2.
                04  X07-VALUE   PIC S9(5).
              03  X07-INPUTCD-G    OCCURS  6.
                04  X07-INPUTCD-STATE   PIC S9(9)   BINARY.
                04  X07-INPUTCD-VISIBLE PIC X(1).
                04  X07-INPUTCD PIC X(10).
              03  X07-BYOMEI.
                04  X07-VALUE   PIC X(400).
              03  X07-BUNRUICD-STATE   PIC S9(9)   BINARY.
              03  X07-BUNRUICD.
                04  X07-VALUE   PIC S9(3).
              03  X07-KENBYOMEI-STATE  PIC S9(9)   BINARY.
              03  X07-KENBYOMEI.
                04  X07-VALUE   PIC X(50).
              03  X07-SCR-ACTLST.
                04  X07-BUNRUILST.
                  05  X07-BUNRUI-COUNT   PIC S9(9)   BINARY.
                  05  X07-BUNRUI-ROW     PIC S9(9)   BINARY.
                  05  X07-BUNRUI-ROWATTR PIC S9(9)   BINARY.
                  05  X07-ITEM        OCCURS  100 TIMES.
                    06  X07-BBUNRUICD    PIC X(4).
                    06  X07-BBUNRUIMEI   PIC X(20).
                  05  X07-BUNRUI-SELECT  PIC X       OCCURS  100 TIMES.
              03  X07-SCROLLEDWINDOW28.
                04  X07-BYOMEILST.
                  05  X07-BYOMEILST-COUNT   PIC S9(9)   BINARY.
                  05  X07-BYOMEILST-ROW     PIC S9(9)   BINARY.
                  05  X07-BYOMEILST-ROWATTR PIC S9(9)   BINARY.
                  05  X07-ITEM        OCCURS  100 TIMES.
                    06  X07-TBANGO          PIC X(5).
                    06  X07-TBYOMEIINPUTCD  PIC X(10).
                    06  X07-TBYOMEI         PIC X(80).
                  05  X07-BYOMEILST-SELECT  PIC X   OCCURS  100 TIMES.
