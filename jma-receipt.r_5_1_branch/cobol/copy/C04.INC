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
          02  C04.
              03  C04-B04-STATE PIC S9(9)   BINARY.
              03  C04-B10-STATE PIC S9(9)   BINARY.
              03  C04-B10-LABEL PIC X(12).
              03  C04-B12-STATE PIC S9(9)   BINARY.
              03  C04-TITLE     PIC X(10).
              03  C04-SELNUM2.
                04  C04-VALUE   PIC S9(5).
              03  C04-INPUTCD-G    OCCURS  6.
                04  C04-INPUTCD-STATE   PIC S9(9)   BINARY.
                04  C04-INPUTCD PIC X(10).
              03  C04-BYOMEI.
                04  C04-VALUE   PIC X(400).
              03  C04-HOSOKUCD-G    OCCURS  3.
                04  C04-HOSOKUCD-STATE   PIC S9(9)   BINARY.
                04  C04-HOSOKUCD PIC X(10).
              03  C04-HOSOKU-COMMENT.
                04  C04-VALUE   PIC X(40).
              03  C04-BUNRUICD-STATE   PIC S9(9)   BINARY.
              03  C04-BUNRUICD.
                04  C04-VALUE   PIC S9(3).
              03  C04-KENBYOMEI-STATE  PIC S9(9)   BINARY.
              03  C04-KENBYOMEI.
                04  C04-VALUE   PIC X(50).
              03  C04-SCR-ACTLST.
                04  C04-BUNRUILST.
                  05  C04-BUNRUI-COUNT   PIC S9(9)   BINARY.
                  05  C04-BUNRUI-ROW     PIC S9(9)   BINARY.
                  05  C04-BUNRUI-ROWATTR PIC S9(9)   BINARY.
                  05  C04-ITEM        OCCURS  100 TIMES.
                    06  C04-BBUNRUICD    PIC X(4).
                    06  C04-BBUNRUIMEI   PIC X(20).
                  05  C04-BUNRUI-SELECT  PIC X       OCCURS  100 TIMES.
              03  C04-SCROLLEDWINDOW28.
                04  C04-BYOMEILST.
                  05  C04-BYOMEILST-COUNT   PIC S9(9)   BINARY.
                  05  C04-BYOMEILST-ROW     PIC S9(9)   BINARY.
                  05  C04-BYOMEILST-ROWATTR PIC S9(9)   BINARY.
                  05  C04-ITEM        OCCURS  700 TIMES.
                    06  C04-TBANGO          PIC X(5).
                    06  C04-TMARK           PIC X(02).
                    06  C04-TBYOMEIINPUTCD  PIC X(10).
                    06  C04-TBYOMEI         PIC X(80).
                  05  C04-BYOMEILST-SELECT  PIC X   OCCURS  100 TIMES.
