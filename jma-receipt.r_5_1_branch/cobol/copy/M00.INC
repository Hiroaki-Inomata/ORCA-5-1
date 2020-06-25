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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
        02  M00.
            03  M00-TITLE          PIC X(120).
            03  M00-BGCOLORS.
              04  M00-VALUE   PIC X(20).
            03  M00-FIXED1.
              04  M00-STATE-G.
                05  M00-G01.
                  06  M00-G01-STATE   PIC S9(9)   BINARY.
                05  M00-G03.
                  06  M00-G03-STATE   PIC S9(9)   BINARY.
                  06  M00-G03-VISIBLE PIC X(1).
              04  M00-STATE-R       REDEFINES  M00-STATE-G.
                05  M00-STATE-O     PIC S9(09) BINARY  OCCURS  2.
              04  M00-SELNUM.
                05  M00-VALUE   PIC 9(2).
              04  M00-LBLMSG-G.
                05  M00-LBLMSG-STYLE   PIC X(20).
                05  M00-LBLMSG         PIC X(60).
              04  M00-LBLMSG2-G.
                05  M00-LBLMSG2-STYLE   PIC X(20).
                05  M00-LBLMSG2         PIC X(100).
              04  M00-LBLMSG3-G.
                05  M00-LBLMSG3-STYLE   PIC X(20).
                05  M00-LBLMSG3         PIC X(60).
              04  M00-MNTTITLE          PIC X(20).
              04  M00-MNTYMD            PIC X(20).
              04  M00-MNTMSG-G.
                05  M00-MNTMSG-STYLE    PIC X(20).
                05  M00-MNTMSG          PIC X(70).
              04  M00-MLITITLE          PIC X(20).
              04  M00-MLIYMD            PIC X(20).
              04  M00-MLIMSG-G.
                05  M00-MLIMSG-STYLE    PIC X(20).
                05  M00-MLIMSG          PIC X(70).
              04  M00-HTML1-G.
                05  M00-HTML1          PIC X(200).
              04  M00-B04-G.
                05  M00-B04-STATE      PIC S9(9)   BINARY.
              04  M00-B05-G.
                05  M00-B05-LABEL      PIC X(10).
                05  M00-B05-STATE      PIC S9(9)   BINARY.
              04  M00-B06-G.
                05  M00-B06-STATE      PIC S9(9)   BINARY.
              04  M00-B09-G.
                05  M00-B09-LABEL      PIC X(10).
                05  M00-B09-STATE      PIC S9(9)   BINARY.
              04  M00-B10-G.
                05  M00-B10-LABEL      PIC X(10).
                05  M00-B10-STATE      PIC S9(9)   BINARY.
              04  M00-B11-G.
                05  M00-B11-LABEL      PIC X(10).
                05  M00-B11-STATE      PIC S9(9)   BINARY.
              04  M00-HOSPID-G.
                05  M00-HOSPID-STYLE   PIC X(20).
                05  M00-HOSPID         PIC X(24).
              04  M00-CONTRACTMSG-G.
                05  M00-CONTRACTMSG-STYLE  PIC X(20).
                05  M00-CONTRACTMSG    PIC X(60).
