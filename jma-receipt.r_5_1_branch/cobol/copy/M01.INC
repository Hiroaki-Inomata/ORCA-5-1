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
         02  M01.
            03  M01-TITLE           PIC X(120).
            03  M01-BGCOLORS.
              04  M01-VALUE   PIC X(20).
            03  M01-FIXED1.
              04  M01-B08-LABEL     PIC X(10).
              04  M01-STATE-G.
                05  M01-G11-G.
                  06  M01-G11-VISIBLE PIC X(01).
                  06  M01-G11-STATE   PIC S9(09) BINARY.
                05  M01-G12-G.
                  06  M01-G12-VISIBLE PIC X(01).
                  06  M01-G12-STATE   PIC S9(09) BINARY.
                05  M01-G13-G.
                  06  M01-G13-VISIBLE PIC X(01).
                  06  M01-G13-STATE   PIC S9(09) BINARY.
                05  M01-G14-G.
                  06  M01-G14-VISIBLE PIC X(01).
                  06  M01-G14-STATE   PIC S9(09) BINARY.
                05  M01-G21-G.
                  06  M01-G21-VISIBLE PIC X(01).
                  06  M01-G21-STATE   PIC S9(09) BINARY.
                05  M01-G22-G.
                  06  M01-G22-VISIBLE PIC X(01).
                  06  M01-G22-STATE   PIC S9(09) BINARY.
                05  M01-G23-G.
                  06  M01-G23-VISIBLE PIC X(01).
                  06  M01-G23-STATE   PIC S9(09) BINARY.
                05  M01-G24-G.
                  06  M01-G24-VISIBLE PIC X(01).
                  06  M01-G24-STATE   PIC S9(09) BINARY.
                05  M01-G41-G.
                  06  M01-G41-VISIBLE PIC X(01).
                  06  M01-G41-STATE   PIC S9(09) BINARY.
                05  M01-G42-G.
                  06  M01-G42-VISIBLE PIC X(01).
                  06  M01-G42-STATE   PIC S9(09) BINARY.
                05  M01-G43-G.
                  06  M01-G43-VISIBLE PIC X(01).
                  06  M01-G43-STATE   PIC S9(09) BINARY.
                05  M01-G44-G.
                  06  M01-G44-VISIBLE PIC X(01).
                  06  M01-G44-STATE   PIC S9(09) BINARY.
                05  M01-G51-G.
                  06  M01-G51-VISIBLE PIC X(01).
                  06  M01-G51-STATE   PIC S9(09) BINARY.
                05  M01-G52-G.
                  06  M01-G52-VISIBLE PIC X(01).
                  06  M01-G52-STATE   PIC S9(09) BINARY.
                05  M01-G71-G.
                  06  M01-G71-VISIBLE PIC X(01).
                  06  M01-G71-STATE   PIC S9(09) BINARY.
                05  M01-G82-G.
                  06  M01-G82-VISIBLE PIC X(01).
                  06  M01-G82-STATE   PIC S9(09) BINARY.
                05  M01-G91-G.
                  06  M01-G91-VISIBLE PIC X(01).
                  06  M01-G91-STATE   PIC S9(09) BINARY.
                05  M01-G92-G.
                  06  M01-G92-VISIBLE PIC X(01).
                  06  M01-G92-STATE   PIC S9(09) BINARY.
              04  M01-STATE-R       REDEFINES  M01-STATE-G.
                05  M01-STATE-OCC   OCCURS 18.
                  06  M01-VISIBLE-O   PIC X(1).
                  06  M01-STATE-O     PIC S9(09) BINARY.
              04  M01-HTML1-G.
                05  M01-HTML1         PIC X(200).
              04  M01-FR6-VISIBLE     PIC X(1).
              04  M01-SELNUM  PIC 9(2).
              04  M01-HOSPID-G.
                05  M01-HOSPID-STYLE  PIC X(20).
                05  M01-HOSPID        PIC X(24).
              04  M01-CONTRACTMSG-G.
                05  M01-CONTRACTMSG-STYLE PIC X(20).
                05  M01-CONTRACTMSG       PIC X(10).
