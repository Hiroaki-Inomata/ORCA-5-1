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
         02  M01N.
            03  M01N-TITLE           PIC X(120).
            03  M01N-BGCOLORS.
              04  M01N-VALUE   PIC X(20).
            03  M01N-FIXED1.
              04  M01N-B08-LABEL     PIC X(10).
              04  M01N-STATE-G.
                05  M01N-G11-G.
                  06  M01N-G11-VISIBLE     PIC X(01).
                  06  M01N-G11-STATE       PIC S9(09) BINARY.
                05  M01N-G12-G.
                  06  M01N-G12-VISIBLE     PIC X(01).
                  06  M01N-G12-STATE       PIC S9(09) BINARY.
                05  M01N-G13-G.
                  06  M01N-G13-VISIBLE     PIC X(01).
                  06  M01N-G13-STATE       PIC S9(09) BINARY.
                05  M01N-G14-G.
                  06  M01N-G14-VISIBLE     PIC X(01).
                  06  M01N-G14-STATE       PIC S9(09) BINARY.
                05  M01N-G21-G.
                  06  M01N-G21-VISIBLE     PIC X(01).
                  06  M01N-G21-STATE       PIC S9(09) BINARY.
                05  M01N-G22-G.
                  06  M01N-G22-VISIBLE     PIC X(01).
                  06  M01N-G22-STATE       PIC S9(09) BINARY.
                05  M01N-G23-G.
                  06  M01N-G23-VISIBLE     PIC X(01).
                  06  M01N-G23-STATE       PIC S9(09) BINARY.
                05  M01N-G24-G.
                  06  M01N-G24-VISIBLE     PIC X(01).
                  06  M01N-G24-STATE       PIC S9(09) BINARY.
                05  M01N-G31-G.
                  06  M01N-G31-VISIBLE     PIC X(01).
                  06  M01N-G31-STATE       PIC S9(09) BINARY.
                05  M01N-G32-G.
                  06  M01N-G32-VISIBLE     PIC X(01).
                  06  M01N-G32-STATE       PIC S9(09) BINARY.
                05  M01N-G33-G.
                  06  M01N-G33-VISIBLE     PIC X(01).
                  06  M01N-G33-STATE       PIC S9(09) BINARY.
                05  M01N-G34-G.
                  06  M01N-G34-VISIBLE     PIC X(01).
                  06  M01N-G34-STATE       PIC S9(09) BINARY.
                05  M01N-G35-G.
                  06  M01N-G35-VISIBLE     PIC X(01).
                  06  M01N-G35-STATE       PIC S9(09) BINARY.
                05  M01N-G36-G.
                  06  M01N-G36-VISIBLE     PIC X(01).
                  06  M01N-G36-STATE       PIC S9(09) BINARY.
                05  M01N-G41-G.
                  06  M01N-G41-VISIBLE     PIC X(01).
                  06  M01N-G41-STATE       PIC S9(09) BINARY.
                05  M01N-G42-G.
                  06  M01N-G42-VISIBLE     PIC X(01).
                  06  M01N-G42-STATE       PIC S9(09) BINARY.
                05  M01N-G43-G.
                  06  M01N-G43-VISIBLE     PIC X(01).
                  06  M01N-G43-STATE       PIC S9(09) BINARY.
                05  M01N-G44-G.
                  06  M01N-G44-VISIBLE     PIC X(01).
                  06  M01N-G44-STATE       PIC S9(09) BINARY.
                05  M01N-G51-G.
                  06  M01N-G51-VISIBLE     PIC X(01).
                  06  M01N-G51-STATE       PIC S9(09) BINARY.
                05  M01N-G52-G.
                  06  M01N-G52-VISIBLE     PIC X(01).
                  06  M01N-G52-STATE       PIC S9(09) BINARY.
                05  M01N-G71-G.
                  06  M01N-G71-VISIBLE     PIC X(01).
                  06  M01N-G71-STATE       PIC S9(09) BINARY.
                05  M01N-G82-G.
                  06  M01N-G82-VISIBLE     PIC X(01).
                  06  M01N-G82-STATE       PIC S9(09) BINARY.
                05  M01N-G91-G.
                  06  M01N-G91-VISIBLE     PIC X(01).
                  06  M01N-G91-STATE       PIC S9(09) BINARY.
                05  M01N-G92-G.
                  06  M01N-G92-VISIBLE     PIC X(01).
                  06  M01N-G92-STATE       PIC S9(09) BINARY.
              04  M01N-STATE-R       REDEFINES  M01N-STATE-G.
                05  M01N-STATE-OCC   OCCURS     24.
                  06  M01N-VISIBLE-O      PIC X(01).
                  06  M01N-STATE-O        PIC S9(09) BINARY.
              04  M01N-HTML1-G.
                05  M01N-HTML1         PIC X(200).
              04  M01N-FR6-VISIBLE     PIC X(1).
              04  M01N-SELNUM  PIC 9(2).
              04  M01N-HOSPID-G.
                05  M01N-HOSPID-STYLE  PIC X(20).
                05  M01N-HOSPID        PIC X(24).
              04  M01N-CONTRACTMSG-G.
                05  M01N-CONTRACTMSG-STYLE PIC X(20).
                05  M01N-CONTRACTMSG       PIC X(10).
