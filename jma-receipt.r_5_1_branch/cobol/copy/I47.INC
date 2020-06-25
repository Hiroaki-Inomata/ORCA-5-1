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
        02  I47.
          03  I47-B10-STATE     PIC S9(9)   BINARY.
          03  I47-B10S-STATE    PIC S9(9)   BINARY.  
          03  I47-LBLINFO-STYLE PIC X(20).
          03  I47-LBLINFO   PIC X(80).
          03  I47-LBLINFO2-STYLE PIC X(20).
          03  I47-LBLINFO2 PIC X(80).
          03  I47-PTNUM.
            04  I47-VALUE   PIC X(20).
          03  I47-SRYYM.
            04  I47-VALUE   PIC X(8).
          03  I47-KANANAME.
            04  I47-VALUE   PIC X(50).
          03  I47-NAME.
            04  I47-VALUE   PIC X(50).
          03  I47-BIRTHDAY.
            04  I47-VALUE   PIC X(9).
          03  I47-SEX.
            04  I47-VALUE   PIC X(2).
          03  I47-NYUINYMD.
            04  I47-VALUE   PIC X(9).
          03  I47-TAIINYMD.
            04  I47-VALUE   PIC X(9).
          03  I47-SELNUM.
            04  I47-VALUE   PIC X(50).
          03  I47-MEISYO.
            04  I47-VALUE   PIC X(200).
          03  I47-DAY-AREA-STATE PIC S9(9)   BINARY.
          03  I47-DAYALL.
            04  I47-VALUE   PIC X(2).
          03  I47-DAY-G.
            04  I47-DAY     PIC X(2)   OCCURS  31.
          03  I47-GRPINPUT.
            04  I47-VALUE   PIC X(50).
          03  I47-SCROLLEDWINDOW1.
            04  I47-RYOLIST.
              05  I47-RYOLIST-COUNT   PIC S9(9)   BINARY.
              05  I47-RYOLIST-ROW PIC S9(9)   BINARY.
              05  I47-RYOLIST-ROWATTR PIC S9(9)   BINARY.
              05  I47-RYOLIST-ITEM    OCCURS  100 TIMES.
                06  I47-TNUM    PIC X(3).
                06  I47-TMARK   PIC X(1).
                06  I47-TNAME   PIC X(100).
                06  I47-TDAYLMT PIC X(2).
                06  I47-TDAYALL PIC X(2).
                06  I47-TDAY-G.
                  07  I47-TDAY  PIC X(2) OCCURS 31.
              05  I47-RYOLIST-SELECT
                      PIC X       OCCURS  100 TIMES.
          03  I47-SCROLLEDWINDOW2.
            04  I47-NYULIST.
              05  I47-NYULIST-COUNT   PIC S9(9)   BINARY.
              05  I47-NYULIST-ROW PIC S9(9)   BINARY.
              05  I47-NYULIST-ROWATTR PIC S9(9)   BINARY.
              05  I47-NYULIST-NDAY-OCC       OCCURS 31.
                  07  I47-NYULIST-NDAY-STYLE PIC X(20).
                  07  I47-NYULIST-NDAY       PIC X(02).
              05  I47-NYULIST-ITEM    OCCURS  3 TIMES.
                06  I47-TNL-NUM        PIC X(2).
                06  I47-TNL-MARK       PIC X(1).
                06  I47-TNL-NAME       PIC X(100).
                06  I47-TNL-LMT        PIC X(2).
                06  I47-TNL-DAYALL     PIC X(2).
                06  I47-TNL-DAY-G.
                  07  I47-TNL-DAY      PIC X(2) OCCURS 31.
              05  I47-NYULIST-SELECT
                      PIC X       OCCURS  3 TIMES.
