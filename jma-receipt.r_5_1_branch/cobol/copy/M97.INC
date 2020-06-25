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
        02  M97.
            03  M97-FIXED1.
              04  M97-ONKBN             PIC X.
              04  M97-OFFKBN.
                05  M97-OFFKBN-STATE    PIC S9(9)   BINARY.
                05  M97-OFFKBN-NAIYO    PIC X(1).
              04  M97-OFFCOMBO.
                05  M97-OFF-NAIYO       PIC X(14).
                05  M97-OFF-STATE       PIC S9(9)   BINARY.
                05  M97-OFF-COUNT       PIC S9(9)   BINARY.
                05  M97-OFF-LIST        PIC X(14)   OCCURS  2 TIMES.
              04  M97-SCROLLEDWINDOW1.
                05  M97-NAIYOLIST.
                  06  M97-NAIYO-COUNT   PIC S9(9)   BINARY.
                  06  M97-NAIYO-ROW     PIC S9(9)   BINARY.
                  06  M97-NAIYO-ROWATTR PIC S9(9)   BINARY.
                  06  M97-NAIYO-LIST                OCCURS  99 TIMES.
                    07  M97-NAIYO-0     PIC X(9).
                    07  M97-NAIYO-1     PIC X(8).
                    07  M97-NAIYO-2     PIC X(100).
                  06  M97-NAIYO-SELECT  PIC X       OCCURS  99 TIMES.
              04  M97-MESSEGE.
                05  M97-MSG-STYLE       PIC X(20).
                05  M97-MSG-NAIYO       PIC X(122).
              04 M97-DURATION   PIC S9(09) BINARY.
              04  M97-CONTRACTMSG-G.
                05  M97-CONTRACTMSG-STYLE PIC X(20).
                05  M97-CONTRACTMSG       PIC X(10).
