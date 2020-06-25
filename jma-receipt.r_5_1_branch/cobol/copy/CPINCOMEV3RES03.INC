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
      ******************************************************************
      *    ＡＰＩレスポンス収納情報     COPY (CPINCOMEV3RES03.INC)
      ******************************************************************
        01  INCOMERES03-AREA.
            03  INCOMERES03-CONTEXT                PIC S9(9) BINARY.
            03  INCOMERES03-OBJECT                 PIC X(36).
            03  INCOMERES03-MODE                   PIC S9(9) BINARY.
            03  INCOMERES03-RECORDNAME             PIC X(128).
            03  INCOMERES03-DATA-AREA.
              05  INCOMERES03-INFORMATION-DATE     PIC X(10).
              05  INCOMERES03-INFORMATION-TIME     PIC X(8).
              05  INCOMERES03-API-RESULT           PIC X(5).
              05  INCOMERES03-API-RESULT-MSG       PIC X(100).
              05  INCOMERES03-REQNUM               PIC X(2).
              05  INCOMERES03-REQMODE              PIC X(2).
              05  INCOMERES03-RESNUM               PIC X(2).
              05  INCOMERES03-KARTE-UID            PIC X(36).
              05  INCOMERES03-ORCA-UID             PIC X(36).
              05  INCOMERES03-PTNUM                PIC X(20).
              05  INCOMERES03-NYUGAIKBN            PIC X(1).
              05  INCOMERES03-DENPNUM              PIC X(7).
              05  INCOMERES03-SEIKYU               PIC X(10).
              05  INCOMERES03-NYUKIN               PIC X(10).
              05  INCOMERES03-MISYU                PIC X(10).
              05  INCOMERES03-JOTAIKBN             PIC X(01).
              05  INCOMERES03-JOTAINAME            PIC X(10).
              05  INCOMERES03-SMEI-OCC           OCCURS  99.
                  07  INCOMERES03-TDENPEDANUM      PIC X(2).
                  07  INCOMERES03-TNYUHENYMD       PIC X(10).
                  07  INCOMERES03-TNYUHENTIME      PIC X(5).
                  07  INCOMERES03-TCHOSEI1         PIC X(10).
                  07  INCOMERES03-TCHOSEI2         PIC X(10).
                  07  INCOMERES03-TSKYMONEY        PIC X(10).
                  07  INCOMERES03-TNYUKIN          PIC X(10).
                  07  INCOMERES03-TJOTAI           PIC X(2).
                  07  INCOMERES03-TJOTAI-NAME      PIC X(10).
                  07  INCOMERES03-TNYUKIN-HOHO     PIC X(2).
                  07  INCOMERES03-TNYUKIN-HOHO-NAME PIC X(20).
              05  INCOMERES03-DATA-ID-AREA.
                  07 INCOMERES03-DATA-ID           PIC X(37).
