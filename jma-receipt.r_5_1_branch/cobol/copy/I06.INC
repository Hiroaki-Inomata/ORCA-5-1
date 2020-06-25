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
        02  I06.
      *     02  I06-FIXED6.
              03  I06-B06-STATE                PIC S9(9)   BINARY.
              03  I06-B06S-STATE               PIC S9(9)   BINARY.
              03  I06-B07-STATE                PIC S9(9)   BINARY.
              03  I06-B07S-STATE               PIC S9(9)   BINARY.
              03  I06-B12-STATE                PIC S9(9)   BINARY.
              03  I06-SELNUM                   PIC S9(4).
              03  I06-SELUKEYMD                PIC X(10).
              03  I06-SELIDOYMD                PIC X(10).
              03  I06-SELPTNUM                 PIC X(20).
              03  I06-SELNAME-STYLE            PIC X(20).
              03  I06-SELNAME                  PIC X(50).
              03  I06-CMB-SELSTS-G.
                04  I06-SELSTS                 PIC X(22).
                04  I06-SELSTS-COUNT           PIC S9(9)   BINARY.
                04  I06-SELSTS-ITEM            PIC X(22)   OCCURS  5.
              03  I06-CMB-SELSORT-G.
                04  I06-SELSORT                PIC X(53).
                04  I06-SELSORT-COUNT          PIC S9(9)   BINARY.
                04  I06-SELSORT-ITEM           PIC X(53)   OCCURS  10.
              03  I06-PTNUM                    PIC X(20).
              03  I06-NAME                     PIC X(100).
              03  I06-SEX                      PIC X(4).
              03  I06-BIRTHDAY                 PIC X(9).
              03  I06-AGE                      PIC X(9).
              03  I06-HSRYKA                   PIC X(10).
              03  I06-HHKNCOMBI                PIC X(46).
              03  I06-IDOJIYU                  PIC X(30).
              03  I06-HNYUINYMD                PIC X(10).
              03  I06-LBL-LASTIDOYMD           PIC X(12).
              03  I06-LASTIDOYMD               PIC X(10).
              03  I06-BRMNUM-G.
                04  I06-BRMNUM                 PIC X(6).
                04  I06-BRMNUM-STATE           PIC S9(9)   BINARY.
              03  I06-CMB-BTUNAME.
                04  I06-BTUNAME                PIC X(23).
                04  I06-BTUNAME-STATE          PIC S9(9)   BINARY.
                04  I06-BTUNAME-COUNT          PIC S9(9)   BINARY.
                04  I06-BTUNAME-ITEM           PIC X(23)   OCCURS  50.
                04  I06-BTUNAME-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-CMB-SAGAKU.
                04  I06-SAGAKU                 PIC X(23).
                04  I06-SAGAKU-STATE           PIC S9(9)   BINARY.
                04  I06-SAGAKU-COUNT           PIC S9(9)   BINARY.
                04  I06-SAGAKU-ITEM            PIC X(23)   OCCURS  99.
                04  I06-SAGAKU-ITEM-STATE      PIC S9(9)   BINARY.
              03  I06-CMB-SENTEIKBN.
                04  I06-SENTEIKBN              PIC X(24).
                04  I06-SENTEIKBN-STATE        PIC S9(9)   BINARY.
                04  I06-SENTEIKBN-COUNT        PIC S9(9)   BINARY.
                04  I06-SENTEIKBN-ITEM         PIC X(24)   OCCURS  10.
                04  I06-SENTEIKBN-ITEM-STATE   PIC S9(9)   BINARY.
              03  I06-NYUINYMD-G.
                04  I06-NYUINYMD                PIC X(10).
                04  I06-NYUINYMD-STATE          PIC S9(9)  BINARY.
              03  I06-CMB-NYUINKA.
                04  I06-NYUINKA                PIC X(27).
                04  I06-NYUINKA-STATE          PIC S9(9)   BINARY.
                04  I06-NYUINKA-COUNT          PIC S9(9)   BINARY.
                04  I06-NYUINKA-ITEM           PIC X(27)   OCCURS  99.
                04  I06-NYUINKA-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-CMB-SHOKAI.
                04  I06-SHOKAI                 PIC X(8).
                04  I06-SHOKAI-STATE           PIC S9(9)   BINARY.
                04  I06-SHOKAI-COUNT           PIC S9(9)   BINARY.
                04  I06-SHOKAI-ITEM            PIC X(8)    OCCURS  5.
                04  I06-SHOKAI-ITEM-STATE      PIC S9(9)   BINARY.
              03  I06-SHONUM-G.
                04  I06-SHONUM                 PIC S9(2).
                04  I06-SHONUM-STATE           PIC S9(9)   BINARY.
              03  I06-LBL-SHONYUINYMD          PIC X(16).
              03  I06-CMB-KAIGOKBN.
                04  I06-KAIGOKBN               PIC X(18).
                04  I06-KAIGOKBN-STATE         PIC S9(9)   BINARY.
                04  I06-KAIGOKBN-COUNT         PIC S9(9)   BINARY.
                04  I06-KAIGOKBN-ITEM          PIC X(18)   OCCURS  5.
                04  I06-KAIGOKBN-ITEM-STATE    PIC S9(9)   BINARY.
              03  I06-TAIINYMD-G.
                04  I06-TAIINYMD               PIC X(10).
                04  I06-TAIINYMD-STATE         PIC S9(9)   BINARY.
              03  I06-CMB-TAIINCD.
                04  I06-TAIINCD                PIC X(23).
                04  I06-TAIINCD-STATE          PIC S9(9)   BINARY.
                04  I06-TAIINCD-COUNT          PIC S9(9)   BINARY.
                04  I06-TAIINCD-ITEM           PIC X(23)   OCCURS  50.
                04  I06-TAIINCD-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-CMB-DR-G.
                04  I06-CMB-DR-OCC             OCCURS  3.
                  05  I06-DR                   PIC X(29).
                  05  I06-DR-STATE             PIC S9(9)   BINARY.
                  05  I06-DR-COUNT             PIC S9(9)   BINARY.
                  05  I06-DR-ITEM              PIC X(29)   OCCURS  99.
                  05  I06-DR-ITEM-STATE        PIC S9(9)   BINARY.
              03  I06-CMB-HKNCOMBI.
                04  I06-HKNCOMBI               PIC X(51).
                04  I06-HKNCOMBI-STATE         PIC S9(9)   BINARY.
                04  I06-HKNCOMBI-COUNT         PIC S9(9)   BINARY.
                04  I06-HKNCOMBI-ITEM          PIC X(51)   OCCURS  30.
                04  I06-HKNCOMBI-ITEM-STATE    PIC S9(9)   BINARY.
              03  I06-LBL-TOKNYUIN             PIC X(10).
              03  I06-CMB-TOKNYUIN.
                04  I06-TOKNYUIN               PIC X(63).
                04  I06-TOKNYUIN-STATE         PIC S9(9)   BINARY.
                04  I06-TOKNYUIN-COUNT         PIC S9(9)   BINARY.
                04  I06-TOKNYUIN-ITEM          PIC X(63)   OCCURS  100.
                04  I06-TOKNYUIN-ITEM-STATE    PIC S9(9)   BINARY.
              03  I06-CMB-TOKHIGAITOKBN.
                04  I06-TOKHIGAITOKBN            PIC X(30).
                04  I06-TOKHIGAITOKBN-STATE      PIC S9(9)   BINARY.
                04  I06-TOKHIGAITOKBN-COUNT      PIC S9(9)   BINARY.
                04  I06-TOKHIGAITOKBN-ITEM       PIC X(30)   OCCURS  5.
                04  I06-TOKHIGAITOKBN-ITEM-STATE PIC S9(9)   BINARY.
              03  I06-SNTNYUIN-G.
                04  I06-SNTNYUIN               PIC X(60).
                04  I06-SNTNYUIN-STATE         PIC S9(9)   BINARY.
              03  I06-CMB-NACCTKBN.
                04  I06-NACCTKBN               PIC X(30).
                04  I06-NACCTKBN-STATE         PIC S9(9)   BINARY.
                04  I06-NACCTKBN-COUNT         PIC S9(9)   BINARY.
                04  I06-NACCTKBN-ITEM          PIC X(30)   OCCURS  5.
                04  I06-NACCTKBN-ITEM-STATE    PIC S9(9)   BINARY.
              03  I06-KSNCD-G.
                04  I06-KSNCD-OCC              OCCURS  3.
                  05  I06-KSNCD                PIC X(73).
                  05  I06-KSNCD-STATE          PIC S9(9)   BINARY.
                  05  I06-KSNCD-COUNT          PIC S9(9)   BINARY.
                  05  I06-KSNCD-ITEM           PIC X(73)   OCCURS  20.
                  05  I06-KSNCD-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-CMB-TEIKIKBN.
                04  I06-TEIKIKBN               PIC X(24).
                04  I06-TEIKIKBN-STATE         PIC S9(9)   BINARY.
                04  I06-TEIKIKBN-COUNT         PIC S9(9)   BINARY.
                04  I06-TEIKIKBN-ITEM          PIC X(24)   OCCURS  5.
                04  I06-TEIKIKBN-ITEM-STATE    PIC S9(9)   BINARY.
              03  I06-CMB-DISPKBN.
                04  I06-DISPKBN                PIC X(11).
                04  I06-DISPKBN-STATE          PIC S9(9)   BINARY.
                04  I06-DISPKBN-COUNT          PIC S9(9)   BINARY.
                04  I06-DISPKBN-ITEM           PIC X(11)   OCCURS  5.
                04  I06-DISPKBN-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-CMB-SKJTIME.
                04  I06-SKJTIME                PIC X(10).
                04  I06-SKJTIME-STATE          PIC S9(9)   BINARY.
                04  I06-SKJTIME-COUNT          PIC S9(9)   BINARY.
                04  I06-SKJTIME-ITEM           PIC X(10)   OCCURS  5.
                04  I06-SKJTIME-ITEM-STATE     PIC S9(9)   BINARY.
              03  I06-NISSU-G.
                04  I06-NISSU                  PIC X(100).
                04  I06-NISSU-STATE            PIC S9(9)   BINARY.
              03  I06-CMB-BUNBENKBN.
                04  I06-BUNBENKBN                 PIC X(20).
                04  I06-BUNBENKBN-STATE           PIC S9(9)   BINARY.
                04  I06-BUNBENKBN-COUNT           PIC S9(9)   BINARY.
                04  I06-BUNBENKBN-ITEM            PIC X(20)
                                                  OCCURS  5.
                04  I06-BUNBENKBN-ITEM-STATE      PIC S9(9)   BINARY.
              03  I06-CMB-RIYOUKBN.
                04  I06-RIYOUKBN                  PIC X(20).
                04  I06-RIYOUKBN-STATE            PIC S9(9)   BINARY.
                04  I06-RIYOUKBN-COUNT            PIC S9(9)   BINARY.
                04  I06-RIYOUKBN-ITEM             PIC X(20)
                                                  OCCURS  5.
                04  I06-RIYOUKBN-ITEM-STATE       PIC S9(9)   BINARY.
              03  I06-SCROLLEDWINDOW4.
                04  I06-ERRMSGLST.
                  05  I06-ERRMSGLST-COUNT      PIC S9(9)   BINARY.
                  05  I06-ERRMSGLST-ROW        PIC S9(9)   BINARY.
                  05  I06-ERRMSGLST-ROWATTR    PIC S9(9)   BINARY.
                  05  I06-ERRMSGLST-ITEM       OCCURS  20 TIMES.
                    06  I06-TERRCD             PIC X(4).
                    06  I06-TERRMSG            PIC X(100).
                  05  I06-ERRMSGLST-SELECT     PIC X       OCCURS  20.
              03  I06-SCROLLEDWINDOW1.
                04  I06-ALLIDOLST.
                  05  I06-ALLIDOLST-COUNT      PIC S9(9)   BINARY.
                  05  I06-ALLIDOLST-ROW        PIC S9(9)   BINARY.
                  05  I06-ALLIDOLST-ROWATTR    PIC S9(9)   BINARY.
                  05  I06-ALLIDOLST-ITEM       OCCURS  100 TIMES.
                    06  I06-TALLNUM            PIC X(4).
                    06  I06-TALLIDOJIYU        PIC X(20).
                    06  I06-TALLIDOYMD         PIC X(10).
                    06  I06-TALLPTNUM          PIC X(20).
                    06  I06-TALLNAME           PIC X(50).
                    06  I06-TALLERR            PIC X(10).
                    06  I06-TALLERRCD          PIC X(04).
                    06  I06-TALLUKEYMD-G.
                      07   I06-TALLUKEYMD      PIC X(10).
                      07   I06-TALLUKEHMS      PIC X(05).
                  05  I06-ALLIDOLST-SELECT     PIC X       OCCURS  100.
              03  I06-SCROLLEDWINDOW3.
                04  I06-PTIDOLST-VISIBLE      PIC X.
                04  I06-PTIDOLST.
                  05  I06-PTIDOLST-COUNT      PIC S9(9)   BINARY.
                  05  I06-PTIDOLST-ROW        PIC S9(9)   BINARY.
                  05  I06-PTIDOLST-ROWATTR    PIC S9(9)   BINARY.
                  05  I06-PTIDOLST-ITEM       OCCURS  50 TIMES.
                    06  I06-TPTNUM            PIC X(4).
                    06  I06-TPTIDOYMD         PIC X(10).
                    06  I06-TPTIDOJIYU        PIC X(20).
                    06  I06-TPTERR            PIC X(10).
                    06  I06-TPTUKEYMD-G.
                        07  I06-TPTUKEYMD     PIC X(10).
                        07  I06-TPTUKEHMS     PIC X(5).
                  05  I06-PTIDOLST-SELECT     PIC X       OCCURS  50.
