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
        02  W24.
            03  W24-FIXED6.
              04  W24-B03.
                05  W24-B03-STATE              PIC S9(9)
                                               BINARY.
              04  W24-STYUKYMD                 PIC X(10).
              04  W24-EDYUKYMD                 PIC X(10).
              04  W24-FRAME1.
                05  W24-FRAME1-LABEL           PIC X(20).
                05  W24-FIXED7.
                  06  W24-CMB-SKJRYOYO.
                    07  W24-SKJRYOYO           PIC X(28).
                    07  W24-SKJRYOYO-CNT       PIC S9(9)
                                               BINARY.
                    07  W24-SKJRYOYO-ITM       PIC X(28)
                                               OCCURS  5.
                  06  W24-LBLSYOKUJI           PIC X(20)
                                               OCCURS 2.
                  06  W24-CMB-SYOKUJI-OCC      OCCURS 2.
                    07  W24-SYOKUJI            PIC X(28).
                    07  W24-SYOKUJI-STATE      PIC S9(9)
                                               BINARY.
                    07  W24-SYOKUJI-CNT        PIC S9(9)
                                               BINARY.
                    07  W24-SYOKUJI-ITM        PIC X(28)
                                               OCCURS  5.
                    07  W24-SYOKUJI-ITM-STATE  PIC S9(9)
                                               BINARY.
                  06  W24-LBLEIYOUKSN          PIC X(20).
                  06  W24-CMB-EIYOUKSN.
                    07  W24-EIYOUKSN               PIC X(28).
                    07  W24-EIYOUKSN-STATE         PIC S9(9)
                                                   BINARY.
                    07  W24-EIYOUKSN-CNT           PIC S9(9)
                                                   BINARY.
                    07  W24-EIYOUKSN-ITM           PIC X(28)
                                                   OCCURS  5.
                    07  W24-EIYOUKSN-ITM-STATE     PIC S9(9)
                                                   BINARY.
              04  W24-CMB-BRMSGK-TAX.
                05  W24-BRMSGK-TAX             PIC X(10).
                05  W24-BRMSGK-TAX-CNT         PIC S9(9)
                                               BINARY.
                05  W24-BRMSGK-TAX-ITM         PIC X(10)
                                               OCCURS  5.
              04  W24-CMB-NACCT-BRMSGK.
                05  W24-NACCT-BRMSGK           PIC X(16).
                05  W24-NACCT-BRMSGK-CNT       PIC S9(9)
                                               BINARY.
                05  W24-NACCT-BRMSGK-ITM       PIC X(16)
                                               OCCURS  5.
              04  W24-CMB-NACCT-GAIHAKU.
                05  W24-NACCT-GAIHAKU          PIC X(14).
                05  W24-NACCT-GAIHAKU-CNT      PIC S9(9)
                                               BINARY.
                05  W24-NACCT-GAIHAKU-ITM      PIC X(14)
                                               OCCURS  5.
              04  W24-NOTEBOOK-AREA.
                05  W24-NOTEBOOK-PAGENO        PIC S9(9)   BINARY.
                05  W24-FIXED9.
                  06 W24-CHK-NYUKSN-OCC        OCCURS 30.
                    07  W24-CHK-NYUKSN         PIC X(01).
                  06  W24-NYUKSNMEI            PIC X(50)
                                               OCCURS 30.
                  06  W24-CMB-CHIIKIKSN.
                    07  W24-CHIIKIKSN          PIC X(28).
                    07  W24-CHIIKIKSN-CNT      PIC S9(9)
                                               BINARY.
                    07  W24-CHIIKIKSN-ITM      PIC X(28)
                                               OCCURS  7.
                05  W24-LBLNYUKSN              PIC X(20).
                05  W24-FIXED10.
                  06  W24-SRYCD                PIC X(09)
                                               OCCURS 10.
                  06  W24-SRYACT               PIC X(30)
                                               OCCURS 10.
              04  W24-FRAME5.
                05  W24-FRAME5-LABEL           PIC X(20).
                05  W24-FIXED11.
                  06  W24-CMB-SENTEIKBN.
                    07  W24-SENTEIKBN          PIC X(28).
                    07  W24-SENTEIKBN-CNT      PIC S9(9)
                                               BINARY.
                    07  W24-SENTEIKBN-ITM      PIC X(28)
                                               OCCURS  5 TIMES.
                  06  W24-CMB-SENTEI-FTNKBN.
                    07  W24-SENTEI-FTNKBN      PIC X(22).
                    07  W24-SENTEI-FTNKBN-CNT  PIC S9(9)
                                               BINARY.
                    07  W24-SENTEI-FTNKBN-ITM  PIC X(22)
                                               OCCURS  10 TIMES.
                  06  W24-SENTEI-TANKA         PIC S9(4).
                  06  W24-SENTEI-TANKAIP       PIC S9(4).
                  06  W24-SENTEI-TANKAIP-STATE PIC S9(9)
                                               BINARY.
                  06  W24-SENTEI-TANKARJ       PIC S9(4).
                  06  W24-SENTEI-TANKARJ-STATE PIC S9(9)
                                               BINARY.
                  06  W24-CMB-SENTEI-TAX.
                    07  W24-SENTEI-TAX         PIC X(20).
                    07  W24-SENTEI-TAX-CNT     PIC S9(9)
                                               BINARY.
                    07  W24-SENTEI-TAX-ITM     PIC X(20)
                                               OCCURS  5 TIMES.
                  06  W24-LBL-SENTEI-TANKA     PIC X(10).
                  06  W24-LBL-SENTEI-EN        PIC X(2).
                  06  W24-LBL-SENTEI-TANKAIP   PIC X(8).
                  06  W24-LBL-SENTEI-ENIP      PIC X(2).
                  06  W24-LBL-SENTEI-TANKARJ   PIC X(8).
                  06  W24-LBL-SENTEI-ENRJ      PIC X(2).
              04  W24-CMB-REKI-DISPKBN.
                05  W24-REKI-DISPKBN           PIC X(24).
                05  W24-REKI-DISPKBN-CNT       PIC S9(9)
                                               BINARY.
                05  W24-REKI-DISPKBN-ITM       PIC X(24)
                                               OCCURS  5.
              04  W24-CMB-SAIKEISANKBN.
                05  W24-SAIKEISANKBN           PIC X(20).
                05  W24-SAIKEISANKBN-CNT       PIC S9(9)
                                               BINARY.
                05  W24-SAIKEISANKBN-ITM       PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-NYKNKNRKBN.
                05  W24-NYKNKNRKBN             PIC X(40).
                05  W24-NYKNKNRKBN-CNT         PIC S9(9)
                                               BINARY.
                05  W24-NYKNKNRKBN-ITM         PIC X(40)
                                               OCCURS  10.
              04  W24-CMB-SKYPRTKBN.
                05  W24-SKYPRTKBN              PIC X(30).
                05  W24-SKYPRTKBN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-SKYPRTKBN-ITM          PIC X(30)
                                               OCCURS  5.
              04  W24-CMB-SKYSUMKBN.
                05  W24-SKYSUMKBN              PIC X(30).
                05  W24-SKYSUMKBN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-SKYSUMKBN-ITM          PIC X(30)
                                               OCCURS  5.
              04  W24-CMB-SKYMISYUPRTKBN.
                05  W24-SKYMISYUPRTKBN         PIC X(20).
                05  W24-SKYMISYUPRTKBN-CNT     PIC S9(9)
                                               BINARY.
                05  W24-SKYMISYUPRTKBN-ITM     PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-SRYPRTKBN.
                05  W24-SRYPRTKBN              PIC X(20).
                05  W24-SRYPRTKBN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-SRYPRTKBN-ITM          PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-TAIPRTKBN.
                05  W24-TAIPRTKBN              PIC X(20).
                05  W24-TAIPRTKBN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-TAIPRTKBN-ITM          PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-TAIKSIKBN.
                05  W24-TAIKSIKBN              PIC X(30).
                05  W24-TAIKSIKBN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-TAIKSIKBN-ITM          PIC X(30)
                                               OCCURS  10.
              04  W24-CMB-HYOUKETU.
                05  W24-HYOUKETU               PIC X(32).
                05  W24-HYOUKETU-STATE         PIC S9(9)
                                               BINARY.
                05  W24-HYOUKETU-CNT           PIC S9(9)
                                               BINARY.
                05  W24-HYOUKETU-ITM           PIC X(32)
                                               OCCURS  5.
                05  W24-HYOUKETU-ITM-STATE     PIC S9(9)
                                               BINARY.
              04  W24-CMB-TEISU.
                05  W24-TEISU                  PIC X(32).
                05  W24-TEISU-STATE            PIC S9(9)
                                               BINARY.
                05  W24-TEISU-CNT              PIC S9(9)
                                               BINARY.
                05  W24-TEISU-ITM              PIC X(32)
                                               OCCURS  2.
                05  W24-TEISU-ITM-STATE        PIC S9(9)
                                               BINARY.
              04  W24-CMB-YKNCHOKA.
                05  W24-YKNCHOKA-VISIBLE       PIC X(1).
                05  W24-YKNCHOKA               PIC X(32).
                05  W24-YKNCHOKA-STATE         PIC S9(9)
                                               BINARY.
                05  W24-YKNCHOKA-CNT           PIC S9(9)
                                               BINARY.
                05  W24-YKNCHOKA-ITM           PIC X(32)
                                               OCCURS  2.
                05  W24-YKNCHOKA-ITM-STATE     PIC S9(9)
                                               BINARY.
              04  W24-CMB-ADLINPUT.
                05  W24-ADLINPUT               PIC X(10).
                05  W24-ADLINPUT-STATE         PIC S9(9)
                                               BINARY.
                05  W24-ADLINPUT-CNT           PIC S9(9)
                                               BINARY.
                05  W24-ADLINPUT-ITM           PIC X(10)
                                               OCCURS  5.
                05  W24-ADLINPUT-ITM-STATE     PIC S9(9)
                                               BINARY.
              04  W24-CMB-TEIKISAGAKUACT.
                05  W24-TEIKISAGAKUACT         PIC X(20).
                05  W24-TEIKISAGAKUACT-CNT     PIC S9(9)
                                               BINARY.
                05  W24-TEIKISAGAKUACT-ITM     PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-RYOYODSP.
                05  W24-RYOYODSP               PIC X(20).
                05  W24-RYOYODSP-STATE         PIC S9(9)
                                               BINARY.
                05  W24-RYOYODSP-CNT           PIC S9(9)
                                               BINARY.
                05  W24-RYOYODSP-ITM           PIC X(20)
                                               OCCURS  5.
                05  W24-RYOYODSP-ITM-STATE     PIC S9(9)
                                               BINARY.
              03  W24-CMB-SHOHOUKBN.
                04  W24-SHOHOUKBN.
                  05  W24-VALUE   PIC X(20).
                04  W24-SHOHOUKBN-COUNT   PIC S9(9)   BINARY.
                04  W24-SHOHOUKBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              04  W24-CMB-IPPANKSN.
                05  W24-IPPANKSN               PIC X(20).
                05  W24-IPPANKSN-STATE         PIC S9(9)
                                               BINARY.
                05  W24-IPPANKSN-CNT           PIC S9(9)
                                               BINARY.
                05  W24-IPPANKSN-ITM           PIC X(20)
                                               OCCURS  5.
                05  W24-IPPANKSN-ITM-STATE     PIC S9(9)
                                               BINARY.
              04  W24-CMB-KYUKYUKSN.
                05  W24-KYUKYUKSN              PIC X(20).
                05  W24-KYUKYUKSN-STATE        PIC S9(9)
                                               BINARY.
                05  W24-KYUKYUKSN-CNT          PIC S9(9)
                                               BINARY.
                05  W24-KYUKYUKSN-ITM          PIC X(20)
                                               OCCURS  5.
                05  W24-KYUKYUKSN-ITM-STATE    PIC S9(9)
                                               BINARY.
              04  W24-CMB-REKI-JITAKBN.
                05  W24-REKI-JITAKBN           PIC X(40).
                05  W24-REKI-JITAKBN-CNT       PIC S9(9)
                                               BINARY.
                05  W24-REKI-JITAKBN-ITM       PIC X(40)
                                               OCCURS  5.
              04  W24-CMB-REKI-TAIINKBN.
                05  W24-REKI-TAIINKBN       PIC X(20).
                05  W24-REKI-TAIINKBN-CNT   PIC S9(9)
                                               BINARY.
                05  W24-REKI-TAIINKBN-ITM   PIC X(20)
                                               OCCURS  5.
              04  W24-CMB-SKJ-CREATE.
                05  W24-SKJ-CREATE             PIC X(20).
                05  W24-SKJ-CREATE-CNT         PIC S9(9)
                                               BINARY.
                05  W24-SKJ-CREATE-ITM         PIC X(20)
                                               OCCURS  5.
