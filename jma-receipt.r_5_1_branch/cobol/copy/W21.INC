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
        02  W21.
            03  W21-FIXED6.
              04  W21-B02-STATE            PIC S9(9)   BINARY.
              04  W21-B05-STATE            PIC S9(9)   BINARY.
              04  W21-B06-STATE            PIC S9(9)   BINARY.
              04  W21-B07-STATE            PIC S9(9)   BINARY.
              04  W21-B08-STATE            PIC S9(9)   BINARY.
              04  W21-B09                  PIC X(4).
              04  W21-B09-STATE            PIC S9(9)   BINARY.
              04  W21-B12-STATE            PIC S9(9)   BINARY.
              04  W21-CMB-BTU-NAME.
                05  W21-BTU-NAME           PIC X(23).
                05  W21-BTU-NAME-CNT       PIC S9(9)   BINARY.
                05  W21-BTU-NAME-ITM       PIC X(23)   OCCURS  50.
              04  W21-BTU-KHNRYO           PIC X(60).
              04  W21-KJNYMD               PIC X(10).
              04  W21-SELNUM1              PIC X(50).
              04  W21-BRM-NUM              PIC X(6).
              04  W21-BRM-NUM-STT          PIC S9(9)   BINARY.
              04  W21-CMB-BRM-SBT.
                05  W21-BRM-SBT            PIC X(23).
                05  W21-BRM-SBT-STT        PIC S9(9)   BINARY.
                05  W21-BRM-SBT-CNT        PIC S9(9)   BINARY.
                05  W21-BRM-SBT-ITM        PIC X(23)   OCCURS  15.
                05  W21-BRM-SBT-ITM-STT    PIC S9(9)   BINARY.
              04  W21-CMB-BRM-TOKUTEINYUIN.
                05  W21-BRM-TOKUTEINYUIN   PIC X(53).
                05  W21-BRM-TOKUTEINYUIN-STT
                                           PIC S9(9)   BINARY.
                05  W21-BRM-TOKUTEINYUIN-CNT
                                           PIC S9(9)   BINARY.
                05  W21-BRM-TOKUTEINYUIN-ITM
                                           PIC X(53)   OCCURS  50.
                05  W21-BRM-TOKUTEINYUIN-ITM-STT
                                           PIC S9(9)   BINARY.
              04  W21-BRM-NINZU            PIC S9(2).
              04  W21-BRM-NINZU-STT        PIC S9(9)   BINARY.
              04  W21-CMB-BRM-SAGAKU.
                05  W21-BRM-SAGAKU         PIC X(12).
                05  W21-BRM-SAGAKU-STT     PIC S9(9)   BINARY.
                05  W21-BRM-SAGAKU-CNT     PIC S9(9)   BINARY.
                05  W21-BRM-SAGAKU-ITM     PIC X(12)   OCCURS  99.
                05  W21-BRM-SAGAKU-ITM-STT PIC S9(9)   BINARY.
              04  W21-CMB-BRM-SEX.
                05  W21-BRM-SEX            PIC X(12).
                05  W21-BRM-SEX-STT        PIC S9(9)   BINARY.
                05  W21-BRM-SEX-CNT        PIC S9(9)   BINARY.
                05  W21-BRM-SEX-ITM        PIC X(12)   OCCURS  10.
                05  W21-BRM-SEX-ITM-STT    PIC S9(9)   BINARY.
              04  W21-BRM-NAITEL           PIC X(5).
              04  W21-BRM-NAITEL-STT       PIC S9(9)   BINARY.
              04  W21-CMB-BRM-KA.
                05  W21-BRM-KA             PIC X(23).
                05  W21-BRM-KA-STT         PIC S9(9)   BINARY.
                05  W21-BRM-KA-CNT         PIC S9(9)   BINARY.
                05  W21-BRM-KA-ITM         PIC X(23)   OCCURS  99.
                05  W21-BRM-KA-ITM-STT     PIC S9(9)   BINARY.
              04  W21-LBLKHNRYO1           PIC X(10).
              04  W21-LBLKHNRYO2           PIC X(02).
              04  W21-CMB-KHNRYO.
                05  W21-KHNRYO             PIC X(63).
                05  W21-KHNRYO-STT         PIC S9(9)   BINARY.
                05  W21-KHNRYO-CNT         PIC S9(9)   BINARY.
                05  W21-KHNRYO-ITM         PIC X(63)   OCCURS  20.
                05  W21-KHNRYO-ITM-STT     PIC S9(9)   BINARY.
              04  W21-KHNTEN               PIC S9(6).
              04  W21-KHNTEN-STT           PIC S9(9)   BINARY.
      *
              04  W21-CMB-TBTUTYPE.
                05  W21-TBTUTYPE            PIC X(20).
                05  W21-TBTUTYPE-STATE      PIC S9(9)   BINARY.
                05  W21-TBTUTYPE-CNT        PIC S9(9)   BINARY.
                05  W21-TBTUTYPE-ITM        PIC X(20)   OCCURS  5.
                05  W21-TBTUTYPE-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W21-TBTUCD                PIC S9(2).
              04  W21-TBTUCD-STATE          PIC S9(9)   BINARY.
      *
              04  W21-LBLKSN-G.
                05  W21-LBLKSN             PIC X(36)
                                           OCCURS      14.
      *
              04  W21-CMB-KSN-G.
                05  W21-CMB-KSN-OCC        OCCURS      14.
      *
                  06  W21-KSN              PIC X(40).
                  06  W21-KSN-STATE        PIC S9(9)   BINARY.
      *
                  06  W21-KSN-CNT          PIC S9(9)   BINARY.
                  06  W21-KSN-ITM          PIC X(40)   OCCURS  10.
                  06  W21-KSN-ITM-STATE    PIC S9(9)   BINARY.
              04  W21-YUKOSTYMD            PIC X(10).
              04  W21-YUKOSTYMD-STT        PIC S9(9)   BINARY.
              04  W21-YUKOEDYMD            PIC X(10).
              04  W21-YUKOEDYMD-STT        PIC S9(9)   BINARY.
              04  W21-SELNUM2              PIC S9(2).
              04  W21-SELNUM2-STT          PIC S9(9)   BINARY.
              04  W21-LBLMODE-STYLE        PIC X(20).
              04  W21-LBLMODE              PIC X(40).
              04  W21-LBLINFO-STYLE        PIC X(20).
              04  W21-LBLINFO              PIC X(100).
              04  W21-SCRW28.
                05  W21-BRMLST-TBL.
                  06  W21-TKHNRYO-TITLE    PIC X(20).
                  06  W21-BRMLST-CNT       PIC S9(9)   BINARY.
                  06  W21-BRMLST-ROW       PIC S9(9)   BINARY.
                  06  W21-BRMLST-ROWATTR   PIC S9(9)   BINARY.
                  06  W21-BRMLST-OCC       OCCURS  200.
                    07  W21-TSELNUM        PIC X(4).
                    07  W21-TBRM-NUM       PIC X(08).
                    07  W21-TBRM-SBT       PIC X(20).
                    07  W21-TBRM-TOKUTEINYUIN
                                           PIC X(24).
                    07  W21-TBRM-NINZU     PIC X(08).
                    07  W21-TBRM-SAGAKU    PIC X(08).
                    07  W21-TBRM-SEX       PIC X(10).
                    07  W21-TBRM-NAITEL    PIC X(5).
                    07  W21-TBRM-KA        PIC X(20).
                    07  W21-TKHNRYO        PIC X(20).
                    07  W21-TYUKOSTYMD     PIC X(9).
                    07  W21-TYUKOEDYMD     PIC X(9).
                  06  W21-BRMLST-SEL       PIC X   OCCURS  200 TIMES.
              04  W21-SCROLLEDWINDOW29.
                05  W21-RIREKILST-TBL.
                  06  W21-RIREKILST-CNT    PIC S9(9)   BINARY.
                  06  W21-RIREKILST-ROW    PIC S9(9)   BINARY.
                  06  W21-RIREKILST-ROWATTR    PIC S9(9)   BINARY.
                  06  W21-RIREKILST-OCC    OCCURS  20.
                    07  W21-TRSELNUM       PIC X(04).
                    07  W21-TRIREKI        PIC X(22).
                  06  W21-RIREKILST-SEL    PIC X   OCCURS  20.
