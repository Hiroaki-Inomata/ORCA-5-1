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
        02  W20.
            03  W20-FIXED6.
              04  W20-B02-STATE            PIC S9(9)   BINARY.
              04  W20-B05-STATE            PIC S9(9)   BINARY.
              04  W20-B06-STATE            PIC S9(9)   BINARY.
              04  W20-B07-STATE            PIC S9(9)   BINARY.
              04  W20-B08-STATE            PIC S9(9)   BINARY.
              04  W20-B09-G.
                05  W20-B09                PIC X(04).
                05  W20-B09-STATE          PIC S9(9)   BINARY.
              04  W20-B10-G.
                05  W20-B10                PIC X(10).
                05  W20-B10-STATE          PIC S9(9)   BINARY.
              04  W20-B12-STATE            PIC S9(9)   BINARY.
              04  W20-KJNYMD               PIC X(10).
              04  W20-BTUSELNUM            PIC S9(2).
              04  W20-BTUNUM-G.
                05  W20-BTUNUM             PIC S9(2).
                05  W20-BTUNUM-STATE       PIC S9(9)   BINARY.
      *
              04  W20-NAME-G.
                05  W20-NAME               PIC X(20).
                05  W20-NAME-STATE         PIC S9(9)   BINARY.
      *
              04  W20-TANNAME-G.
                05  W20-TANNAME            PIC X(10).
                05  W20-TANNAME-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-TOKUTEINYUIN.
      *
                05  W20-TOKUTEINYUIN       PIC X(43).
                05  W20-TOKUTEINYUIN-STATE
                                           PIC S9(9)   BINARY.
      *
                05  W20-TOKUTEINYUIN-CNT
                                           PIC S9(9)   BINARY.
                05  W20-TOKUTEINYUIN-ITM
                                           PIC X(43)   OCCURS  50.
                05  W20-TOKUTEINYUIN-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-CMB-SRYKA.
      *
                05  W20-SRYKA             PIC X(23).
                05  W20-SRYKA-STATE       PIC S9(9)   BINARY.
      *
                05  W20-SRYKA-CNT         PIC S9(9)   BINARY.
                05  W20-SRYKA-ITM         PIC X(23)   OCCURS  99.
                05  W20-SRYKA-ITM-STATE   PIC S9(9)   BINARY.
      *
              04  W20-CMB-SBT.
      *
                05  W20-SBT                PIC X(23).
                05  W20-SBT-STATE          PIC S9(9)   BINARY.
      *
                05  W20-SBT-CNT            PIC S9(9)   BINARY.
                05  W20-SBT-ITM            PIC X(23)   OCCURS  15.
                05  W20-SBT-ITM-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-KHNRYO.
      *
                05  W20-KHNRYO             PIC X(63).
                05  W20-KHNRYO-STATE       PIC S9(9)   BINARY.
                05  W20-KHNRYO-CNT         PIC S9(9)   BINARY.
                05  W20-KHNRYO-ITM         PIC X(63)   OCCURS  50.
                05  W20-KHNRYO-ITM-STATE   PIC S9(9)   BINARY.
      *
      *
              04  W20-KHNTEN-G.
                05  W20-KHNTEN               PIC S9(6).
                05  W20-KHNTEN-STATE         PIC S9(9)   BINARY.
      *
              04  W20-LBLYKNCHOKAKBN          PIC X(40).
      *
              04  W20-CMB-YKNCHOKAKBN.
                05  W20-YKNCHOKAKBN           PIC X(52).
                05  W20-YKNCHOKAKBN-STATE     PIC S9(9)   BINARY.
                05  W20-YKNCHOKAKBN-CNT       PIC S9(9)   BINARY.
                05  W20-YKNCHOKAKBN-ITM       PIC X(52)   OCCURS  10.
                05  W20-YKNCHOKAKBN-ITM-STATE PIC S9(9)   BINARY.
      *
              04  W20-CMB-RECEDSPKBN.
      *
                05  W20-RECEDSPKBN         PIC X(13).
                05  W20-RECEDSPKBN-STATE
                                           PIC S9(9)   BINARY.
      *
                05  W20-RECEDSPKBN-CNT     PIC S9(9)   BINARY.
                05  W20-RECEDSPKBN-ITM     PIC X(13)   OCCURS  10.
                05  W20-RECEDSPKBN-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-CMB-BTUTYPE.
                05  W20-BTUTYPE            PIC X(20).
                05  W20-BTUTYPE-STATE      PIC S9(9)   BINARY.
                05  W20-BTUTYPE-LST.
                  06  W20-BTUTYPE-CNT      PIC S9(9)   BINARY.
                  06  W20-BTUTYPE-ITM      PIC X(20)   OCCURS  5.
                  06  W20-BTUTYPE-ITM-STATE
                                           PIC S9(9)   BINARY.
      *
              04  W20-BTUCD                PIC S9(2).
              04  W20-BTUCD-STATE          PIC S9(9)   BINARY.
      *
              04  W20-CMB-TBTUTYPE.
                05  W20-TBTUTYPE            PIC X(20).
                05  W20-TBTUTYPE-STATE      PIC S9(9)   BINARY.
                05  W20-TBTUTYPE-LST.
                  06  W20-TBTUTYPE-CNT      PIC S9(9)   BINARY.
                  06  W20-TBTUTYPE-ITM      PIC X(20)   OCCURS  5.
                  06  W20-TBTUTYPE-ITM-STATE
                                            PIC S9(9)   BINARY.
      *
              04  W20-TBTUCD                PIC S9(2).
              04  W20-TBTUCD-STATE          PIC S9(9)   BINARY.
      *
              04  W20-LBLRYOYOKBN          PIC X(40).
      *
              04  W20-CMB-RYOYOKBN.
                05  W20-RYOYOKBN           PIC X(52).
                05  W20-RYOYOKBN-STATE     PIC S9(9)   BINARY.
                05  W20-RYOYOKBN-CNT       PIC S9(9)   BINARY.
                05  W20-RYOYOKBN-ITM       PIC X(52)   OCCURS  10.
                05  W20-RYOYOKBN-ITM-STATE PIC S9(9)   BINARY.
      *
              04  W20-SELNUM               PIC S9(2).
      *
      *
              04  W20-KSNAREA.
                05  W20-KSN-PAGENO             PIC S9(9)   BINARY.
                05  W20-KSN-P01-G.
                  06  W20-LBLKNGHJO            PIC X(36).
                  06  W20-CMB-KNGHJO.
                    07  W20-KNGHJO             PIC X(41).
                    07  W20-KNGHJO-STATE       PIC S9(9)   BINARY.
      *
                    07  W20-KNGHJO-CNT         PIC S9(9)   BINARY.
                    07  W20-KNGHJO-ITM         PIC X(41)   OCCURS  20.
                    07  W20-KNGHJO-ITM-STATE
                                               PIC S9(9)   BINARY.
                  06  W20-LBLKSN-P01-G.
                    07  W20-LBLKSN-P01         PIC X(36)
                                               OCCURS      14.
                  06  W20-CMB-KSN-P01-G.
                    07  W20-CMB-KSN-P01-OCC        OCCURS    14.
                      08  W20-KSN-P01              PIC X(30).
                      08  W20-KSN-P01-STATE        PIC S9(9) BINARY.
                      08  W20-KSN-P01-CNT          PIC S9(9) BINARY.
                      08  W20-KSN-P01-ITM          PIC X(30) OCCURS  10.
                      08  W20-KSN-P01-ITM-STATE    PIC S9(9) BINARY.
      *
                  06  W20-LBLKSN-P02-G.
                    07  W20-LBLKSN-P02             PIC X(36)
                                                   OCCURS      16.
                  06  W20-CMB-KSN-P02-G.
                    07  W20-CMB-KSN-P02-OCC        OCCURS    16.
                      08  W20-KSN-P02              PIC X(30).
                      08  W20-KSN-P02-STATE        PIC S9(9) BINARY.
                      08  W20-KSN-P02-CNT          PIC S9(9) BINARY.
                      08  W20-KSN-P02-ITM          PIC X(30) OCCURS  10.
                      08  W20-KSN-P02-ITM-STATE    PIC S9(9) BINARY.
                    07  W20-LBLKSNP02-STATE        PIC S9(9) BINARY.
      *
                  06  W20-LBLKSN-P03-G.
                    07  W20-LBLKSN-P03             PIC X(36)
                                                   OCCURS      16.
                  06  W20-CMB-KSN-P03-G.
                    07  W20-CMB-KSN-P03-OCC        OCCURS    16.
                      08  W20-KSN-P03              PIC X(30).
                      08  W20-KSN-P03-STATE        PIC S9(9) BINARY.
                      08  W20-KSN-P03-CNT          PIC S9(9) BINARY.
                      08  W20-KSN-P03-ITM          PIC X(30) OCCURS  10.
                      08  W20-KSN-P03-ITM-STATE    PIC S9(9) BINARY.
                    07  W20-LBLKSNP03-STATE        PIC S9(9) BINARY.
                  06  W20-YKTTYMD-G.
                    07  W20-YKTTYMD-OCC            OCCURS 12.
                      08  W20-YKTTYMD              PIC X(10).
                      08  W20-YKTTYMD-STATE        PIC S9(9) BINARY.
                    07  W20-LBLYKTTYMDP-STATE      PIC S9(9) BINARY.
              04  W20-YUKOSTYMD            PIC X(10).
              04  W20-YUKOSTYMD-STATE      PIC S9(9)   BINARY.
      *
              04  W20-YUKOEDYMD            PIC X(10).
              04  W20-YUKOEDYMD-STATE      PIC S9(9)   BINARY.
      *
              04  W20-CMB-EDKBN.
      *
                05  W20-EDKBN              PIC X(8).
                05  W20-EDKBN-STATE        PIC S9(9)   BINARY.
                05  W20-EDKBN-CNT          PIC S9(9)   BINARY.
                05  W20-EDKBN-ITM          PIC X(8)    OCCURS  2.
                05  W20-EDKBN-ITM-STATE    PIC S9(9)   BINARY.
      *
              04  W20-SCROLLEDWINDOW28.
                05  W20-BTULST-TBL.
                  06  W20-BTULST-CNT       PIC S9(9)   BINARY.
                  06  W20-BTULST-ROW       PIC S9(9)   BINARY.
                  06  W20-BTULST-ROWATTR   PIC S9(9)   BINARY.
                  06  W20-BTULST-OCC       OCCURS  50.
                    07  W20-TBTUNUM        PIC X(04).
                    07  W20-TNAME          PIC X(20).
                    07  W20-TSBT           PIC X(20).
                    07  W20-TTOKUTEINYUIN  PIC X(24).
                    07  W20-TSRYKA         PIC X(20).
                    07  W20-TKHNRYO        PIC X(30).
                    07  W20-TKHNTEN        PIC X(12).
                    07  W20-TNINZU         PIC X(10).
                    07  W20-TYUKOSTYMD     PIC X(09).
                    07  W20-TYUKOEDYMD     PIC X(09).
                  06  W20-BTULST-SEL       PIC X   OCCURS  50.
              04  W20-SCROLLEDWINDOW29.
                05  W20-RIREKILST-TBL.
                  06  W20-RIREKILST-CNT    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-ROW    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-ROWATTR    PIC S9(9)   BINARY.
                  06  W20-RIREKILST-OCC    OCCURS  20.
                    07  W20-TSELNUM        PIC X(04).
                    07  W20-TRIREKI        PIC X(22).
                  06  W20-RIREKILST-SEL    PIC X   OCCURS  20.
