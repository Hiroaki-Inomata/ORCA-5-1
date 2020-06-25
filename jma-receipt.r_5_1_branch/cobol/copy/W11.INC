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
        02  W11.
      *      02  W11-FIXED2.
              03  W11-B03.
                04  W11-B03-STATE              PIC S9(9)   BINARY.
              03  W11-B04.
                04  W11-B04-STATE              PIC S9(9)   BINARY.
                04  W11-B04-LABEL              PIC X(10).
              03  W11-B05.
                04  W11-B05-STATE              PIC S9(9)   BINARY.
                04  W11-B05-LABEL              PIC X(10).
              03  W11-B06.
                04  W11-B06-STATE              PIC S9(9)   BINARY.
                04  W11-B06-LABEL              PIC X(10).
              03  W11-B08.
                04  W11-B08-STATE              PIC S9(9)   BINARY.
                04  W11-B08-LABEL              PIC X(10).
              03  W11-B09.
                04  W11-B09-STATE              PIC S9(9)   BINARY.
                04  W11-B09-LABEL              PIC X(10).
              03  W11-B10.
                04  W11-B10-STATE              PIC S9(9)   BINARY.
                04  W11-B10-LABEL              PIC X(10).
              03  W11-NOTE3.
                04  W11-PAGENO                 PIC S9(9)   BINARY.
                04  W11-FI23.
                  05  W11-USERKBNCOMBO.
                    06  W11-USERKBN            PIC X(20).
                    06  W11-USERKBN-COUNT      PIC S9(9)   BINARY.
                    06  W11-USERKBNLST         PIC X(20)
                                               OCCURS  5 TIMES.
                  05  W11-YUKOKBNCOMBO.
                    06  W11-YUKOKBN            PIC X(20).
                    06  W11-YUKOKBN-STATE      PIC S9(9)   BINARY.
                    06  W11-YUKOKBN-COUNT      PIC S9(9)   BINARY.
                    06  W11-YUKOKBNLST         PIC X(20)
                                               OCCURS  2 TIMES.
                  05  W11-USERLSTSCROLLE.
                    06  W11-USERLST.
                      07  W11-USERLST-COUNT    PIC S9(9)   BINARY.
                      07  W11-USERLST-ROW      PIC S9(9)   BINARY.
                      07  W11-USERLST-ROWATTR  PIC S9(9)   BINARY.
                      07  W11-ITEM             OCCURS  100 TIMES.
                        08  W11-USERNUM        PIC X(4).
                        08  W11-USERNAME       PIC X(16).
                      07  W11-USERLST-SELECT   PIC X
                                               OCCURS  100 TIMES.
                  05  W11-SELNUM-G.
                    06  W11-SELNUM-STATE       PIC S9(9)   BINARY.
                    06  W11-SELNUM             PIC S9(3).
                  05  W11-NOTE4.
                    06  W11-NOTE4-PAGENO       PIC S9(9)   BINARY.
                    06  W11-FIXED3.
                      07  W11-DSPNAME-G        OCCURS  20.
                        08  W11-DSPNAME        PIC X(30).
                      07  W11-PRTNM-G          OCCURS  20.
                        08  W11-PRTNM          PIC X(16).
                      07  W11-ORCAORGNM01      PIC X(20).
                      07  W11-ORGNM02-COB.
                        08  W11-ORCAORGNM02.
                          09  W11-VALUE   PIC X(20).
                        08  W11-ORCAORGNM02-COUNT   PIC S9(9)   BINARY.
                        08  W11-ORCAORGNM02-ITEM
                                PIC X(20)   OCCURS  5 TIMES.
                      07  W11-ORCAORGNM03COMBO.
                        08  W11-ORCAORGNM03    PIC X(20).
                        08  W11-ORCAORGNM03-COUNT
                                               PIC S9(9)   BINARY.
                        08  W11-ORCAORGNM03LST PIC X(20)
                                               OCCURS  6 TIMES.
                      07  W11-ORGNM04-COB.
                        08  W11-ORCAORGNM04.
                          09  W11-VALUE   PIC X(20).
                        08  W11-ORCAORGNM04-COUNT   PIC S9(9)   BINARY.
                        08  W11-ORCAORGNM04-ITEM
                                PIC X(20)   OCCURS  5 TIMES.
                      07  W11-ORCAORGNM05      PIC X(20).
                      07  W11-ORCAORGNM06      PIC X(20).
                      07  W11-ORCAORGNM07      PIC X(20).
                      07  W11-ORCAORGNM08      PIC X(20).
                      07  W11-ORCAORGNM09      PIC X(20).
                      07  W11-ORCAORGNM10      PIC X(20).
                      07  W11-ORCAORGNM11      PIC X(20).
                      07  W11-ORCAORGNM12      PIC X(20).
                      07  W11-ORCAORGCHK-G     OCCURS   12.
                        08  W11-ORCAORGCHK     PIC X.
                      07  W11-ORCACSTNM-COB-G   OCCURS   12.
                        08  W11-ORCACSTNM-G.
                          09  W11-ORCACSTNM-STYLE PIC X(20).
                          09  W11-ORCACSTNM       PIC X(20).
                          09  W11-ORCACSTNM-COUNT PIC S9(9)   BINARY.
                          09  W11-ORCACSTNM-ITEM  PIC X(20)
                                                  OCCURS  10 TIMES.
                      07  W11-ORCACSTCHK-G     OCCURS   12.
                        08  W11-ORCACSTCHK     PIC X.
                      07  W11-JIHI-UCHIWAKE    PIC X(20).
                    06  W11-N0101-STATE        PIC S9(9)   BINARY.
                    06  W11-FIXED4.
                      07  W11-DSPNAME-N-G      OCCURS  20.
                        08  W11-DSPNAME-N      PIC X(30).
                      07  W11-PRTNM-N-G        OCCURS  20.
                        08  W11-PRTNM-N        PIC X(16).
                      07  W11-ORCAORGNM-N01    PIC X(20).
                      07  W11-ORCAORGNM-N02    PIC X(20).
                      07  W11-ORCAORGNM-N03COMBO.
                        08  W11-ORCAORGNM-N03  PIC X(20).
                        08  W11-ORCAORGNM-N03-COUNT  PIC S9(9)   BINARY.
                        08  W11-ORCAORGNM-N03LST PIC X(20)
                                               OCCURS  5 TIMES.
                      07  W11-ORCAORGNM-N04    PIC X(20).
                      07  W11-ORCAORGNM05N.
                        08  W11-VALUE   PIC X(20).
                      07  W11-ORCAORGNM06N.
                        08  W11-VALUE   PIC X(20).
                      07  W11-ORCAORGNM07N.
                        08  W11-VALUE   PIC X(20).
                      07  W11-ORCAORGNM08N.
                        08  W11-VALUE   PIC X(20).
                      07  W11-ORCAORGNM-N09COMBO.
                        08  W11-ORCAORGNM-N09  PIC X(20).
                        08  W11-ORCAORGNM-N09-COUNT  PIC S9(9)   BINARY.
                        08  W11-ORCAORGNM-N09LST PIC X(20)
                                               OCCURS  5 TIMES.
                      07  W11-ORCAORGNM10N.
                        08  W11-VALUE   PIC X(20).
                      07  W11-ORCAORGCHK-N-G   OCCURS   10.
                        08  W11-ORCAORGCHK-N   PIC X.
                      07  W11-ORCACSTNM-N-COB-G   OCCURS   10.
                        08  W11-ORCACSTNM-N-G.
                          09  W11-ORCACSTNM-N-STYLE PIC X(20).
                          09  W11-ORCACSTNM-N       PIC X(20).
                          09  W11-ORCACSTNM-N-COUNT PIC S9(9)   BINARY.
                          09  W11-ORCACSTNM-N-ITEM  PIC X(20)
                                                  OCCURS  10 TIMES.
                      07  W11-ORCACSTCHK-N-G   OCCURS   10.
                        08  W11-ORCACSTCHK-N   PIC X.
                      07  W11-BUNBEN           PIC X(20).
                    06  W11-N0102-STATE        PIC S9(9)   BINARY.
                04  W11-MSG01-STYLE              PIC X(20).
                04  W11-MSG01                    PIC X(100).
                04  W11-N01-STATE              PIC S9(9)   BINARY.
                04  W11-FI28.
                  05  W11-SYOHODSPNAME.
                    06  W11-VALUE              PIC X(10).
                  05  W11-SYOHOGEPRTFLG-COMBO.
                    06  W11-SYOHOGEPRTFLG      PIC X(12).
                    06  W11-SYOHOGEPRTFLG-COUNT
                                               PIC S9(9)  BINARY.
                    06  W11-SYOHOGEPRTFLGLST   PIC X(12)
                                               OCCURS  2 TIMES.
                  05  W11-KARUTE3KBN-COMBO.
                    06  W11-KARUTE3KBN.
                      07  W11-VALUE   PIC X(20).
                    06  W11-KARUTE3KBN-COUNT   PIC S9(9)   BINARY.
                    06  W11-KARUTE3KBN-ITEM
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W11-GAZOFOLDER.
                    06  W11-VALUE              PIC X(128).
                  05  W11-KOUHATUKA-COMB.
                    06  W11-KOUHATUKA.
                      07  W11-VALUE   PIC X(22).
                    06  W11-KOUHATUKA-COUNT   PIC S9(9)   BINARY.
                    06  W11-KOUHATUKA-ITEM
                            PIC X(22)   OCCURS  3 TIMES.
                04  W11-N02-STATE              PIC S9(9)    BINARY.
              03  W11-STYUKYMD                 PIC X(9).
              03  W11-EDYUKYMD                 PIC X(9).
              03  W11-USE-PRINT-SERVER         PIC X.
              03  W11-USE-PRINT-SERVER-STATE   PIC S9(9)   BINARY.
