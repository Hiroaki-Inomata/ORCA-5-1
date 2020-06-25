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
          02  Q01.
            03  Q01-FIXED12.
              04  Q01-B05S-STATE               PIC S9(9)   BINARY.
              04  Q01-B06S-STATE               PIC S9(9)   BINARY.
              04  Q01-B07S-STATE               PIC S9(9)   BINARY.
              04  Q01-B06-LABEL                PIC X(10).
              04  Q01-B07-LABEL                PIC X(10).
              04  Q01-CMB-SELPATTERN.
                05  Q01-SELPATTERN             PIC X(104).
                05  Q01-CMB-SELPATTERN-COUNT   PIC S9(9)   BINARY.
                05  Q01-CMB-SELPATTERN-ITEM    PIC X(104)  OCCURS 200.
              04  Q01-STSRYYMD                 PIC X(10).
              04  Q01-EDSRYYMD                 PIC X(10).
              04  Q01-LBLSENISAKI              PIC X(40).
              04  Q01-NOTEBOOK3.
                05  Q01-PAGENO                 PIC S9(9)   BINARY.
                05  Q01-FIXED16.
                  06  Q01-STBIRTHDAY           PIC X(10).
                  06  Q01-EDBIRTHDAY           PIC X(10).
                  06  Q01-STAGE                PIC X(03).
                  06  Q01-EDAGE                PIC X(03).
                  06  Q01-KJYNYMD              PIC X(10).
                  06  Q01-BIRTHMON             PIC X(2).
                  06  Q01-CMB-SEX.
                    07  Q01-SEX                PIC X(10).
                    07  Q01-CMB-SEX-COUNT      PIC S9(9)   BINARY.
                    07  Q01-CMB-SEX-ITEM       PIC X(10)   OCCURS  3.
                  06  Q01-STLASTYMD            PIC X(10).
                  06  Q01-EDLASTYMD            PIC X(10).
                  06  Q01-STPTNUM              PIC X(20).
                  06  Q01-EDPTNUM              PIC X(20).
                  06  Q01-CMB-NYUGAIKBN.
                    07  Q01-NYUGAIKBN          PIC X(12).
                    07  Q01-CMB-NYUGAIKBN-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-NYUGAIKBN-ITEM PIC X(12)   OCCURS  2.
                  06  Q01-CMB-DRCD.
                    07  Q01-DRCD                 PIC X(29).
                    07  Q01-CMB-DRCD-COUNT       PIC S9(9)   BINARY.
                    07  Q01-CMB-DRCD-ITEM        PIC X(29)   OCCURS  99.
                  06  Q01-LBLMSG01             PIC X(100).
                  06  Q01-LBLMSG01-STYLE       PIC X(20).
                  06  Q01-CMB-SRYKA.
                    07  Q01-SRYKA              PIC X(23).
                    07  Q01-CMB-SRYKA-COUNT    PIC S9(9)   BINARY.
                    07  Q01-CMB-SRYKA-ITEM     PIC X(23)   OCCURS  50.
                  06  Q01-CMB-DEATH.
                    07  Q01-DEATH              PIC         X(10).
                    07  Q01-CMB-DEATH-COUNT    PIC S9(9)   BINARY.
                    07  Q01-CMB-DEATH-ITEM     PIC X(10)   OCCURS  2.
                  06  Q01-CMB-AGREEMENT.
                    07  Q01-AGREEMENT              PIC         X(22).
                    07  Q01-CMB-AGREEMENT-COUNT    PIC S9(9)   BINARY.
                    07  Q01-CMB-AGREEMENT-ITEM     PIC X(22)   OCCURS 5.
                  06  Q01-LBLTSTPT             PIC         X(20).
                  06  Q01-CMB-TSTPT.
                    07  Q01-TSTPT              PIC         X(32).
                    07  Q01-CMB-TSTPT-COUNT    PIC S9(9)   BINARY.
                    07  Q01-CMB-TSTPT-ITEM     PIC X(32)   OCCURS 5.
                  06  Q01-POST-TBL.
                    07  Q01-POST               PIC X(07)   OCCURS 5.
                  06  Q01-JYUSHO-TBL.
                    07  Q01-JYUSHO             PIC X(50)   OCCURS 5.
                  06  Q01-NAME-TBL.
                    07  Q01-NAME               PIC X(50)   OCCURS 5.
                  06  Q01-CMB-CONDITION1-TBL.
                    07  Q01-CMB-CONDITION1-OCC             OCCURS 3.
                      08  Q01-CONDITION1           PIC X(23).
                      08  Q01-CMB-CONDITION1-COUNT PIC S9(9)
                                                           BINARY.
                      08  Q01-CMB-CONDITION1-ITEM  PIC X(23)
                                                           OCCURS 100.
                  06  Q01-CMB-CONDITION2-TBL.
                    07  Q01-CMB-CONDITION2-OCC             OCCURS 3.
                      08  Q01-CONDITION2           PIC X(23).
                      08  Q01-CMB-CONDITION2-COUNT PIC S9(9)
                                                           BINARY.
                      08  Q01-CMB-CONDITION2-ITEM  PIC X(23)
                                                           OCCURS 100.
                  06  Q01-CMB-CONDITION3-TBL.
                    07  Q01-CMB-CONDITION3-OCC             OCCURS 3.
                      08  Q01-CONDITION3           PIC X(23).
                      08  Q01-CMB-CONDITION3-COUNT PIC S9(9)
                                                           BINARY.
                      08  Q01-CMB-CONDITION3-ITEM  PIC X(23)
                                                           OCCURS 100.
                  06  Q01-CMB-GENMEN-TBL.
                    07  Q01-CMB-GENMEN-OCC             OCCURS 3.
                      08  Q01-GENMEN           PIC X(23).
                      08  Q01-CMB-GENMEN-COUNT PIC S9(9)
                                                           BINARY.
                      08  Q01-CMB-GENMEN-ITEM  PIC X(23)
                                                           OCCURS 99.
                  06  Q01-CMB-SELCOMMENT.
                    07  Q01-SELCOMMENT              PIC X(40).
                    07  Q01-CMB-SELCOMMENT-COUNT    PIC S9(9) BINARY.
                    07  Q01-CMB-SELCOMMENT-ITEM     PIC X(40) OCCURS 5.
                  06  Q01-COMMENT-TBL.
                    07  Q01-COMMENT            PIC X(50)   OCCURS 3.
                  06  Q01-CMB-TEL-TO.
                      08  Q01-TEL-TO               PIC X(20).
                      08  Q01-CMB-TEL-TO-COUNT     PIC S9(9)
                                                           BINARY.
                      08  Q01-CMB-TEL-TO-ITEM      PIC X(20)
                                                           OCCURS 5.
                  06  Q01-TEL-TBL.
                    07  Q01-TEL                    PIC X(15) OCCURS 3.
                  06  Q01-CMB-TSY.
                    07  Q01-TSY                    PIC X(30).
                    07  Q01-CMB-TSY-COUNT          PIC S9(9) BINARY.
                    07  Q01-CMB-TSY-ITEM           PIC X(30) OCCURS 5.
                  06  Q01-STTSYYM                  PIC X(10).
                  06  Q01-CMB-STTSYSKBT.
                    07  Q01-STTSYSKBT              PIC X(10).
                    07  Q01-CMB-STTSYSKBT-COUNT    PIC S9(9) BINARY.
                    07  Q01-CMB-STTSYSKBT-ITEM     PIC X(10) OCCURS 5.
                  06  Q01-EDTSYYM                  PIC X(10).
                  06  Q01-EDTSYYM-STATE            PIC S9(9)   BINARY.
                  06  Q01-LBLTSY                   PIC X(02).
                  06  Q01-TOKKICD                  PIC X(02).
                  06  Q01-TOKKINAME                PIC X(12).
                  06  Q01-CMB-TOKKIINFO.
                    07  Q01-TOKKIINFO              PIC X(33).
                    07  Q01-CMB-TOKKIINFO-COUNT    PIC S9(9) BINARY.
                    07  Q01-CMB-TOKKIINFO-ITEM     PIC X(33) OCCURS 50.
                  06  Q01-CMB-SAIGAIKBN.
                    07  Q01-SAIGAIKBN              PIC X(22).
                    07  Q01-CMB-SAIGAIKBN-COUNT    PIC S9(9) BINARY.
                    07  Q01-CMB-SAIGAIKBN-ITEM     PIC X(22) OCCURS 10.
                  06  Q01-TOTALTEN                 PIC S9(7).
                  06  Q01-CMB-TENKENRECE.
                    07  Q01-TENKENRECE              PIC X(22).
                    07  Q01-CMB-TENKENRECE-COUNT    PIC S9(9) BINARY.
                    07  Q01-CMB-TENKENRECE-ITEM     PIC X(22) OCCURS 5.
                05  Q01-FIXED17.
                  06  Q01-HKNEDYM              PIC X(07).
                  06  Q01-CMB-HONKZKKBN.
                    07  Q01-HONKZKKBN          PIC X(10).
                    07  Q01-CMB-HONKZKKBN-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-HONKZKKBN-ITEM
                                               PIC X(10)   OCCURS 5.
                  06  Q01-CMB-FTNRATE.
                    07  Q01-FTNRATE            PIC X(35).
                    07  Q01-CMB-FTNRATE-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-FTNRATE-ITEM
                                               PIC X(35)   OCCURS 10.
                  06  Q01-HKNJANUM-TBL.
                    07  Q01-HKNJANUM           PIC X(08)   OCCURS 5.
                  06  Q01-CMB-HKNNUM-TBL.
                    07  Q01-CMB-HKNNUM-OCC                 OCCURS 5.
                      08  Q01-HKNNUM           PIC X(30).
                      08  Q01-CMB-HKNNUM-COUNT PIC S9(9)   BINARY.
                      08  Q01-CMB-HKNNUM-ITEM  PIC X(30)   OCCURS  50.
                  06  Q01-KIGO-TBL.
                    07  Q01-KIGO               PIC X(40)   OCCURS 5.
                  06  Q01-NUM-TBL.
                    07  Q01-NUM                PIC X(40)   OCCURS 5.
                  06  Q01-KOHEDYM              PIC X(07).
                  06  Q01-CMB-KOHNUM-TBL.
                    07  Q01-CMB-KOHNUM-OCC                 OCCURS 5.
                      08  Q01-KOHNUM           PIC X(20).
                      08  Q01-CMB-KOHNUM-COUNT PIC S9(9)   BINARY.
                      08  Q01-CMB-KOHNUM-ITEM  PIC X(20)   OCCURS 150.
                  06  Q01-FTNJANUM-TBL.
                    07  Q01-FTNJANUM           PIC X(8)    OCCURS 5.
                  06  Q01-CMB-JKNHKNJANUM.
                    07  Q01-JKNHKNJANUM           PIC X(10).
                    07  Q01-CMB-JKNHKNJANUM-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-JKNHKNJANUM-ITEM  PIC X(10)   OCCURS 5.
                  06  Q01-CMB-JKNHKNNUM.
                    07  Q01-JKNHKNNUM           PIC X(10).
                    07  Q01-CMB-JKNHKNNUM-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-JKNHKNNUM-ITEM  PIC X(10)   OCCURS 5.
                  06  Q01-CMB-JKNKOHNUM.
                    07  Q01-JKNKOHNUM           PIC X(10).
                    07  Q01-CMB-JKNKOHNUM-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-JKNKOHNUM-ITEM  PIC X(10)   OCCURS 5.
                  06  Q01-RDHKNKOH-AND         PIC X.
                  06  Q01-RDHKNKOH-OR          PIC X.
                05  Q01-FIXED18.
                  06  Q01-STBYOYMD             PIC X(10).
                  06  Q01-EDBYOYMD             PIC X(10).
                  06  Q01-CMB-TENKIKBN.
                    07  Q01-TENKIKBN          PIC X(20).
                    07  Q01-CMB-TENKIKBN-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-TENKIKBN-ITEM PIC X(20)    OCCURS  10.
                  06  Q01-STTENKIYMD           PIC X(10).
                  06  Q01-STTENKIYMD-STATE     PIC S9(9)   BINARY.
                  06  Q01-EDTENKIYMD           PIC X(10).
                  06  Q01-EDTENKIYMD-STATE     PIC S9(9)   BINARY.
                  06  Q01-CMB-UTAGAIKBN.
                    07  Q01-UTAGAIKBN          PIC X(20).
                    07  Q01-CMB-UTAGAIKBN-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-UTAGAIKBN-ITEM PIC X(20)   OCCURS  3.
                  06  Q01-CMB-KYUSEIKBN.
                    07  Q01-KYUSEIKBN          PIC X(20).
                    07  Q01-CMB-KYUSEIKBN-COUNT
                                               PIC S9(9)   BINARY.
                    07  Q01-CMB-KYUSEIKBN-ITEM PIC X(20)   OCCURS  3.
                  06  Q01-CMB-SYUBYOKBN.
                    07  Q01-SYUBYOKBN           PIC X(20).
                    07  Q01-CMB-SYUBYOKBN-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-SYUBYOKBN-ITEM  PIC X(20)   OCCURS  5.
                  06  Q01-CMB-BYOBUN.
                    07  Q01-BYOBUN             PIC X(24).
                    07  Q01-BYOBUN-STATE       PIC S9(9)   BINARY.
                    07  Q01-CMB-BYOBUN-COUNT   PIC S9(9)   BINARY.
                    07  Q01-CMB-BYOBUN-ITEM    PIC X(24)   OCCURS  100.
                    07  Q01-CMB-BYOBUN-STATE   PIC S9(9)   BINARY.
                  06  Q01-RDBYOMEI-U           PIC X.
                  06  Q01-RDBYOMEI-U-STATE     PIC S9(9)   BINARY.
                  06  Q01-RDBYOMEI-A           PIC X.
                  06  Q01-RDBYOMEI-A-STATE     PIC S9(9)   BINARY.
                  06  Q01-BYOMEISEL            PIC X(80).
                  06  Q01-BYOMEICD-TBL.
                    07  Q01-BYOMEICD           PIC X(20)    OCCURS 5.
                  06  Q01-BYOMEI-TBL.
                    07  Q01-BYOMEI             PIC X(80)    OCCURS 5.
                  06  Q01-BYOCOMP-TBL.
                    07  Q01-CMB-BYOCOMP         OCCURS  5.
                      08  Q01-BYOCOMP           PIC X(22).
                      08  Q01-CMB-BYOCOMP-COUNT PIC S9(9)   BINARY.
                      08  Q01-CMB-BYOCOMP-ITEM  PIC X(22)   OCCURS  5.
                  06  Q01-SCR-BYOLST.
                    07  Q01-BYOLST.
                      08  Q01-BYOLST-COUNT     PIC S9(9)   BINARY.
                      08  Q01-BYOLST-ROW       PIC S9(9)   BINARY.
                      08  Q01-BYOLST-ROWATTR   PIC S9(9)   BINARY.
                      08  Q01-BYOLST-ITEM  OCCURS  200 TIMES.
                        09  Q01-BYOLST-BYOMEICD
                                               PIC X(20).
                        09  Q01-BYOLST-BYOMEI  PIC X(80).
                      08  Q01-BYOLST-SELECT-TBL.
                        09  Q01-BYOLST-SELECT  PIC X   OCCURS  200.
                  06  Q01-CMB-SRYKBN.
                    07  Q01-SRYKBN             PIC X(23).
                    07  Q01-CMB-SRYKBN-COUNT   PIC S9(9)   BINARY.
                    07  Q01-CMB-SRYKBN-ITEM    PIC X(23)   OCCURS  30.
                  06  Q01-LBLSRYSYUKBN         PIC X(20).
                  06  Q01-CMB-SRYSYUKBN.
                    07  Q01-SRYSYUKBN             PIC X(23).
                    07  Q01-SRYSYUKBN-STATE       PIC S9(9)   BINARY.
                    07  Q01-CMB-SRYSYUKBN-COUNT   PIC S9(9)   BINARY.
                    07  Q01-CMB-SRYSYUKBN-ITEM    PIC X(23)  OCCURS  10.
                    07  Q01-CMB-SRYSYUKBN-STATE   PIC S9(9)   BINARY.
                  06  Q01-CMB-HOKATSUKBN.
                    07  Q01-HOKATSUKBN            PIC X(22).
                    07  Q01-CMB-HOKATSUKBN-COUNT  PIC S9(9)   BINARY.
                    07  Q01-CMB-HOKATSUKBN-ITEM   PIC X(22)   OCCURS  5.
                  06  Q01-SRYACTSEL            PIC X(80).
                  06  Q01-SRYCD-TBL.
                    07  Q01-SRYCD-OCC   OCCURS   5.
                      08  Q01-SRYCD         PIC X(20).
                  06  Q01-SRYACT-TBL.
                    07  Q01-SRYACT-OCC   OCCURS   5.
                      08  Q01-SRYACT        PIC X(80).
                  06  Q01-SCR-SRYLST.
                    07  Q01-SRYLST.
                      08  Q01-SRYLST-COUNT     PIC S9(9)   BINARY.
                      08  Q01-SRYLST-ROW       PIC S9(9)   BINARY.
                      08  Q01-SRYLST-ROWATTR   PIC S9(9)   BINARY.
                      08  Q01-SRYLST-ITEM  OCCURS  200 TIMES.
                        09  Q01-SRYLST-SRYCD
                                               PIC X(09).
                        09  Q01-SRYLST-SRYACT  PIC X(80).
                      08  Q01-SRYLST-SELECT-TBL.
                        09  Q01-SRYLST-SELECT  PIC X   OCCURS  200.
                  06  Q01-CMB-SRYVALITEM.
                    07 Q01-SRYVALITEM       PIC X(32).
                    07 Q01-CMB-SRYVALITEM-COUNT PIC S9(9) BINARY.
                    07 Q01-CMB-SRYVALITEM-ITEM  PIC X(32) OCCURS 10.
                  06  Q01-LBLSRYVAL     PIC X(10).
                  06  Q01-SRYVAL-OCC    OCCURS   5.
                      07  Q01-SRYVAL    PIC X(11).
                      07  Q01-SRYVAL-STATE      PIC S9(9) BINARY.
                  06  Q01-CMB-SRYVALCOMP-G.
                    07  Q01-CMB-SRYVALCOMP-OCC  OCCURS   5.
                      08  Q01-CMB-SRYVALCOMP.
                        09 Q01-SRYVALCOMP       PIC X(22).
                        09 Q01-SRYVALCOMP-STATE PIC S9(9) BINARY.
                        09 Q01-CMB-SRYVALCOMP-COUNT
                                                   PIC S9(9)   BINARY.
                        09 Q01-CMB-SRYVALCOMP-ITEM
                                                   PIC X(22) OCCURS 10.
                        09 Q01-CMB-SRYVALCOMP-ITEM-STATE
                                                   PIC S9(9) BINARY.
                  06  Q01-RECECOM-TBL.
                    07  Q01-RECECOM            PIC X(50)   OCCURS 3.
                  06  Q01-CMB-JKNBYOMEI.
                    07  Q01-JKNBYOMEI           PIC X(10).
                    07  Q01-CMB-JKNBYOMEI-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-JKNBYOMEI-ITEM  PIC X(10)   OCCURS 5.
                  06  Q01-CMB-JKNSRYACT.
                    07  Q01-JKNSRYACT           PIC X(10).
                    07  Q01-CMB-JKNSRYACT-COUNT PIC S9(9)   BINARY.
                    07  Q01-CMB-JKNSRYACT-ITEM  PIC X(10)   OCCURS 5.
                05  Q01-FIXED20.
                  06  Q01-CMB-KHSKBT.
                    07  Q01-KHSKBT              PIC X(10).
                    07  Q01-KHSKBT-COUNT        PIC S9(9)   BINARY.
                    07  Q01-KHSKBT-ITEM         PIC X(10)   OCCURS 5.
                  06  Q01-STKHYM                PIC X(10).
                  06  Q01-LBLKHYM               PIC X(02).
                  06  Q01-EDKHYM                PIC X(10).
                  06  Q01-EDKHYM-STATE          PIC S9(9) BINARY.
                  06  Q01-KHHKNJANUM            PIC X(06).
                  06  Q01-CMB-KNSKBT.
                    07  Q01-KNSKBT              PIC X(10).
                    07  Q01-KNSKBT-COUNT        PIC S9(9)   BINARY.
                    07  Q01-KNSKBT-ITEM         PIC X(10)   OCCURS 5.
                  06  Q01-STKNYM                PIC X(10).
                  06  Q01-LBLKNYM               PIC X(02).
                  06  Q01-EDKNYM                PIC X(10).
                  06  Q01-EDKNYM-STATE          PIC S9(9) BINARY.
                  06  Q01-CMB-KNJOTAI.
                    07  Q01-KNJOTAI               PIC X(20).
                    07  Q01-KNJOTAI-COUNT         PIC S9(9)   BINARY.
                    07  Q01-KNJOTAI-ITEM          PIC X(20)   OCCURS 20.
                05  Q01-FIXED19.
                  06  Q01-CMB-KMK-TBL.
                    07  Q01-CMB-KMK-OCC    OCCURS  3.
                      08  Q01-CMB-KMK          PIC X(30).
                      08  Q01-CMB-KMK-COUNT    PIC S9(9) BINARY.
                      08  Q01-CMB-KMK-ITEM     PIC X(30) OCCURS  10.
                  06  Q01-RDIO-TBL.
                    07  Q01-RDIO-OCC       OCCURS  3.
                      08  Q01-RDIO1            PIC X.
                      08  Q01-RDIO2            PIC X.
                  06  Q01-LBLORDER-TBL.
                    07  Q01-LBLORDER-OCC   OCCURS  3.
                      08  Q01-LBLORDER         PIC X(80).
                      08  Q01-LBLORDER-STYLE   PIC X(20).
                  06  Q01-CMB-TKMK-TBL.
                    07  Q01-CMB-TKMK-OCC    OCCURS  3.
                      08  Q01-CMB-TKMK          PIC X(30).
                      08  Q01-CMB-TKMK-COUNT    PIC S9(9) BINARY.
                      08  Q01-CMB-TKMK-ITEM     PIC X(30) OCCURS  10.
                  06  Q01-TRDIO-TBL.
                    07  Q01-TRDIO-OCC       OCCURS  3.
                      08  Q01-TRDIO1            PIC X.
                      08  Q01-TRDIO2            PIC X.
                  06  Q01-CMB-SKMK-TBL.
                    07  Q01-CMB-SKMK-OCC    OCCURS  3.
                      08  Q01-CMB-SKMK          PIC X(30).
                      08  Q01-CMB-SKMK-COUNT    PIC S9(9) BINARY.
                      08  Q01-CMB-SKMK-ITEM     PIC X(30) OCCURS  10.
                  06  Q01-SRDIO-TBL.
                    07  Q01-SRDIO-OCC       OCCURS  3.
                      08  Q01-SRDIO1            PIC X.
                      08  Q01-SRDIO2            PIC X.

