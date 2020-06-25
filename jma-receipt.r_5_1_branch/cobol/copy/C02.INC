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
           02  C02.
              03  C02-BYOMEI-UP-STATE   PIC S9(09) BINARY.
              03  C02-BYOMEI-DOWN-STATE PIC S9(09) BINARY.
              03  C02-PTNUM-G.
                04  C02-PTNUM-STATE   PIC S9(9)   BINARY.
                04  C02-PTNUM   PIC X(20).
              03  C02-SYSYMD.
                04  C02-VALUE   PIC X(9).
              03  C02-KANANAME-STYLE   PIC X(20).
              03  C02-KANANAME.
                04  C02-VALUE   PIC X(30).
              03  C02-NAME-STYLE   PIC X(20).
              03  C02-NAME.
                04  C02-VALUE   PIC X(30).
              03  C02-SEX.
                04  C02-VALUE   PIC X(2).
              03  C02-BIRTHDAY.
                04  C02-VALUE   PIC X(12).
              03  C02-AGE.
                04  C02-VALUE   PIC X(6).
              03  C02-HKNNAME.
                04  C02-VALUE   PIC X(60).
              03  C02-PTFTNRATE.
                04  C02-VALUE   PIC X(4).
              03  C02-LASTYMD.
                04  C02-VALUE   PIC X(16).
              03  C02-SRYKA-COMBO.
                04  C02-SRYKA   PIC X(21).
                04  C02-SRYKA-COUNT
                                PIC S9(9)   BINARY.
                04  C02-SRYKALST
                                PIC X(21)   OCCURS  100 TIMES.
              03  C02-SYOKBN-COMBO.
                04  C02-SYOKBN   PIC X(6).
                04  C02-SYOKBN-COUNT
                                PIC S9(9)   BINARY.
                04  C02-SYOKBNLST
                                PIC X(6)   OCCURS  3 TIMES.
              03  C02-HNYUGAIKBN-COMBO.
                04  C02-HNYUGAIKBN
                                PIC X(14).
                04  C02-HNYUGAIKBN-COUNT
                                PIC S9(9)   BINARY.
                04  C02-HNYUGAIKBNLST
                                PIC X(14)   OCCURS  3 TIMES.
              03  C02-B03-STATE
                                PIC S9(9)   BINARY.
              03  C02-B04-STATE
                                PIC S9(9)   BINARY.
              03  C02-B05-STATE
                                PIC S9(9)   BINARY.
              03  C02-B06-STATE
                                PIC S9(9)   BINARY.
              03  C02-B07-STATE
                                PIC S9(9)   BINARY.
              03  C02-B08-STATE
                                PIC S9(9)   BINARY.
              03  C02-B09-STATE
                                PIC S9(9)   BINARY.
              03  C02-B12-STATE
                                PIC S9(9)   BINARY.
              03  C02-B21-LABEL PIC X(4).
              03  C02-B23-LABEL PIC X(6).
              03  C02-BANGO.
                04  C02-VALUE   PIC X(50).
              03  C02-SRYYMD.
                04  C02-VALUE   PIC X(10).
              03  C02-REZEMM.
                04  C02-VALUE   PIC S9(2).
              03  C02-TENKIYMD.
                04  C02-VALUE   PIC X(10).
              03  C02-BYOMEICD-G    OCCURS  6.
                04  C02-BYOMEICD.
                  05  C02-VALUE PIC X(12).
              03  C02-BYOMEI.
                04  C02-VALUE   PIC X(480).
              03  C02-HOSOKUCD-G    OCCURS  3.
                04  C02-HOSOKUCD.
                  05  C02-VALUE PIC X(12).
              03  C02-HOSOKU-COMMENT.
                04  C02-VALUE   PIC X(40).
              03  C02-CHARTBYOMEI.
                04  C02-VALUE   PIC X(480).
              03  C02-SYUBYOFLGCOMBO.
                04  C02-SYUBYOFLG.
                  05  C02-VALUE   PIC X(14).
                04  C02-SYUBYOFLG-COUNT   PIC S9(9)   BINARY.
                04  C02-SYUBYOFLG-ITEM    PIC X(14)   OCCURS  5 TIMES.
              03  C02-UTAGAIFLGCOMBO.
                04  C02-UTAGAIFLG.
                  05  C02-VALUE   PIC X(14).
                04  C02-UTAGAIFLG-COUNT   PIC S9(9)   BINARY.
                04  C02-UTAGAIFLG-ITEM    PIC X(14)   OCCURS  5 TIMES.
              03  C02-NYUGAIKBNCOMBO.
                04  C02-NYUGAIKBN.
                  05  C02-VALUE   PIC X(8).
                04  C02-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  C02-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  5 TIMES.
              03  C02-HKNBYOMEIFLGCOMBO.
                04  C02-HKNBYOMEIFLG.
                  05  C02-VALUE   PIC X(10).
                04  C02-HKNBYOMEIFLG-COUNT   PIC S9(9)   BINARY.
                04  C02-HKNBYOMEIFLG-ITEM    PIC X(10)   OCCURS  5.
              03  C02-HKNCOMBICOMBO.
                04  C02-HKNCOMBI.
                  05  C02-VALUE   PIC X(40).
                04  C02-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  C02-HKNCOMBI-ITEM    PIC X(40)   OCCURS  200 TIMES.
              03  C02-KOHIDCOMBO.
                04  C02-KOHIDCOMBO-STATE   PIC S9(9)   BINARY.
                04  C02-KOHID.
                  05  C02-VALUE   PIC X(22).
                04  C02-KOHID-COUNT   PIC S9(9)   BINARY.
                04  C02-KOHID-ITEM    PIC X(22)   OCCURS  100 TIMES.
              03  C02-BYOMEIHENFLG.
                04  C02-BYOMEIHENFLG-STYLE   PIC X(20).
                04  C02-BYOMEIHENFLG-VALUE   PIC X(10).
              03  C02-TENKICOMBO.
                04  C02-TENKIKBN.
                  05  C02-VALUE   PIC X(10).
                04  C02-TENKIKBN-COUNT   PIC S9(9)   BINARY.
                04  C02-TENKIKBN-ITEM    PIC X(10)    OCCURS  5 TIMES.
              03  C02-MANSEIFLGCOMBO.
                04  C02-MANSEIFLG.
                  05  C02-VALUE   PIC X(46).
                04  C02-MANSEIFLG-COUNT   PIC S9(9)   BINARY.
                04  C02-MANSEIFLG-ITEM    PIC X(46)   OCCURS  6 TIMES.
              03  C02-REZEFLGCOMBO.
                04  C02-REZEFLG.
                  05  C02-VALUE   PIC X(12).
                04  C02-REZEFLG-COUNT   PIC S9(9)   BINARY.
                04  C02-REZEFLG-ITEM    PIC X(12)   OCCURS  5 TIMES.
              03  C02-DISCHARGEFLGCOMBO.
                04  C02-DISCHARGEFLG.
                  05  C02-VALUE   PIC X(12).
                04  C02-DISCHARGEFLG-COUNT   PIC S9(9)  BINARY.
                04  C02-DISCHARGEFLG-ITEM    PIC X(12)  OCCURS  2 TIMES.
              03  C02-CLASS1COMBO.
                04  C02-CLASS1.
                  05  C02-VALUE   PIC X(11).
                04  C02-CLASS1-COUNT   PIC S9(9)   BINARY.
                04  C02-CLASS1-ITEM    PIC X(11)   OCCURS  10 TIMES.
              03  C02-CLASS2COMBO.
                04  C02-CLASS2.
                  05  C02-VALUE   PIC X(13).
                04  C02-CLASS2-COUNT   PIC S9(9)   BINARY.
                04  C02-CLASS2-ITEM    PIC X(13)   OCCURS  10 TIMES.
              03  C02-SCROLLEDWINDOW6.
                04  C02-HKNCOMBILST.
                  05  C02-HKN-COUNT       PIC S9(9)   BINARY.
                  05  C02-HKN-ROW         PIC S9(9)   BINARY.
                  05  C02-HKN-ROWATTR     PIC S9(9)   BINARY.
                  05  C02-HKN-ITEM        OCCURS  400 TIMES.
                    06  C02-HKNCOMBINUM   PIC X(4).
                    06  C02-HKNCOMBIMEI   PIC X(50).
                  05  C02-HKN-SELECT      PIC X   OCCURS  400 TIMES.
              03  C02-SCROLLEDWINDOW28.
                04  C02-BYOMEILST.
                  05  C02-BYO-COUNT       PIC S9(9)   BINARY.
                  05  C02-BYO-ROW         PIC S9(9)   BINARY.
                  05  C02-BYO-ROWATTR     PIC S9(9)   BINARY.
                  05  C02-BYO-ITEM        OCCURS  200 TIMES.
                    06  C02-TBANGO        PIC X(5).
                    06  C02-TSRYKA        PIC X(4).
                    06  C02-TBYOMEI       PIC X(50).
                    06  C02-TSRYYMD       PIC X(10).
                    06  C02-TTENKIYMD     PIC X(10).
                    06  C02-TTENKIKBN     PIC X(8).
                    06  C02-TMANSEIFLG    PIC X(12).
                    06  C02-TSYUBYOFLG    PIC X(8).
                    06  C02-TUTAGAIFLG    PIC X(14).
                    06  C02-TNYUGAIKBN    PIC X(8).
                    06  C02-THKNCOMBI     PIC X(40).
                    06  C02-TKOHID        PIC X(22).
                    06  C02-TREZEFLG      PIC X(10).
                    06  C02-TREZEMM       PIC X(4).
                    06  C02-THKNBYOMEIFLG PIC X(10).
                    06  C02-TDISCHARGEFLG PIC X(4).
                    06  C02-TCLASS1       PIC X(8).
                    06  C02-TCLASS2       PIC X(10).
                    06  C02-TCREYMD       PIC X(09).
                    06  C02-TCREHMS       PIC X(08).
                    06  C02-TUPYMD        PIC X(09).
                    06  C02-TUPHMS        PIC X(08).
                  05  C02-BYO-SELECT      PIC X   OCCURS  200 TIMES.
                  05  C02-BYO-FGCOLOR     PIC X(16)   OCCURS  200 TIMES.
                  05  C02-BYO-BGCOLOR     PIC X(16)   OCCURS  200 TIMES.
              03  C02-BSRYKA-COMBO.
                04  C02-BSRYKA  PIC X(21).
                04  C02-BSRYKA-COUNT
                                PIC S9(9)   BINARY.
                04  C02-BSRYKALST
                                PIC X(21)   OCCURS  100 TIMES.
              03  C02-HYOUJI-STYLE   PIC X(20).
              03  C02-HYOUJI         PIC X(22).
