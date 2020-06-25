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
            02  W02.
              03  W02-B03.
                04  W02-B03-STATE   PIC S9(9)   BINARY.
              03  W02-STYUKYMD.
                04  W02-VALUE   PIC X(09).
              03  W02-EDYUKYMD.
                04  W02-VALUE   PIC X(09).
              03  W02-NOTEBOOK3.
                04  W02-PAGENO  PIC S9(9)   BINARY.
                04  W02-FIXED23.
                  05  W02-HOSPNAME.
                    06  W02-VALUE   PIC X(60).
                  05  W02-TANHOSPNAME.
                    06  W02-VALUE   PIC X(10).
                  05  W02-KAISETUNAME.
                    06  W02-VALUE   PIC X(60).
                  05  W02-KANRINAME.
                    06  W02-VALUE   PIC X(40).
                  05  W02-ROUPAYKBN-COMBO.
                    06  W02-ROUPAYKBN         PIC X(8).
                    06  W02-ROUPAYKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-ROUPAYKBNLST      PIC X(8)    OCCURS  2.
                  05  W02-HOSPCDN.
                    06  W02-VALUE   PIC X(20).
                  05  W02-SANKANUM            PIC X(10).
                  05  W02-PREFNUM-COMBO.
                    06  W02-PREFNUM           PIC X(13).
                    06  W02-PREFNUM-COUNT     PIC S9(9)   BINARY.
                    06  W02-PREFNUMLST        PIC X(13)   OCCURS  47.
                  05  W02-TENHYOKBN-COMBO.
                    06  W02-TENHYOKBN         PIC X(8).
                    06  W02-TENHYOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-TENHYOKBNLST      PIC X(8)    OCCURS  2.
                  05  W02-HOSPCD.
                    06  W02-VALUE   PIC S9(7).
                  05  W02-HOSPSBT-COMBO.
                    06  W02-HOSPSBT           PIC X(10).
                    06  W02-HOSPSBT-COUNT     PIC S9(9)   BINARY.
                    06  W02-HOSPSBTLST        PIC X(10)   OCCURS  2.
                  05  W02-BEDSU.
                    06  W02-VALUE   PIC S9(4).
                  05  W02-BEDSUIPPN.
                    06  W02-VALUE   PIC S9(4).
                  05  W02-HOSPSBT1-COMBO.
                    06  W02-HOSPSBT1         PIC X(20).
                    06  W02-HOSPSBT1-COUNT   PIC S9(9)   BINARY.
                    06  W02-HOSPSBT1LST      PIC X(20)   OCCURS  2.
                  05  W02-HOSPID.
                    06  W02-VALUE   PIC X(24).
                  05  W02-SYOHOKBN-COMBO.
                    06  W02-SYOHOKBN         PIC X(8).
                    06  W02-SYOHOKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-SYOHOKBNLST      PIC X(8)    OCCURS  2.
                  05  W02-SKYPRTFLG-COMBO.
                    06  W02-SKYPRTFLG        PIC X(32).
                    06  W02-SKYPRTFLG-COUNT  PIC S9(9)   BINARY.
                    06  W02-SKYPRTFLGLST     PIC X(32)   OCCURS  9.
                  05  W02-SYOHOPRTFLG-COMBO.
                    06  W02-SYOHOPRTFLG      PIC X(30).
                    06  W02-SYOHOPRTFLG-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-SYOHOPRTFLGLST   PIC X(30)   OCCURS  5.
                  05  W02-ZENSYOHOFLG-COMBO.
                    06  W02-ZENSYOHOFLG      PIC X(14).
                    06  W02-ZENSYOHOFLG-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-ZENSYOHOFLGLST   PIC X(14)   OCCURS  2.
                  05  W02-YAKUJYOFLG-COMBO.
                    06  W02-YAKUJYOFLG       PIC X(30).
                    06  W02-YAKUJYOFLG-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-YAKUJYOFLGLST PIC X(30)   OCCURS  3.
                  05  W02-DISCOUNTHASU-CMB-G.
                    06  W02-DISCOUNTHASU-CMB-OCC      OCCURS  10.
                      07  W02-DISCOUNTHASU-CMB.
                        08  W02-DISCOUNTHASU     PIC X(32).
                        08  W02-DISCOUNTHASU-COUNT
                                             PIC S9(9)   BINARY.
                        08  W02-DISCOUNTHASULST  PIC X(32)
                                                      OCCURS  5.
                  05  W02-DAISANKBN-CMB.
                    06  W02-DAISANKBN        PIC X(32).
                    06  W02-DAISANKBN-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-DAISANKBNLST     PIC X(32)
                                                      OCCURS  5.
                  05  W02-GENTAIKBN-CMB.
                    06  W02-GENTAIKBN        PIC X(22).
                    06  W02-GENTAIKBN-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-GENTAIKBNLST     PIC X(22)
                                                      OCCURS  5.
                  05  W02-TAXHASU-COMBO.
                    06  W02-TAXHASU          PIC X(32).
                    06  W02-TAXHASU-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-TAXHASULST       PIC X(32)
                                                      OCCURS  3.
                  05  W02-HIWARIKBN-COMBO.
                    06  W02-HIWARIKBN        PIC X(20).
                    06  W02-HIWARIKBN-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-HIWARIKBNLST     PIC X(20)
                                                      OCCURS  2.
                  05  W02-DATACREATE-COMB.
                    06  W02-DATACREATE.
                      07  W02-VALUE   PIC X(14).
                    06  W02-DATACREATE-COUNT   PIC S9(9)   BINARY.
                    06  W02-DATACREATE-ITEM
                            PIC X(14)   OCCURS  5 TIMES.
                  05  W02-DATATEISYU-COMB.
                    06  W02-DATATEISYU.
                      07  W02-VALUE   PIC X(14).
                    06  W02-DATATEISYU-COUNT   PIC S9(9)   BINARY.
                    06  W02-DATATEISYU-ITEM
                            PIC X(14)   OCCURS  5 TIMES.
                  05  W02-DATAINFECT-COMB.
                    06  W02-DATAINFECT.
                      07  W02-VALUE   PIC X(14).
                    06  W02-DATAINFECT-COUNT   PIC S9(9)   BINARY.
                    06  W02-DATAINFECT-ITEM
                            PIC X(14)   OCCURS  5 TIMES.
                  05  W02-JIHIPOSITION-COMB.
                    06  W02-JIHIPOSITION.
                      07  W02-VALUE   PIC X(40).
                    06  W02-JIHIPOSITION-COUNT   PIC S9(9)   BINARY.
                    06  W02-JIHIPOSITION-ITEM
                            PIC X(40)   OCCURS  3 TIMES.
                  05  W02-MEIPRTFLG-COMB.
                    06  W02-MEIPRTFLG.
                      07  W02-VALUE   PIC X(32).
                    06  W02-MEIPRTFLG-COUNT   PIC S9(9)   BINARY.
                    06  W02-MEIPRTFLG-ITEM
                            PIC X(32)   OCCURS  9 TIMES.
                  05  W02-CHIHOUNUMKBN-COMB.
                    06  W02-CHIHOUNUMKBN.
                      07  W02-VALUE   PIC X(10).
                    06  W02-CHIHOUNUMKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-CHIHOUNUMKBN-ITEM
                            PIC X(10)   OCCURS  2 TIMES.
                  05  W02-OKUSURIFLG-COMB.
                    06  W02-OKUSURIFLG.
                      07  W02-VALUE   PIC X(36).
                    06  W02-OKUSURIFLG-COUNT   PIC S9(9)   BINARY.
                    06  W02-OKUSURIFLG-ITEM
                            PIC X(36)   OCCURS  9 TIMES.
                  05  W02-YYKHYOFLG-COMB.
                    06  W02-YYKHYOFLG.
                      07  W02-VALUE   PIC X(20).
                    06  W02-YYKHYOFLG-COUNT   PIC S9(9)   BINARY.
                    06  W02-YYKHYOFLG-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W02-JIHIHASUKBN-COMB.
                    06  W02-JIHIHASUKBN.
                      07  W02-VALUE   PIC X(40).
                    06  W02-JIHIHASUKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-JIHIHASUKBN-ITEM
                            PIC X(40)   OCCURS  5 TIMES.
                04  W02-N01-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED24.
                  05  W02-POST.
                    06  W02-VALUE   PIC X(7).
                  05  W02-ADRS.
                    06  W02-VALUE   PIC X(100).
                  05  W02-TEL.
                    06  W02-VALUE   PIC X(15).
                  05  W02-FAX.
                    06  W02-VALUE   PIC X(15).
                04  W02-N02-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED25.
                  05  W02-EMAIL.
                    06  W02-VALUE   PIC X(100).
                  05  W02-HPURL.
                    06  W02-VALUE   PIC X(100).
                  05  W02-USERURL.
                    06  W02-VALUE   PIC X(200).
                04  W02-N03-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED26.
                  05  W02-TEIRITU-COMBO.
                    06  W02-TEIRITU PIC X(10).
                    06  W02-TEIRITU-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-TEIRITULST       PIC X(10)
                                                         OCCURS  2.
                  05  W02-DISCOUNTRATE.
                    06  W02-VALUE   PIC S9(7).
                  05  W02-LBL-RATE  PIC X(02).
                04  W02-N04-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED27.
                  05  W02-DISCOUNTBODY.
                    06  W02-VALUE   PIC X(20).
                  05  W02-DISCOUNTKBN-COMBO.
                    06  W02-DISCOUNTKBN      PIC X(23).
                    06  W02-DISCOUNTKBN-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-DISCOUNTKBNLST   PIC X(23)
                                                         OCCURS  99.
                04  W02-N05-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED28.
                  05  W02-CONDITIONBODY.
                    06  W02-VALUE   PIC X(20).
                  05  W02-CONDITIONWORD.
                    06  W02-VALUE   PIC X(2).
                  05  W02-ICONFILENAME.
                    06  W02-VALUE   PIC X(100).
                04  W02-N06-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED29.
                  05  W02-GJIHINAME PIC X(20)   OCCURS 10.
                  05  W02-NJIHINAME PIC X(20)   OCCURS 10.
                04  W02-N07-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED30.
                  05  W02-SRYNAIYO.
                    06  W02-VALUE   PIC X(20).
                  05  W02-SRYNAIYODSP.
                    06  W02-VALUE   PIC X(12).
                  05  W02-SRYNAIHYOJI-COMB.
                    06  W02-SRYNAIHYOJI.
                      07  W02-VALUE   PIC X(20).
                    06  W02-SRYNAIHYOJI-COUNT   PIC S9(9)   BINARY.
                    06  W02-SRYNAIHYOJI-ITEM
                            PIC X(20)   OCCURS  5 TIMES.
                04  W02-N08.
                  05  W02-N08-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED31.
                  05  W02-NYKN-KBNCD.
                    06  W02-VALUE   PIC X(02).
                  05  W02-NYKN-NAME.
                    06  W02-VALUE   PIC X(30).
                  05  W02-NYKN-TANNAME.
                    06  W02-VALUE   PIC X(10).
                  05  W02-NYKN-BUNRUI-COMBO.
                    06  W02-NYKN-BUNRUI      PIC X(23).
                    06  W02-NYKN-BUNRUI-COUNT
                                             PIC S9(9)   BINARY.
                    06  W02-NYKN-BUNRUILST   PIC X(23)
                                                     OCCURS  10.
                  05  W02-NYKN-JYOTAI-COMB.
                    06  W02-NYKN-JYOTAI.
                      07  W02-VALUE   PIC X(22).
                    06  W02-NYKN-JYOTAI-COUNT   PIC S9(9)   BINARY.
                    06  W02-NYKN-JYOTAI-ITEM
                            PIC X(22)   OCCURS  10 TIMES.
                  05  W02-NYKN-JYOTAI-N-COMB.
                    06  W02-NYKN-JYOTAI-N.
                      07  W02-VALUE   PIC X(22).
                    06  W02-NYKN-JYOTAI-N-COUNT   PIC S9(9)   BINARY.
                    06  W02-NYKN-JYOTAI-N-ITEM
                            PIC X(22)   OCCURS  10 TIMES.
                  05  W02-NYKN-JYOTAI-T-COMB.
                    06  W02-NYKN-JYOTAI-T.
                      07  W02-VALUE   PIC X(22).
                    06  W02-NYKN-JYOTAI-T-COUNT   PIC S9(9)   BINARY.
                    06  W02-NYKN-JYOTAI-T-ITEM
                            PIC X(22)   OCCURS  10 TIMES.
                04  W02-N09.
                  05  W02-N09-STATE   PIC S9(9)   BINARY.
                04  W02-FIXED32.
                  05  W02-YYKKBNNAME.
                    06  W02-VALUE   PIC X(30).
                  05  W02-YYKKBNNAME1.
                    06  W02-VALUE   PIC X(10).
                  05  W02-HYOJIKBN-COMB.
                    06  W02-HYOJIKBN.
                      07  W02-VALUE   PIC X(20).
                    06  W02-HYOJIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W02-HYOJIKBN-ITEM
                            PIC X(20)   OCCURS  2 TIMES.
                04  W02-N10.
                  05  W02-N10-STATE   PIC S9(9)   BINARY.


