        02  Z010.
            03  Z010-TITLE  PIC X(256).
            03  Z010-POPUP-SUMMARY  PIC X(256).
            03  Z010-POPUP-BODY PIC X(256).
            03  Z010-POPUP-ICON PIC X(256).
            03  Z010-POPUP-TIMEOUT  PIC S9(9)   BINARY.
            03  Z010-FIXED10.
              04  Z010-B01.
                05  Z010-STATE  PIC S9(9)   BINARY.
              04  Z010-B12.
                05  Z010-STATE  PIC S9(9)   BINARY.
              04  Z010-SELNUM.
                05  Z010-TEXTDATA   PIC X(3).
              04  Z010-B02.
                05  Z010-STATE  PIC S9(9)   BINARY.
              04  Z010-B04.
                05  Z010-STATE  PIC S9(9)   BINARY.
              04  Z010-SHONAME.
                05  Z010-TEXTDATA   PIC X(200).
              04  Z010-SCROLLEDWINDOW1.
                05  Z010-RRKLIST.
                  06  Z010-RRKLIST-COUNT  PIC S9(9)   BINARY.
                  06  Z010-RRKLIST-ROW    PIC S9(9)   BINARY.
                  06  Z010-RRKLIST-SELECT
                          PIC X       OCCURS  50 TIMES.
                  06  Z010-FGCOLOR
                          PIC X(20)  OCCURS  50 TIMES.
                  06  Z010-BGCOLOR
                          PIC X(20)  OCCURS  50 TIMES.
                  06  Z010-ITEM       OCCURS  50 TIMES.
                    07  Z010-TRENNUM    PIC X(3).
                    07  Z010-TYUKOSTYMD PIC X(10).
                    07  Z010-TYUKOEDYMD PIC X(10).
              04  Z010-YUKOSTYMD.
                05  Z010-TEXTDATA   PIC X(10).
              04  Z010-YUKOEDYMD.
                05  Z010-TEXTDATA   PIC X(10).
              04  Z010-SAIKETU-COMB.
                05  Z010-SAIKETU-COUNT  PIC S9(9)   BINARY.
                05  Z010-SAIKETU-ITEM
                        PIC X(16)   OCCURS  4 TIMES.
                05  Z010-SAIKETUKBN.
                  06  Z010-TEXTDATA   PIC X(16).
              04  Z010-ZENKNSKBN-COMB.
                05  Z010-ZENKNSKBN-COUNT  PIC S9(9)   BINARY.
                05  Z010-ZENKNSKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
                05  Z010-ZENKNSKBN.
                  06  Z010-TEXTDATA   PIC X(30).
              04  Z010-PLUS-SANTEI-COMB.
                05  Z010-PLUS-SANTEI-COUNT  PIC S9(9)   BINARY.
                05  Z010-PLUS-SANTEI-ITEM
                        PIC X(30)   OCCURS  3 TIMES.
                05  Z010-PLUS-SANTEIRRK.
                  06  Z010-TEXTDATA   PIC X(30).
              04  Z010-PLUS-JGNCNTETCM-G.
                05  Z010-PLUS-JGNCNTETCM    PIC S9(2).
              04  Z010-PLUS-JGNCNT-G.
                05  Z010-PLUS-JGNCNT      PIC S9(3).
              04  Z010-PLUS-JGNCNT1D-G.
                05  Z010-PLUS-JGNCNT1D    PIC S9(3).
              04  Z010-PLUS-JGNCNT1W-G.
                05  Z010-PLUS-JGNCNT1W    PIC S9(3).
              04  Z010-PLUS-JGNCNTETC-G.
                05  Z010-PLUS-JGNCNTETC    PIC S9(3).
              04  Z010-PLUS-JGNCNTERR-G.
                05  Z010-PLUS-JGNCNTERR    PIC S9(1).
              04  Z010-SOURYOKBN-COMB.
                05  Z010-SOURYOKBN-COUNT  PIC S9(9)   BINARY.
                05  Z010-SOURYOKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
                05  Z010-SOURYOKBN.
                  06  Z010-TEXTDATA   PIC X(30).
              04  Z010-IPNKISAIKBN-COMB.
                05  Z010-IPNKISAIKBN-COUNT  PIC S9(9)   BINARY.
                05  Z010-IPNKISAIKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
                05  Z010-IPNKISAIKBN.
                  06  Z010-TEXTDATA   PIC X(30).
              04  Z010-IPNTANICD-COMB.
                05  Z010-IPNTANICD-COUNT  PIC S9(9)   BINARY.
                05  Z010-IPNTANICD-ITEM
                        PIC X(28)   OCCURS  200 TIMES.
                05  Z010-IPNTANICD.
                  06  Z010-TEXTDATA   PIC X(28).
              04  Z010-BUNRUIKBN-COMBO.
                05  Z010-BUNRUIKBN-COUNT  PIC S9(9)   BINARY.
                05  Z010-BUNRUIKBN-ITEM
                        PIC X(43)   OCCURS  20 TIMES.
                05  Z010-BUNRUIKBN.
                  06  Z010-TEXTDATA   PIC X(43).
              04  Z010-IPNKANZANCHI-G.
                05  Z010-IPNKANZANCHI    PIC S9(5)V9(5).
              04  Z010-ALLSURYOKBN-COMBO.
                05  Z010-ALLSURYOKBN-COUNT  PIC S9(9)   BINARY.
                05  Z010-ALLSURYOKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
                05  Z010-ALLSURYOKBN.
                  06  Z010-TEXTDATA   PIC X(30).
              04  Z010-KANZANTANICD-COMBO.
                05  Z010-KANZANTANICD-COUNT  PIC S9(9)   BINARY.
                05  Z010-KANZANTANICD-ITEM
                        PIC X(28)   OCCURS  200 TIMES.
                05  Z010-KANZANTANICD.
                  06  Z010-TEXTDATA   PIC X(28).
              04  Z010-KANZANCHI-G.
                05  Z010-KANZANCHI    PIC S9(5)V9(5).
              04  Z010-SIPRYOU1-G.
                05  Z010-SIPRYOU1    PIC S9(3).
              04  Z010-SIPIRISU1-G.
                05  Z010-SIPIRISU1    PIC S9(3).
              04  Z010-SIPRYOU2-G.
                05  Z010-SIPRYOU2    PIC S9(3).
              04  Z010-SIPIRISU2-G.
                05  Z010-SIPIRISU2    PIC S9(3).
              04  Z010-MIDASHI.
                05  Z010-MIDASHI-STYLE  PIC X(20).
                05  Z010-MIDASHI-DATA   PIC X(80).
              04  Z010-MEISHO.
                05  Z010-MEISHO-STYLE  PIC X(20).
                05  Z010-MEISHO-DATA   PIC X(20).
              03  Z010-USER-TANICD-COMB.
                04  Z010-USER-TANICD-COUNT  PIC S9(9)   BINARY.
                04  Z010-USER-TANICD-ITEM
                        PIC X(28)   OCCURS  200 TIMES.
                04  Z010-USER-TANICD.
                  05  Z010-TEXTDATA   PIC X(28).
