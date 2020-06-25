            02  H02.
              03  H02-B12-STATE      PIC S9(09) BINARY.
              03  H02-SELNUM-STATE   PIC S9(09) BINARY.
              03  H02-SELNUM.
                04  H02-VALUE   PIC S9(2).
              03  H02-LABEL-STYLE   PIC X(20).
              03  H02-LABEL         PIC X(10).
              03  H02-SYORIYM-STYLE PIC X(20).
              03  H02-SYORIYM       PIC X(6).
              03  H02-NYUGAIKBN-STYLE   PIC X(20).
              03  H02-NYUGAIKBN     PIC X(6).
              03  H02-RECESYUBETU-STYLE   PIC X(20).
              03  H02-RECESYUBETU   PIC X(10).
              03  H02-HKNJANUM-STYLE   PIC X(20).
              03  H02-HKNJANUM      PIC X(8).
              03  H02-KIGO-STYLE   PIC X(20).
              03  H02-KIGO         PIC X(20).
              03  H02-ERR-STYLE    PIC X(20).
              03  H02-ERR          PIC X(100).
              03  H02-SCROLLEDWINDOW26.
                04  H02-HENREILIST.
                  05  H02-HENREILIST-COUNT     PIC S9(9)   BINARY.
                  05  H02-HENREILIST-ROW       PIC S9(9)   BINARY.
                  05  H02-HENREILIST-ITEM      OCCURS  30 TIMES.
                    06  H02-TRENNUM            PIC X(2).
                    06  H02-TSYORIYM           PIC X(7).
                    06  H02-TTEISYUTUSAKI2     PIC X(2).
                    06  H02-TNYUGAIKBN         PIC X(2).
                    06  H02-TRECESYUBETU       PIC X(10).
                    06  H02-THKNJANUM          PIC X(8).
                    06  H02-TKIGO              PIC X(20).
                    06  H02-TRECEIPT-KBN       PIC X(4).
                    06  H02-TDISASTER          PIC X(2).
                    06  H02-TERR               PIC X(100).
                    06  H02-TS-MAKE-FLG        PIC X(2).
                    06  H02-TS-SKYYM           PIC X(6).
                    06  H02-TS-NYUGAIKBN       PIC X(2).
                    06  H02-TS-RECESYUBETU     PIC X(10).
                    06  H02-TS-HKNJANUM        PIC X(8).
                    06  H02-TS-KIGO            PIC X(20).
                    06  H02-TS-HOJOKBN         PIC X(2).
                    06  H02-TS-RECEIPT-KBN     PIC X(4).
                    06  H02-TS-DISASTER        PIC X(2).
                    06  H02-TS-HENREI-SIJI-FLG PIC X(2).
                  05  H02-HENREILIST-SELECT
                          PIC X       OCCURS  30 TIMES.
