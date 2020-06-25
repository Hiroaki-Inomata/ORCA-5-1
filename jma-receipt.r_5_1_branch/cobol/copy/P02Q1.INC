        02  P02Q1.
      ***** 02  P02Q1-FIXED1.
              03  P02Q1-SELNUM.
                04  P02Q1-VALUE PIC S9(3).
              03  P02Q1-NAME.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-SEX.
                04  P02Q1-VALUE PIC X(2).
              03  P02Q1-BIRTHDAY.
                04  P02Q1-VALUE PIC X(10).
              03  P02Q1-ADRS.
                04  P02Q1-VALUE PIC X(100).
              03  P02Q1-TEL.
                04  P02Q1-VALUE PIC X(15).
              03  P02Q1-HKNJANUM.
                04  P02Q1-VALUE PIC X(8).
              03  P02Q1-KIGONUM.
                04  P02Q1-VALUE PIC X(20).
              03  P02Q1-PTNUM.
                04  P02Q1-VALUE PIC X(20).
              03  P02Q1-KIKAN.
                04  P02Q1-VALUE PIC X(20).
              03  P02Q1-QR-HKNJANUM.
                04  P02Q1-VALUE PIC X(8).
              03  P02Q1-QR-KIGONUM.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-QR-BIRTHDAY.
                04  P02Q1-VALUE PIC X(10).
              03  P02Q1-QR-SEX.
                04  P02Q1-VALUE PIC X(2).
              03  P02Q1-QR-KANANAME.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-QR-NAME.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-HONKZKKBN.
                04  P02Q1-VALUE PIC X(4).
              03  P02Q1-QR-HONKZKKBN.
                04  P02Q1-VALUE PIC X(4).
              03  P02Q1-HKNNUMMEI.
                04  P02Q1-VALUE PIC X(16).
              03  P02Q1-QR-HKNNUMMEI.
                04  P02Q1-VALUE PIC X(16).
              03  P02Q1-HKNJANAME.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-QR-HKNJANAME.
                04  P02Q1-VALUE PIC X(50).
              03  P02Q1-SCROLLEDWINDOW1.
                04  P02Q1-HPOS  PIC S9(9)   BINARY.
                04  P02Q1-VPOS  PIC S9(9)   BINARY.
                04  P02Q1-DUALLIST.
                  05  P02Q1-COUNT PIC S9(9)   BINARY.
                  05  P02Q1-ROW   PIC S9(9)   BINARY.
                  05  P02Q1-ITEM      OCCURS  50 TIMES.
                    06  P02Q1-TNUM      PIC X(3).
                    06  P02Q1-TPTNUM    PIC X(20).
                    06  P02Q1-TNAME     PIC X(50).
                    06  P02Q1-TBIRTHDAY   PIC X(9).
                    06  P02Q1-TSEX    PIC X(2).
                    06  P02Q1-TADRS     PIC X(100).
                    06  P02Q1-TTEL      PIC X(15).
                  05  P02Q1-SELECT
                          PIC X       OCCURS  50 TIMES.
