            02  C11.
              03  C11-PTNUM.
                04  C11-VALUE   PIC X(20).
              03  C11-SYSYMD.
                04  C11-VALUE   PIC X(9).
              03  C11-KANANAME.
                04  C11-VALUE   PIC X(30).
              03  C11-NAME.
                04  C11-VALUE   PIC X(30).
              03  C11-SEX.
                04  C11-VALUE   PIC X(2).
              03  C11-BIRTHDAY.
                04  C11-VALUE   PIC X(12).
              03  C11-AGE.
                04  C11-VALUE   PIC X(6).
              03  C11-SELNUM1.
                04  C11-VALUE   PIC S9(3).
              03  C11-OPENAME.
                04  C11-VALUE   PIC X(50).
              03  C11-SRYYMD.
                04  C11-VALUE   PIC X(9).
              03  C11-TEKSTYM.
                04  C11-VALUE   PIC X(6).
              03  C11-TEKEDYM.
                04  C11-VALUE   PIC X(6).
              03  C11-SELNUM2.
                04  C11-VALUE   PIC S9(3).
              03  C11-OPECOMMENT.
                04  C11-VALUE   PIC X(50).
              03  C11-SRYKA-COMBO.
                04  C11-SRYKA         PIC X(23).
                04  C11-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  C11-SRYKA-ITEM    PIC X(23)   OCCURS  100 TIMES.
              03  C11-DSPYM-COMBO.
                04  C11-DSPYM         PIC X(12).
                04  C11-DSPYM-COUNT   PIC S9(9)   BINARY.
                04  C11-DSPYM-ITEM    PIC X(12)   OCCURS  6 TIMES.
              03  C11-NYUGAIKBN-COMBO.
                04  C11-NYUGAIKBN     PIC X(8).
                04  C11-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  C11-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  2 TIMES.
              03  C11-HKNNUM-COMBO.
                04  C11-HKNNUM  PIC X(18).
                04  C11-HKNNUM-COUNT   PIC S9(9)   BINARY.
                04  C11-HKNNUM-ITEM
                        PIC X(18)   OCCURS  5 TIMES.
              03  C11-BSRYKA-COMBO.
                04  C11-BSRYKA   PIC X(23).
                04  C11-BSRYKA-COUNT   PIC S9(9)   BINARY.
                04  C11-BSRYKA-ITEM
                        PIC X(23)   OCCURS  100 TIMES.
              03  C11-SCROLLEDWINDOW1.
                04  C11-OPELIST.
                  05  C11-OPE-COUNT   PIC S9(9)   BINARY.
                  05  C11-OPE-ROW PIC S9(9)   BINARY.
                  05  C11-OPE-ROWATTR PIC S9(9)   BINARY.
                  05  C11-OPE-ITEM        OCCURS  100 TIMES.
                    06  C11-OPE-TNUM      PIC X(3).
                    06  C11-OPE-TSRYKA    PIC X(20).
                    06  C11-OPE-TOPENAME  PIC X(50).
                    06  C11-OPE-TSRYYMD   PIC X(9).
                    06  C11-OPE-THKNCOMBI PIC X(40).
                  05  C11-OPE-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  C11-SCROLLEDWINDOW2.
                04  C11-COMMENTLIST.
                  05  C11-COM-COUNT   PIC S9(9)   BINARY.
                  05  C11-COM-ROW PIC S9(9)   BINARY.
                  05  C11-COM-ROWATTR PIC S9(9)   BINARY.
                  05  C11-COM-ITEM        OCCURS  100 TIMES.
                    06  C11-COM-TNUM       PIC X(3).
                    06  C11-COM-TSRYKA     PIC X(20).
                    06  C11-COM-TOPENAME   PIC X(50).
                    06  C11-COM-TSRYYMD    PIC X(9).
                    06  C11-COM-TNYUGAIKBN PIC X(6).
                    06  C11-COM-THKNNUM    PIC X(14).
                    06  C11-COM-TYUKYM     PIC X(16).
                  05  C11-COM-SELECT
                          PIC X       OCCURS  100 TIMES.
