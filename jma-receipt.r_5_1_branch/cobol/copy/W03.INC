        02  W03.
      ****  02  W03-FIXED2.
              03  W03-B03.
                04  W03-B03-STATE   PIC S9(9)   BINARY.
              03  W03-B10.
                04  W03-B10-STATE   PIC S9(9)   BINARY.
                04  W03-B10-LABEL   PIC X(10).
              03  W03-CD01-G      OCCURS  25.
                04  W03-CD-STYLE   PIC X(20).
                04  W03-CD.
                  06  W03-VALUE   PIC X(4).
              03  W03-DSPNAME-G     OCCURS  25.
                04  W03-DSPNAME-STYLE   PIC X(20).
                04  W03-DSPNAME   PIC X(100).
              03  W03-STYUKYMD.
                04  W03-VALUE   PIC X(9).
              03  W03-EDYUKYMD.
                04  W03-VALUE   PIC X(9).
              03  W03-SSTKIJUN-G  OCCURS  25.
                04  W03-SSTKIJUN   PIC S9(1).
              03  W03-SELCD.
                04  W03-VALUE   PIC X(4).
              03  W03-SRYCD.
                04  W03-VALUE   PIC X(09).
              03  W03-MEISYO.
                04  W03-VALUE   PIC X(100).
              03  W03-KIJUNCD-G     OCCURS  10.
                  04  W03-KIJUNCD   PIC S9(4).
              03  W03-TEIGENCD.
                04  W03-VALUE   PIC S9(4).
              03  W03-YUKOKIKAN.
                  04  W03-VALUE   PIC X(30).
              03  W03-MIDASI.
                  04  W03-MIDASI-STYLE   PIC X(20).
                  04  W03-MIDASI-DATA    PIC X(30).
