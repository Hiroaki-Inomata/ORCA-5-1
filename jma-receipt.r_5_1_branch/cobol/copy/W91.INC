        02  W91.
            03  W91-TITLE   PIC X(256).
            03  W91-POPUP-SUMMARY   PIC X(256).
            03  W91-POPUP-BODY  PIC X(1024).
            03  W91-POPUP-ICON  PIC X(256).
            03  W91-POPUP-TIMEOUT   PIC S9(9)   BINARY.
            03  W91-FIXED1.
              04  W91-FILEDIR.
                05  W91-VALUE   PIC X(200).
              04  W91-COOPBASE-COMB.
                05  W91-COOPBASE.
                  07  W91-VALUE   PIC X(20).
                05  W91-COOPBASE-COUNT   PIC S9(9)   BINARY.
                05  W91-COOPBASE-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
              04  W91-LABEL1.
                05  W91-TEXTDATA    PIC X(50).
              04  W91-LABEL2.
                05  W91-TEXTDATA    PIC X(50).
              04  W91-CHKSYREC.
                06  W91-CHKSYREC-VALUE   PIC X.
                06  W91-CHKSYREC-STATE   PIC S9(9)   BINARY.
              04  W91-CHKJIHI.
                06  W91-CHKJIHI-VALUE    PIC X.
                06  W91-CHKJIHI-STATE    PIC S9(9)   BINARY.
              04  W91-CHKPTNUM.
                06  W91-CHKPTNUM-VALUE   PIC X.
                06  W91-CHKPTNUM-STATE   PIC S9(9)   BINARY.
              04  W91-CHKSYBYO.
                06  W91-CHKSYBYO-VALUE   PIC X.
                06  W91-CHKSYBYO-STATE   PIC S9(9)   BINARY.
              04  W91-CHKNMREC.
                06  W91-CHKNMREC-VALUE   PIC X.
                06  W91-CHKNMREC-STATE   PIC S9(9)   BINARY.
              04  W91-COOPBASE2-COMB.
                05  W91-COOPBASE2.
                  07  W91-VALUE   PIC X(20).
                05  W91-COOPBASE2-STATE  PIC S9(9)   BINARY.
                05  W91-COOPBASE2-COUNT  PIC S9(9)   BINARY.
                05  W91-COOPBASE2-ITEM
                        PIC X(20)   OCCURS  10 TIMES.
              04  W91-LABEL3.
                05  W91-LABEL3-STATE     PIC S9(9)   BINARY.
              04  W91-LABEL4.
                05  W91-LABEL4-STATE     PIC S9(9)   BINARY.
