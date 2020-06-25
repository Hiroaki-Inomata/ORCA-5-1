            02  R06.
              03  R06-PRTNM-COMBO.
                04  R06-PRTNM.
                  05  R06-VALUE   PIC X(19).
                04  R06-PRTNM-COUNT   PIC S9(9)   BINARY.
                04  R06-PRTNMLST
                        PIC X(19)   OCCURS  100 TIMES.
              03  R06-PRTNM-N-COMBO.
                04  R06-PRTNM-N.
                  05  R06-VALUE   PIC X(19).
                04  R06-PRTNM-N-COUNT   PIC S9(9)   BINARY.
                04  R06-PRTNM-NLST
                        PIC X(19)   OCCURS  100 TIMES.
              03  R06-SETTEI1-STATE   PIC S9(09) BINARY.
              03  R06-SETTEI1         PIC X.
              03  R06-SETTEI2-STATE   PIC S9(09) BINARY.
              03  R06-SETTEI2         PIC X.
