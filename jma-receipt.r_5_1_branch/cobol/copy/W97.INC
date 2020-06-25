        02  W97.
    ******* 02  W97-FIXED1.
              03  W97-SHELL-TBL     OCCURS   10.
                04  W97-SHELL   PIC X(100).
              03  W97-SYORINM-TBL   OCCURS   10.
                04  W97-SYORINM   PIC X(30).
              03  W97-LBLREPOS-NAME-TBL   OCCURS   10.
                04  W97-LBLREPOS-NAME   PIC X(20).
              03  W97-REPOS-NAME-TBL   OCCURS   10.
                04  W97-REPOS-NAME-VISIBLE   PIC X.
                04  W97-REPOS-NAME           PIC X(128).
              03  W97-PRTNM-TBL     OCCURS   10.
                04  W97-PRTNM   PIC X(16).
              03  W97-GYOUMUMEI-COMB.
                04  W97-GYOUMUMEI.
                  05  W97-VALUE   PIC X(40).
                04  W97-GYOUMUMEI-COUNT   PIC S9(9)   BINARY.
                04  W97-GYOUMUMEI-ITEM
                        PIC X(40)   OCCURS  25 TIMES.
              03  W97-TYPE-TBL      OCCURS  10.
                04  W97-TYPE-COMB.
                  05  W97-TYPE.
                    07  W97-VALUE   PIC X(20).
                  05  W97-TYPE-COUNT   PIC S9(9)   BINARY.
                  05  W97-TYPE-ITEM
                        PIC X(20)   OCCURS  2 TIMES.
              03  W97-EXECUTE-TBL     OCCURS  10.
                04  W97-EXECUTE-VALUE   PIC X.
                04  W97-EXECUTE-STATE   PIC S9(9)   BINARY.
                04  W97-EXECUTE-LABEL   PIC X(10).
              03  W97-GYOUMUFLG-COMB.
                04  W97-GYOUMUFLG.
                  05  W97-VALUE   PIC X(20).
                04  W97-GYOUMUFLG-COUNT   PIC S9(9)   BINARY.
                04  W97-GYOUMUFLG-ITEM
                        PIC X(20)   OCCURS  2 TIMES.
              03  W97-LBLMSG01-STYLE    PIC X(20).
              03  W97-LBLMSG01          PIC X(80).
