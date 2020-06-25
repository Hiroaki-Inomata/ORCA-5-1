        02  W30.
     ****** 02  W30-FIXED2.
              03  W30-B03.
                04  W30-B03-STATE   PIC S9(9)   BINARY.
              03  W30-STYUKYMD.
                04  W30-VALUE   PIC X(9).
              03  W30-EDYUKYMD.
                04  W30-VALUE   PIC X(9).
              03  W30-SYOHOUHEAD-G   OCCURS   4.
                04  W30-SYOHOUHEAD   PIC X(30).
              03  W30-CHUSYAHEAD-G   OCCURS   4.
                04  W30-CHUSYAHEAD   PIC X(30).
              03  W30-SIJISENHEAD-G  OCCURS   4.
                04  W30-SIJISENHEAD   PIC X(30).
              03  W30-MAYAKUHEAD-G   OCCURS   4.
                04  W30-MAYAKUHEAD   PIC X(30).
              03  W30-SYOHOUPRT-G   OCCURS  4.
                04  W30-SYOHOUPRT   PIC S9(2).
              03  W30-CHUSYAPRT-G   OCCURS  4.
                04  W30-CHUSYAPRT   PIC S9(2).
              03  W30-SIJISENPRT-G  OCCURS  4.
                04  W30-SIJISENPRT   PIC S9(2).
              03  W30-MAYAKUPRT-G   OCCURS  4.
                04  W30-MAYAKUPRT   PIC S9(2).
              03  W30-SIJISENKBN-COMB.
                04  W30-SIJISENKBN.
                  05  W30-VALUE   PIC X(12).
                04  W30-SIJISENKBN-COUNT   PIC S9(9)   BINARY.
                04  W30-SIJISENKBN-ITEM
                        PIC X(12)   OCCURS  2 TIMES.
              03  W30-CHUMAYAHEAD-G   OCCURS  4.
                04  W30-CHUMAYAHEAD   PIC X(30).
              03  W30-CHUMAYAPRT-G   OCCURS  4.
                04  W30-CHUMAYAPRT   PIC S9(2).
