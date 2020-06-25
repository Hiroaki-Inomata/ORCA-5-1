        02  N01.
      *      02  N01-FIXED6.
              03  N01-HKNJANUM.
                04  N01-VALUE   PIC X(8).
              03  N01-HKNJANAME.
                04  N01-VALUE   PIC X(100).
              03  N01-HKNJANAMETAN1.
                04  N01-VALUE   PIC X(50).
              03  N01-HKNJANAMETAN2.
                04  N01-VALUE   PIC X(20).
              03  N01-HKNJANAMETAN3.
                04  N01-VALUE   PIC X(20).
              03  N01-HKNNUMCOMBO.
                04  N01-HKNNUM.
                  05  N01-VALUE   PIC X(30).
                04  N01-HKNNUM-COUNT   PIC S9(9)   BINARY.
                04  N01-HKNNUM-ITEM
                        PIC X(30)   OCCURS  100 TIMES.
              03  N01-HGKYURATECOMBO.
                04  N01-HGKYURATE.
                  05  N01-VALUE   PIC X(30).
                04  N01-HGKYURATE-STATE PIC S9(9)   BINARY.
                04  N01-HGKYURATE-COUNT PIC S9(9)   BINARY.
                04  N01-HGKYURATE-ITEM
                        PIC X(30)   OCCURS  4 TIMES.
              03  N01-HNKYURATECOMBO.
                04  N01-HNKYURATE.
                  05  N01-VALUE   PIC X(30).
                04  N01-HNKYURATE-STATE PIC S9(9)   BINARY.
                04  N01-HNKYURATE-COUNT PIC S9(9)   BINARY.
                04  N01-HNKYURATE-ITEM
                        PIC X(30)   OCCURS  4 TIMES.
              03  N01-KGKYURATECOMBO.
                04  N01-KGKYURATE.
                  05  N01-VALUE   PIC X(30).
                04  N01-KGKYURATE-STATE   PIC S9(9)   BINARY.
                04  N01-KGKYURATE-COUNT   PIC S9(9)   BINARY.
                04  N01-KGKYURATE-ITEM
                        PIC X(30)   OCCURS  4 TIMES.
              03  N01-KNKYURATECOMBO.
                04  N01-KNKYURATE.
                  05  N01-VALUE   PIC X(30).
                04  N01-KNKYURATE-STATE   PIC S9(9)   BINARY.
                04  N01-KNKYURATE-COUNT   PIC S9(9)   BINARY.
                04  N01-KNKYURATE-ITEM
                        PIC X(30)   OCCURS  4 TIMES.
              03  N01-POST.
                04  N01-VALUE   PIC X(7).
              03  N01-ADRS.
                04  N01-VALUE   PIC X(100).
              03  N01-BANTI.
                04  N01-VALUE   PIC X(100).
              03  N01-TEL.
                04  N01-VALUE   PIC X(15).
              03  N01-KIGO.
                04  N01-VALUE   PIC X(40).
              03  N01-IDOKBNCOBO.
                04  N01-IDOKBN.
                  05  N01-VALUE   PIC X(30).
                04  N01-IDOKBN-COUNT   PIC S9(9)   BINARY.
                04  N01-IDOKBN-ITEM
                        PIC X(30)   OCCURS  10 TIMES.
              03  N01-IDOYMD.
                04  N01-VALUE   PIC X(10).
              03  N01-MIDASHI.
                04  N01-MIDASHI-STYLE   PIC X(20).
                04  N01-MIDASHI-DATA    PIC X(20).
              03  N01-MIDASHI02.
                04  N01-MIDASHI02-STYLE   PIC X(20).
                04  N01-MIDASHI02-DATA    PIC X(20).
              03  N01-B03.
                04  N01-B03-STATE   PIC S9(9)   BINARY.
              03  N01-B12.
                04  N01-B12-STATE   PIC S9(9)   BINARY.

