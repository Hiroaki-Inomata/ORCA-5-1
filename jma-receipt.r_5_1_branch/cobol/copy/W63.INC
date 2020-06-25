        02  W63.
              03  W63-B06-STATE PIC S9(9)   BINARY.
              03  W63-SELNUM    PIC S9(2).
              03  W63-STYUKYMD.
                04  W63-VALUE   PIC X(10).
              03  W63-EDYUKYMD.
                04  W63-VALUE   PIC X(10).
              03  W63-HKNJANUM.
                04  W63-VALUE   PIC X(10).
              03  W63-HKNJANAME.
                04  W63-VALUE   PIC X(100).
              03  W63-CMB-JYKNKBN.
                04  W63-JYKNKBN PIC X(16).
                04  W63-JYKNKBN-COUNT PIC S9(9)    BINARY.
                04  W63-JYKNKBN-ITEM  PIC X(16)    OCCURS  5 TIMES.
              03  W63-CMB-HKNNUM.
                04  W63-HKNNUM         PIC X(20).
                04  W63-HKNNUM-COUNT   PIC S9(9)   BINARY.
                04  W63-HKNNUM-ITEM    PIC X(20)   OCCURS  50 TIMES.
              03  W63-CMB-KGCHKPTN.
                04  W63-KGCHKPTN       PIC X(40).
                04  W63-KGCHKPTN-COUNT PIC S9(9) BINARY.
                04  W63-KGCHKPTN-ITEM  PIC X(40) OCCURS  5 TIMES.
              03  W63-CMB-KGSUJI.
                04  W63-KGSUJI         PIC X(20).
                04  W63-KGSUJI-COUNT   PIC S9(9)   BINARY.
                04  W63-KGSUJI-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-KGKANJI.
                04  W63-KGKANJI        PIC X(20).
                04  W63-KGKANJI-COUNT  PIC S9(9)  BINARY.
                04  W63-KGKANJI-ITEM   PIC X(20)  OCCURS  5 TIMES.
              03  W63-CMB-KGABT.
                04  W63-KGABT          PIC X(20).
                04  W63-KGABT-COUNT    PIC S9(9)  BINARY.
                04  W63-KGABT-ITEM     PIC X(20)  OCCURS  5 TIMES.
              03  W63-CMB-KGHYPN.
                04  W63-KGHYPN         PIC X(20).
                04  W63-KGHYPN-COUNT   PIC S9(9)   BINARY.
                04  W63-KGHYPN-ITEM    PIC X(20)   OCCURS 5 TIMES.
              03  W63-CMB-KGKAKKO.
                04  W63-KGKAKKO        PIC X(20).
                04  W63-KGKAKKO-COUNT  PIC S9(9)   BINARY.
                04  W63-KGKAKKO-ITEM   PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-KGETC.
                04  W63-KGETC          PIC X(20).
                04  W63-KGETC-COUNT    PIC S9(9)   BINARY.
                04  W63-KGETC-ITEM     PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-KGKANJINASHI.
                04  W63-KGKANJINASHI   PIC X(20).
                04  W63-KGKANJINASHI-COUNT PIC S9(9)   BINARY.
                04  W63-KGKANJINASHI-ITEM  
                                       PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-KGKETACHK.
                04  W63-KGKETACHK       PIC X(20).
                04  W63-KGKETACHK-COUNT PIC S9(9)   BINARY.
                04  W63-KGKETACHK-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-KGKETAMIN.
                04  W63-KGKETAMIN       PIC X(21).
                04  W63-KGKETAMIN-STATE PIC S9(9)   BINARY.
                04  W63-KGKETAMIN-COUNT PIC S9(9)   BINARY.
                04  W63-KGKETAMIN-ITEM  PIC X(21)   OCCURS  25 TIMES.
                04  W63-KGKETAMIN-ITEM-STATE PIC S9(9)   BINARY.
              03  W63-CMB-NM1SUJI.
                04  W63-NM1SUJI        PIC X(20).
                04  W63-NM1SUJI-COUNT  PIC S9(9)   BINARY.
                04  W63-NM1SUJI-ITEM   PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM1KANJI.
                04  W63-NM1KANJI       PIC X(20).
                04  W63-NM1KANJI-COUNT PIC S9(9)   BINARY.
                04  W63-NM1KANJI-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM1ABT.
                04  W63-NM1ABT         PIC X(20).
                04  W63-NM1ABT-COUNT   PIC S9(9)   BINARY.
                04  W63-NM1ABT-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM1HYPN.
                04  W63-NM1HYPN        PIC X(20).
                04  W63-NM1HYPN-COUNT  PIC S9(9)   BINARY.
                04  W63-NM1HYPN-ITEM   PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM1KAKKO.
                04  W63-NM1KAKKO       PIC X(20).
                04  W63-NM1KAKKO-COUNT PIC S9(9)   BINARY.
                04  W63-NM1KAKKO-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM1ETC.
                04  W63-NM1ETC         PIC X(20).
                04  W63-NM1ETC-COUNT   PIC S9(9)   BINARY.
                04  W63-NM1ETC-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NMSUJINASHI.
                04  W63-NMSUJINASHI    PIC X(20).
                04  W63-NMSUJINASHI-COUNT PIC S9(9)   BINARY.
                04  W63-NMSUJINASHI-ITEM
                                       PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NMKETACHK.
                04  W63-NMKETACHK       PIC X(20).
                04  W63-NMKETACHK-COUNT PIC S9(9)   BINARY.
                04  W63-NMKETACHK-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NMKETAMIN.
                04  W63-NMKETAMIN       PIC X(21).
                04  W63-NMKETAMIN-STATE PIC S9(9)   BINARY.
                04  W63-NMKETAMIN-COUNT PIC S9(9)   BINARY.
                04  W63-NMKETAMIN-ITEM  PIC X(21)   OCCURS  15 TIMES.
                04  W63-NMKETAMIN-ITEM-STATE PIC S9(9)   BINARY.
              03  W63-CMB-NM2SUJI.
                04  W63-NM2SUJI        PIC X(20).
                04  W63-NM2SUJI-COUNT  PIC S9(9)   BINARY.
                04  W63-NM2SUJI-ITEM   PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM2KANJI.
                04  W63-NM2KANJI       PIC X(20).
                04  W63-NM2KANJI-COUNT PIC S9(9)   BINARY.
                04  W63-NM2KANJI-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM2ABT.
                04  W63-NM2ABT         PIC X(20).
                04  W63-NM2ABT-COUNT   PIC S9(9)   BINARY.
                04  W63-NM2ABT-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM2HYPN.
                04  W63-NM2HYPN        PIC X(20).
                04  W63-NM2HYPN-COUNT  PIC S9(9)   BINARY.
                04  W63-NM2HYPN-ITEM   PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM2KAKKO.
                04  W63-NM2KAKKO       PIC X(20).
                04  W63-NM2KAKKO-COUNT PIC S9(9)   BINARY.
                04  W63-NM2KAKKO-ITEM  PIC X(20)   OCCURS  5 TIMES.
              03  W63-CMB-NM2ETC.
                04  W63-NM2ETC         PIC X(20).
                04  W63-NM2ETC-COUNT   PIC S9(9)   BINARY.
                04  W63-NM2ETC-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  W63-CLISTSCROLL.
                04  W63-CLISTSCROLL-HPOS   PIC S9(9)   BINARY.
                04  W63-CLISTSCROLL-VPOS   PIC S9(9)   BINARY.
                04  W63-CHECKLIST.
                  05  W63-CHECKLIST-COUNT   PIC S9(9)   BINARY.
                  05  W63-CHECKLIST-ROW     PIC S9(9)   BINARY.
                  05  W63-CHECKLIST-ROWATTR PIC S9(9)   BINARY.
                  05  W63-CHECKLIST-ITEM    OCCURS  200 TIMES.
                    06  W63-TNUM       PIC X(3).
                    06  W63-THKNJANUM  PIC X(9).
                    06  W63-TJYKNKBN   PIC X(4).
                    06  W63-THKNJANAME PIC X(60).
                    06  W63-THKNNUM    PIC X(20).
                    06  W63-TKIGOCHK   PIC X(10).
                    06  W63-TSTYUKYMD  PIC X(9).
                    06  W63-TEDYUKYMD  PIC X(9).
                  05  W63-CHECKLIST-SELECT
                          PIC X        OCCURS  200 TIMES.
