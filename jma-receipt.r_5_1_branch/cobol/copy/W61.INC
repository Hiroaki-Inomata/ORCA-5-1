          02  W61.
              03  W61-SCROLLEDWINDOW28.
                04  W61-TOKKILST.
                  05  W61-COUNT   PIC S9(9)   BINARY.
                  05  W61-ROW PIC S9(9)   BINARY.
                  05  W61-ROWATTR     PIC S9(9)   BINARY.
                  05  W61-ITEM            OCCURS  200 TIMES.
                    06  W61-TRENNUM       PIC X(3).
                    06  W61-THKNNUM       PIC X(3).
                    06  W61-TSEIDONAME    PIC X(50).
                    06  W61-TTEISYUTUSAKI PIC X(10).
                    06  W61-THKNJANUM     PIC X(8).
                    06  W61-TFTNJANUMKBN-JYOKEN
                                          PIC X(10).
                    06  W61-TNYUGAIKBN    PIC X(06).
                    06  W61-TSYOTOKUKBN   PIC X(10).
                    06  W61-TRJNKBN       PIC X(10).
                    06  W61-TZAITAKU      PIC X(10).
                    06  W61-TSTYUKYMD     PIC X(9).
                    06  W61-TEDYUKYMD     PIC X(9).
                  05  W61-SELECT
                          PIC X       OCCURS  200 TIMES.
              03  W61-SELNUM.
                04  W61-VALUE   PIC S9(3).
              03  W61-STYUKYMD.
                04  W61-VALUE   PIC X(10).
              03  W61-EDYUKYMD.
                04  W61-VALUE   PIC X(10).
              03  W61-HKNNUM.
                04  W61-VALUE   PIC X(3).
              03  W61-SEIDONAME.
                04  W61-VALUE   PIC X(50).
              03  W61-TEISYUTUSAKI-COMBO.
                04  W61-TEISYUTUSAKI         PIC X(12).
                04  W61-TEISYUTUSAKI-COUNT   PIC S9(9)   BINARY.
                04  W61-TEISYUTUSAKI-ITEM
                                          PIC X(12)   OCCURS  5 TIMES.
              03  W61-HKNJANUM.
                04  W61-VALUE   PIC X(8).
              03  W61-HKNJANAME.
                04  W61-VALUE   PIC X(100).
              03  W61-FTNJANUMKBN-JYOKEN-COMBO.
                04  W61-FTNJANUMKBN-JYOKEN.
                  05  W61-VALUE   PIC X(32).
                04  W61-FTNJANUMKBN-JYOKEN-COUNT   PIC S9(9)   BINARY.
                04  W61-FTNJANUMKBN-JYOKEN-ITEM
                        PIC X(32)   OCCURS  5 TIMES.
              03  W61-FTNJANUM-JYOKEN.
                04  W61-VALUE   PIC X(8).
              03  W61-NYUGAIKBN-COMBO.
                04  W61-NYUGAIKBN      PIC X(10).
                04  W61-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  5 TIMES.
              03  W61-SYOTOKUKBN-COMBO.
                04  W61-SYOTOKUKBN      PIC X(20).
                04  W61-SYOTOKUKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-SYOTOKUKBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  W61-ZAITAKU-COMBO.
                04  W61-ZAITAKU         PIC X(44).
                04  W61-ZAITAKU-COUNT   PIC S9(9)   BINARY.
                04  W61-ZAITAKU-ITEM
                        PIC X(44)   OCCURS  5 TIMES.
              03  W61-RJNKBN-COMBO.
                04  W61-RJNKBN         PIC X(52).
                04  W61-RJNKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-RJNKBN-ITEM
                        PIC X(52)   OCCURS  5 TIMES.
              03  W61-TOKKI.
                04  W61-VALUE   PIC X(12).
              03  W61-KYURATE.
                04  W61-VALUE   PIC S9(3).
              03  W61-FTNJANUMKBN-COMBO.
                04  W61-FTNJANUMKBN         PIC X(25).
                04  W61-FTNJANUMKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-FTNJANUMKBN-ITEM
                        PIC X(25)   OCCURS  5 TIMES.
              03  W61-FTNJANUM.
                04  W61-VALUE   PIC X(8).
              03  W61-JKYSNUMKBN-COMBO.
                04  W61-JKYSNUMKBN         PIC X(25).
                04  W61-JKYSNUMKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-JKYSNUMKBN-ITEM
                        PIC X(25)   OCCURS  5 TIMES.
              03  W61-JKYSNUM.
                04  W61-VALUE   PIC X(7).
              03  W61-TEKIYOU-COMMENT1.
                04  W61-VALUE   PIC X(52).
              03  W61-TEKIYOU-COMMENT1KBN-COMBO.
                04  W61-TEKIYOU-COMMENT1KBN         PIC X(20).
                04  W61-TEKIYOU-COMMENT1KBN-COUNT   PIC S9(9)   BINARY.
                04  W61-TEKIYOU-COMMENT1KBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  W61-TEKIYOU-COMMENT2.
                04  W61-VALUE   PIC X(52).
              03  W61-TEKIYOU-COMMENT2KBN-COMBO.
                04  W61-TEKIYOU-COMMENT2KBN         PIC X(20).
                04  W61-TEKIYOU-COMMENT2KBN-COUNT   PIC S9(9)   BINARY.
                04  W61-TEKIYOU-COMMENT2KBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  W61-RECEDEN-TOKKI.
                04  W61-VALUE   PIC X(2).
              03  W61-RECEDEN-KYURATE.
                04  W61-VALUE   PIC S9(3).
              03  W61-RECEDEN-KORECKBN-COMBO.
                04  W61-RECEDEN-KORECKBN         PIC X(12).
                04  W61-RECEDEN-KORECKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-RECEDEN-KORECKBN-ITEM
                        PIC X(12)   OCCURS  5 TIMES.
              03  W61-RECEDEN-FTNJANUMKBN-COMBO.
                04  W61-RECEDEN-FTNJANUMKBN         PIC X(25).
                04  W61-RECEDEN-FTNJANUMKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-RECEDEN-FTNJANUMKBN-ITEM
                        PIC X(25)   OCCURS  5 TIMES.
              03  W61-RECEDEN-FTNJANUM  PIC X(8).
              03  W61-RECEDEN-JKYSNUMKBN-COMBO.
                04  W61-RECEDEN-JKYSNUMKBN         PIC X(25).
                04  W61-RECEDEN-JKYSNUMKBN-COUNT   PIC S9(9)   BINARY.
                04  W61-RECEDEN-JKYSNUMKBN-ITEM
                        PIC X(25)   OCCURS  5 TIMES.
              03  W61-RECEDEN-JKYSNUM   PIC X(7).
