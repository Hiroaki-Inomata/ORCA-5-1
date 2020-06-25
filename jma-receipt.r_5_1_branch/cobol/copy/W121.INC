            02  W121.
              03  W121-SELNUM1.
                04  W121-VALUE  PIC S9(2).
              03  W121-STYUKYMD.
                04  W121-VALUE  PIC X(10).
              03  W121-EDYUKYMD.
                04  W121-VALUE  PIC X(10).
              03  W121-HKNJANUM-STATE   PIC S9(09) BINARY.
              03  W121-HKNJANUM.
                04  W121-VALUE  PIC X(8).
              03  W121-HKNJANAME.
                04  W121-VALUE  PIC X(20).
              03  W121-NT1.
                04  W121-PAGENO PIC S9(9)   BINARY.
                04  W121-FI8.
                  05  W121-TOP-STATE   PIC S9(09) BINARY.
                  05  W121-BOTTOM-STATE   PIC S9(09) BINARY.
                  05  W121-DEL-STATE   PIC S9(09) BINARY.
                  05  W121-SYUBETU-UP-STATE   PIC S9(09) BINARY.
                  05  W121-SYUBETU-DOWN-STATE   PIC S9(09) BINARY.
                  05  W121-KOH-UP-STATE   PIC S9(09) BINARY.
                  05  W121-KOH-DOWN-STATE   PIC S9(09) BINARY.
                  05  W121-HKNNUM-STATE   PIC S9(09) BINARY.
                  05  W121-HKNNUM.
                    06  W121-VALUE  PIC X(3).
                  05  W121-SEIDONAME.
                    06  W121-VALUE  PIC X(20).
                  05  W121-GRP-DSP-STYLE   PIC X(20).
                  05  W121-GRP-DSP.
                    06  W121-VALUE  PIC X(14).
                  05  W121-GRP1-NO-STATE   PIC S9(09) BINARY.
                  05  W121-GRP1-NO.
                    06  W121-VALUE  PIC X(3).
                  05  W121-GRP2-NO-STATE   PIC S9(09) BINARY.
                  05  W121-GRP2-NO.
                    06  W121-VALUE  PIC X(3).
                  05  W121-GRP-SETTEI   PIC X(12).
                  05  W121-GRP-KAIJYO   PIC X(12).
                  05  W121-RECESYUBETU-STYLE   PIC X(20).
                  05  W121-RECESYUBETU.
                    06  W121-VALUE  PIC X(4).
                  05  W121-SYUBETUNAME-STYLE   PIC X(20).
                  05  W121-SYUBETUNAME.
                    06  W121-VALUE  PIC X(40).
                  05  W121-SCR3.
                    06  W121-BODYLIST.
                      07  W121-BODYLIST-COUNT  PIC S9(9)   BINARY.
                      07  W121-BODYLIST-ROW    PIC S9(9)   BINARY.
                      07  W121-BODYLIST-ROWATTR    PIC S9(9)   BINARY.
                      07  W121-BODYLIST-ITEM       OCCURS  50 TIMES.
                        08  W121-BODYLIST-TNO PIC X(2).
                        08  W121-BODYLIST-TGRP1 PIC X(3).
                        08  W121-BODYLIST-TGRP2 PIC X(3).
                        08  W121-BODYLIST-TRECESYUBETU PIC X(4).
                        08  W121-BODYLIST-TSYUBETUNAME PIC X(40).
                        08  W121-BODYLIST-TSETTEI PIC X(6).
                      07  W121-BODYLIST-SELECT
                              PIC X       OCCURS  50 TIMES.
                  05  W121-SCR4-STATE   PIC S9(09) BINARY.
                  05  W121-SCR4.
                    06  W121-KOHLIST.
                      07  W121-KOHLIST-COUNT  PIC S9(9)   BINARY.
                      07  W121-KOHLIST-ROW    PIC S9(9)   BINARY.
                      07  W121-KOHLIST-ROWATTR    PIC S9(9)   BINARY.
                      07  W121-KOHLIST-ITEM       OCCURS  100 TIMES.
                        08  W121-KOHLIST-TNO PIC X(3).
                        08  W121-KOHLIST-THKNNUM PIC X(3).
                        08  W121-KOHLIST-TSEIDONAME PIC X(20).
                        08  W121-KOHLIST-TJYUN PIC X(10).
                      07  W121-KOHLIST-SELECT
                              PIC X       OCCURS  100 TIMES.
                  05  W121-GRP1-STATE   PIC S9(09) BINARY.
                  05  W121-GRP1.
                    06  W121-VALUE  PIC X.
                  05  W121-GRP2-STATE   PIC S9(09) BINARY.
                  05  W121-GRP2.
                    06  W121-VALUE  PIC X.
                  05  W121-SYU-PRTJYUN2-COMBO.
                    06  W121-SYU-PRTJYUN2-STATE   PIC S9(09) BINARY.
                    06  W121-SYU-PRTJYUN2.
                      07  W121-VALUE  PIC X(31).
                    06  W121-SYU-PRTJYUN2-COUNT  PIC S9(9)   BINARY.
                    06  W121-SYU-PRTJYUN2LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W121-SYU-PRTJYUN1-COMBO.
                    06  W121-SYU-PRTJYUN1-STATE   PIC S9(09) BINARY.
                    06  W121-SYU-PRTJYUN1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-SYU-PRTJYUN1-COUNT  PIC S9(9)   BINARY.
                    06  W121-SYU-PRTJYUN1LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W121-PRTJYUN1-COMBO.
                    06  W121-PRTJYUN1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-PRTJYUN1-COUNT  PIC S9(9)   BINARY.
                    06  W121-PRTJYUN1LST
                            PIC X(31)   OCCURS  5 TIMES.
                  05  W121-PRTJYUN2-COMBO.
                    06  W121-PRTJYUN2.
                      07  W121-VALUE  PIC X(31).
                    06  W121-PRTJYUN2-COUNT  PIC S9(9)   BINARY.
                    06  W121-PRTJYUN2LST
                            PIC X(31)   OCCURS  5 TIMES.
                  05  W121-ZAITAKU-COMBO.
                    06  W121-ZAITAKU.
                      07  W121-VALUE  PIC X(26).
                    06  W121-ZAITAKU-COUNT  PIC S9(9)   BINARY.
                    06  W121-ZAITAKULST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W121-CHOUKI-COMBO.
                    06  W121-CHOUKI.
                      07  W121-VALUE  PIC X(26).
                    06  W121-CHOUKI-COUNT  PIC S9(9)   BINARY.
                    06  W121-CHOUKILST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W121-KOUGAKU-TEN.
                    06  W121-VALUE  PIC S9(7).
                  05  W121-SRT-LAST-COMBO.
                    06  W121-SRT-LAST.
                      07  W121-VALUE  PIC X(14).
                    06  W121-SRT-LAST-COUNT  PIC S9(9)   BINARY.
                    06  W121-SRT-LASTLST
                            PIC X(14)   OCCURS  5 TIMES.
                04  W121-FI7.
                  05  W121-TOP-1-STATE   PIC S9(09) BINARY.
                  05  W121-BOTTOM-1-STATE   PIC S9(09) BINARY.
                  05  W121-DEL-1-STATE   PIC S9(09) BINARY.
                  05  W121-SYUBETU-UP-1-STATE   PIC S9(09) BINARY.
                  05  W121-SYUBETU-DOWN-1-STATE   PIC S9(09) BINARY.
                  05  W121-KOH-UP-1-STATE   PIC S9(09) BINARY.
                  05  W121-KOH-DOWN-1-STATE   PIC S9(09) BINARY.
                  05  W121-HKNNUM-1-STATE   PIC S9(09) BINARY.
                  05  W121-HKNNUM-1.
                    06  W121-VALUE  PIC X(3).
                  05  W121-SEIDONAME-1.
                    06  W121-VALUE  PIC X(20).
                  05  W121-GRP-DSP-1-STYLE   PIC X(20).
                  05  W121-GRP-DSP-1.
                    06  W121-VALUE  PIC X(14).
                  05  W121-GRP1-NO-1-STATE   PIC S9(09) BINARY.
                  05  W121-GRP1-NO-1.
                    06  W121-VALUE  PIC X(3).
                  05  W121-GRP2-NO-1-STATE   PIC S9(09) BINARY.
                  05  W121-GRP2-NO-1.
                    06  W121-VALUE  PIC X(3).
                  05  W121-GRP-SETTEI-1   PIC X(12).
                  05  W121-GRP-KAIJYO-1   PIC X(12).
                  05  W121-RECESYUBETU-1-STYLE   PIC X(20).
                  05  W121-RECESYUBETU-1.
                    06  W121-VALUE  PIC X(4).
                  05  W121-SYUBETUNAME-1-STYLE   PIC X(20).
                  05  W121-SYUBETUNAME-1.
                    06  W121-VALUE  PIC X(40).
                  05  W121-SCR13.
                    06  W121-BODYLIST-1.
                      07  W121-BODYLIST-1-COUNT  PIC S9(9)   BINARY.
                      07  W121-BODYLIST-1-ROW    PIC S9(9)   BINARY.
                      07  W121-BODYLIST-1-ROWATTR    PIC S9(9)   BINARY.
                      07  W121-BODYLIST-1-ITEM       OCCURS  50 TIMES.
                        08  W121-BODYLIST-1-TNO PIC X(2).
                        08  W121-BODYLIST-1-TGRP1 PIC X(3).
                        08  W121-BODYLIST-1-TGRP2 PIC X(3).
                        08  W121-BODYLIST-1-TRECESYUBETU PIC X(4).
                        08  W121-BODYLIST-1-TSYUBETUNAME PIC X(40).
                        08  W121-BODYLIST-1-TSETTEI PIC X(6).
                      07  W121-BODYLIST-1-SELECT
                              PIC X       OCCURS  50 TIMES.
                  05  W121-SCR4-1-STATE   PIC S9(09) BINARY.
                  05  W121-SCR4-1.
                    06  W121-KOHLIST-1.
                      07  W121-KOHLIST-1-COUNT  PIC S9(9)   BINARY.
                      07  W121-KOHLIST-1-ROW    PIC S9(9)   BINARY.
                      07  W121-KOHLIST-1-ROWATTR    PIC S9(9)   BINARY.
                      07  W121-KOHLIST-1-ITEM       OCCURS  100 TIMES.
                        08  W121-KOHLIST-1-TNO PIC X(3).
                        08  W121-KOHLIST-1-THKNNUM PIC X(3).
                        08  W121-KOHLIST-1-TSEIDONAME PIC X(20).
                        08  W121-KOHLIST-1-TJYUN PIC X(10).
                      07  W121-KOHLIST-1-SELECT
                              PIC X       OCCURS  100 TIMES.
                  05  W121-GRP1-1-STATE   PIC S9(09) BINARY.
                  05  W121-GRP1-1.
                    06  W121-VALUE  PIC X.
                  05  W121-GRP2-1-STATE   PIC S9(09) BINARY.
                  05  W121-GRP2-1.
                    06  W121-VALUE  PIC X.
                  05  W121-SYU-PRTJYUN2-1-COMBO.
                    06  W121-SYU-PRTJYUN2-1-STATE   PIC S9(09) BINARY.
                    06  W121-SYU-PRTJYUN2-1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-SYU-PRTJYUN2-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-SYU-PRTJYUN2-1LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W121-SYU-PRTJYUN1-1-COMBO.
                    06  W121-SYU-PRTJYUN1-1-STATE   PIC S9(09) BINARY.
                    06  W121-SYU-PRTJYUN1-1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-SYU-PRTJYUN1-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-SYU-PRTJYUN1-1LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W121-PRTJYUN1-1-COMBO.
                    06  W121-PRTJYUN1-1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-PRTJYUN1-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-PRTJYUN1-1LST
                            PIC X(31)   OCCURS  5 TIMES.
                  05  W121-PRTJYUN2-1-COMBO.
                    06  W121-PRTJYUN2-1.
                      07  W121-VALUE  PIC X(31).
                    06  W121-PRTJYUN2-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-PRTJYUN2-1LST
                            PIC X(31)   OCCURS  5 TIMES.
                  05  W121-ZAITAKU-1-COMBO.
                    06  W121-ZAITAKU-1.
                      07  W121-VALUE  PIC X(26).
                    06  W121-ZAITAKU-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-ZAITAKU-1LST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W121-CHOUKI-1-COMBO.
                    06  W121-CHOUKI-1.
                      07  W121-VALUE  PIC X(26).
                    06  W121-CHOUKI-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-CHOUKI-1LST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W121-KOUGAKU-TEN-1.
                    06  W121-VALUE  PIC S9(7).
                  05  W121-SRT-LAST-1-COMBO.
                    06  W121-SRT-LAST-1.
                      07  W121-VALUE  PIC X(14).
                    06  W121-SRT-LAST-1-COUNT  PIC S9(9)   BINARY.
                    06  W121-SRT-LAST-1LST
                            PIC X(14)   OCCURS  5 TIMES.
      *
              03  W121-HKN-PRTJYUN-COMBO.
                04  W121-HKN-PRTJYUN-STATE   PIC S9(09) BINARY.
                04  W121-HKN-PRTJYUN.
                  05  W121-VALUE  PIC X(46).
                04  W121-HKN-PRTJYUN-COUNT  PIC S9(9)   BINARY.
                04  W121-HKN-PRTJYUNLST
                        PIC X(46)   OCCURS  6 TIMES.
              03  W121-SCR1.
                04  W121-HEADLIST.
                  05  W121-HEADLIST-COUNT  PIC S9(9)   BINARY.
                  05  W121-HEADLIST-ROW    PIC S9(9)   BINARY.
                  05  W121-HEADLIST-ROWATTR    PIC S9(9)   BINARY.
                  05  W121-HEADLIST-ITEM       OCCURS  50 TIMES.
                    06  W121-HEADLIST-TNO PIC X(2).
                    06  W121-HEADLIST-TSTYUKYMD  PIC X(9).
                    06  W121-HEADLIST-TEDYUKYMD  PIC X(9).
                  05  W121-HEADLIST-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  W121-PR-PRTJYUN-COMBO.
                04  W121-PR-PRTJYUN.
                  05  W121-VALUE  PIC X(28).
                04  W121-PR-PRTJYUN-COUNT  PIC S9(9)   BINARY.
                04  W121-PR-PRTJYUNLST
                        PIC X(28)   OCCURS  5 TIMES.
              03  W121-HKNJ-PRTKBN-COMBO.
                04  W121-HKNJ-PRTKBN-STATE   PIC S9(09) BINARY.
                04  W121-HKNJ-PRTKBN.
                  05  W121-VALUE  PIC X(24).
                04  W121-HKNJ-PRTKBN-COUNT  PIC S9(9)   BINARY.
                04  W121-HKNJ-PRTKBNLST
                        PIC X(24)   OCCURS  5 TIMES.
