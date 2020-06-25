            02  W12.
              03  W12-YUKOSTYMD.
                04  W12-VALUE   PIC X(9).
              03  W12-YUKOEDYMD.
                04  W12-VALUE   PIC X(9).
              03  W12-NOTE3.
                04  W12-PAGENO  PIC S9(9)   BINARY.
                04  W12-FI23.
                  05  W12-BYOMEIHENKBN-COMBO.
                    06  W12-BYOMEIHENKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-BYOMEIHENKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-BYOMEIHENKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-BYOMEIHENKBN1-COMBO.
                    06  W12-BYOMEIHENKBN1.
                      07  W12-VALUE   PIC X(22).
                    06  W12-BYOMEIHENKBN1-COUNT   PIC S9(9)   BINARY.
                    06  W12-BYOMEIHENKBN1LST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-SYUBYOHENFLG-COMBO.
                    06  W12-SYUBYOHENFLG.
                      07  W12-VALUE   PIC X(46).
                    06  W12-SYUBYOHENFLG-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUBYOHENFLGLST
                            PIC X(46)   OCCURS  4 TIMES.
                  05  W12-TENKIHENKBN-COMBO.
                    06  W12-TENKIHENKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-TENKIHENKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-TENKIHENKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-TENKIBYOMEI-KBN-COMBO.
                    06  W12-TENKIBYOMEI-KBN.
                      07  W12-VALUE   PIC X(52).
                    06  W12-TENKIBYOMEI-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-TENKIBYOMEI-KBNLST
                            PIC X(52)   OCCURS  3 TIMES.
                  05  W12-SYAOCRKBN-COMBO.
                    06  W12-SYAOCRKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-SYAOCRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYAOCRKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-KKHOCRKBN-COMBO.
                    06  W12-KKHOCRKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-KKHOCRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KKHOCRKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-KOUIKIOCRKBN-COMBO.
                    06  W12-KOUIKIOCRKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-KOUIKIOCRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KOUIKIOCRKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-RENNUMHENKBN-COMBO.
                    06  W12-RENNUMHENKBN.
                      07  W12-VALUE   PIC X(22).
                    06  W12-RENNUMHENKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RENNUMHENKBNLST
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W12-NYUTAIINREKI-COMBO.
                    06  W12-NYUTAIINREKI.
                      07  W12-VALUE   PIC X(42).
                    06  W12-NYUTAIINREKI-COUNT   PIC S9(9)   BINARY.
                    06  W12-NYUTAIINREKILST
                            PIC X(42)   OCCURS  3 TIMES.
                  05  W12-NYUTAIINREKI-NYU-COMBO.
                    06  W12-NYUTAIINREKI-NYU.
                      07  W12-VALUE   PIC X(42).
                    06  W12-NYUTAIINREKI-NYU-COUNT  PIC S9(9)   BINARY.
                    06  W12-NYUTAIINREKI-NYULST
                            PIC X(42)   OCCURS  2 TIMES.
                  05  W12-SRYKAHENKBN-COMBO.
                    06  W12-SRYKAHENKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-SRYKAHENKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SRYKAHENKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-BTUNUM-KSIKBN-COMBO.
                    06  W12-BTUNUM-KSIKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-BTUNUM-KSIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-BTUNUM-KSIKBNLST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W12-RJNGNBKKBN-COMBO.
                    06  W12-RJNGNBKKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RJNGNBKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RJNGNBKKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-JIHITEKKSIKBN-COMBO.
                    06  W12-JIHITEKKSIKBN.
                      07  W12-VALUE   PIC X(10).
                    06  W12-JIHITEKKSIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-JIHITEKKSIKBNLST
                            PIC X(10)   OCCURS  2 TIMES.
                  05  W12-COMMENT-NYUINRYO-COMBO.
                    06  W12-COMMENT-NYUINRYO.
                      07  W12-VALUE   PIC X(34).
                    06  W12-COMMENT-NYUINRYO-COUNT   PIC S9(9)   BINARY.
                    06  W12-COMMENT-NYUINRYOLST
                            PIC X(34)   OCCURS  5 TIMES.
                  05  W12-COMMENT-NYUINRYO1-COMBO.
                    06  W12-COMMENT-NYUINRYO1.
                      07  W12-VALUE   PIC X(34).
                    06  W12-COMMENT-NYUINRYO1-COUNT   PIC S9(9)  BINARY.
                    06  W12-COMMENT-NYUINRYO1LST
                            PIC X(34)   OCCURS  5 TIMES.
                  05  W12-CHKKBN-COMBO.
                    06  W12-CHKKBN.
                      07  W12-VALUE   PIC X(36).
                    06  W12-CHKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-CHKKBNLST
                            PIC X(36)   OCCURS  5 TIMES.
                  05  W12-SHOKUJI-PRTKBN-COMBO.
                    06  W12-SHOKUJI-PRTKBN.
                      07  W12-VALUE   PIC X(36).
                    06  W12-SHOKUJI-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SHOKUJI-PRTKBNLST
                            PIC X(36)   OCCURS  5 TIMES.
                  05  W12-BYOMEI-PRTJYUN-COMBO.
                    06  W12-BYOMEI-PRTJYUN.
                      07  W12-VALUE   PIC X(36).
                    06  W12-BYOMEI-PRTJYUN-COUNT   PIC S9(9)   BINARY.
                    06  W12-BYOMEI-PRTJYUNLST
                            PIC X(36)   OCCURS  5 TIMES.
                  05  W12-PREFPRGID.
                    06  W12-VALUE   PIC X(15).
                  05  W12-PREFPRGID1-STATE   PIC S9(9)   BINARY.
                  05  W12-PREFPRGID1       PIC X(15).
                  05  W12-AUTODSPKBN-COMBO.
                    06  W12-AUTODSPKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-AUTODSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-AUTODSPKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-SEIHO-DSPKBN-COMBO.
                    06  W12-SEIHO-DSPKBN.
                      07  W12-VALUE   PIC X(34).
                    06  W12-SEIHO-DSPKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SEIHO-DSPKBNLST
                            PIC X(34)   OCCURS  5 TIMES.
                  05  W12-RECEIPT-MAKE-COMBO.
                    06  W12-RECEIPT-MAKE.
                      07  W12-VALUE   PIC X(20).
                    06  W12-RECEIPT-MAKE-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECEIPT-MAKELST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-ZERO-KOH-PRTKBN-COMBO.
                    06  W12-ZERO-KOH-PRTKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-ZERO-KOH-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-ZERO-KOH-PRTKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-SCR.
                    06  W12-HKNJANUMLIST.
                      07  W12-HKNJANUMLIST-COUNT  PIC S9(9)   BINARY.
                      07  W12-HKNJANUMLIST-ROW    PIC S9(9)   BINARY.
                      07  W12-HKNJANUMLIST-ROWATTR PIC S9(9)   BINARY.
                      07  W12-HKNJANUMLIST-ITEM    OCCURS  10 TIMES.
                        08  W12-HKNJANUMLIST-TNO PIC X(2).
                        08  W12-HKNJANUMLIST-THKNJANUM PIC X(8).
                        08  W12-HKNJANUMLIST-THKNJANUM-63 PIC X(2).
                        08  W12-HKNJANUMLIST-THKNJANAME PIC X(50).
                      07  W12-HKNJANUMLIST-SELECT
                              PIC X       OCCURS  10 TIMES.
                  05  W12-SELNUM.
                    06  W12-VALUE  PIC S9(2).
                  05  W12-HKNJANUM1.
                    06  W12-VALUE  PIC X(8).
                  05  W12-HKNJANAME1.
                    06  W12-VALUE  PIC X(50).
                04  W12-FI32.
                  05  W12-HKN-RIREKI-COMBO.
                    06  W12-HKN-RIREKI.
                      07  W12-VALUE   PIC X(22).
                    06  W12-HKN-RIREKI-COUNT   PIC S9(9)   BINARY.
                    06  W12-HKN-RIREKILST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-GENBUTU-ICHIBU-COMBO.
                    06  W12-GENBUTU-ICHIBU.
                      07  W12-VALUE   PIC X(38).
                    06  W12-GENBUTU-ICHIBU-COUNT   PIC S9(9)   BINARY.
                    06  W12-GENBUTU-ICHIBULST
                            PIC X(38)   OCCURS  5 TIMES.
                  05  W12-COMPRESS-COMBO.
                    06  W12-COMPRESS.
                      07  W12-VALUE   PIC X(30).
                    06  W12-COMPRESS-COUNT   PIC S9(9)   BINARY.
                    06  W12-COMPRESSLST
                            PIC X(30)   OCCURS  2 TIMES.
                  05  W12-DISASTER-RECEIPT-COMBO.
                    06  W12-DISASTER-RECEIPT.
                      07  W12-VALUE   PIC X(20).
                    06  W12-DISASTER-RECEIPT-COUNT   PIC S9(9)   BINARY.
                    06  W12-DISASTER-RECEIPTLST
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W12-15-YKZ-COMBO.
                    06  W12-15-YKZ.
                      07  W12-VALUE    PIC X(20).
                    06  W12-15-YKZ-COUNT   PIC S9(9)   BINARY.
                    06  W12-15-YKZLST
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W12-KOUHI-ICHIBU-KSI-COMBO.
                    06  W12-KOUHI-ICHIBU-KSI.
                      07  W12-VALUE    PIC X(32).
                    06  W12-KOUHI-ICHIBU-KSI-COUNT   PIC S9(9)   BINARY.
                    06  W12-KOUHI-ICHIBU-KSILST
                            PIC X(32)   OCCURS  5 TIMES.
                04  W12-FI30.
                  05  W12-SYAPRTNM.
                    06  W12-VALUE   PIC X(16).
                  05  W12-SYAPRTNM-NEXT.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KKHPRTNM.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KKHPRTNM-NEXT.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KOUIKIPRTNM.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KOUIKIPRTNM-NEXT.
                    06  W12-VALUE   PIC X(16).
                  05  W12-NEXTPRTKBN1-COMBO.
                    06  W12-NEXTPRTKBN1.
                      07  W12-VALUE   PIC X(16).
                    06  W12-NEXTPRTKBN1-COUNT   PIC S9(9)   BINARY.
                    06  W12-NEXTPRTKBN1LST
                            PIC X(16)   OCCURS  4 TIMES.
                  05  W12-SYAPRTNM-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-SYAPRTNM-NEXT-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KKHPRTNM-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KKHPRTNM-NEXT-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KOUIKIPRTNM-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-KOUIKIPRTNM-NEXT-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-NEXTPRTKBN-COMBO.
                    06  W12-NEXTPRTKBN.
                      07  W12-VALUE   PIC X(16).
                    06  W12-NEXTPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-NEXTPRTKBNLST
                            PIC X(16)   OCCURS  3 TIMES.
                  05  W12-PRT-CHECKKBN-COMBO.
                    06  W12-PRT-CHECKKBN.
                      07  W12-VALUE   PIC X(22).
                    06  W12-PRT-CHECKKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRT-CHECKKBNLST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-JIHI-PRTKBN-COMBO.
                    06  W12-JIHI-PRTKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-JIHI-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-JIHI-PRTKBNLST
                            PIC X(26)   OCCURS  2 TIMES.
                  05  W12-PRTJYUNKBN4-COMBO.
                    06  W12-PRTJYUNKBN4.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN4-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN4LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W12-PRTJYUNKBN6-COMBO.
                    06  W12-PRTJYUNKBN6.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN6-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN6LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W12-PRTJYUNKBN1-COMBO.
                    06  W12-PRTJYUNKBN1.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN1-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN1LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W12-PRTJYUNKBN3-COMBO.
                    06  W12-PRTJYUNKBN3.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN3-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN3LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W12-PRTJYUNKBN5-COMBO.
                    06  W12-PRTJYUNKBN5.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN5-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN5LST
                            PIC X(31)   OCCURS  10 TIMES.
                  05  W12-PRTJYUNKBN2-COMBO.
                    06  W12-PRTJYUNKBN2.
                      07  W12-VALUE   PIC X(31).
                    06  W12-PRTJYUNKBN2-COUNT   PIC S9(9)   BINARY.
                    06  W12-PRTJYUNKBN2LST
                            PIC X(31)   OCCURS  10 TIMES.
                04  W12-FI29.
                  05  W12-FRAME-ROU-COMBO.
                    06  W12-FRAME-ROU.
                      07  W12-VALUE   PIC X(22).
                    06  W12-FRAME-ROU-COUNT   PIC S9(9)   BINARY.
                    06  W12-FRAME-ROULST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-FRAME-ROU-N-COMBO.
                    06  W12-FRAME-ROU-N.
                      07  W12-VALUE   PIC X(22).
                    06  W12-FRAME-ROU-N-COUNT   PIC S9(9)   BINARY.
                    06  W12-FRAME-ROU-NLST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-RSI-RECEIPTKBN-COMBO.
                    06  W12-RSI-RECEIPTKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RSI-RECEIPTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RSI-RECEIPTKBNLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-ROUPRTKBN-COMBO.
                    06  W12-ROUPRTKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-ROUPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-ROUPRTKBNLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-ROUPRTKBN-N-COMBO.
                    06  W12-ROUPRTKBN-N.
                      07  W12-VALUE   PIC X(24).
                    06  W12-ROUPRTKBN-N-COUNT   PIC S9(9)   BINARY.
                    06  W12-ROUPRTKBN-NLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-COMMENT-ROU-COMBO.
                    06  W12-COMMENT-ROU.
                      07  W12-VALUE   PIC X(22).
                    06  W12-COMMENT-ROU-COUNT   PIC S9(9)   BINARY.
                    06  W12-COMMENT-ROULST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-FRAME-JIBAI-COMBO.
                    06  W12-FRAME-JIBAI.
                      07  W12-VALUE   PIC X(22).
                    06  W12-FRAME-JIBAI-COUNT   PIC S9(9)   BINARY.
                    06  W12-FRAME-JIBAILST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-FRAME-JIBAI-N-COMBO.
                    06  W12-FRAME-JIBAI-N.
                      07  W12-VALUE   PIC X(22).
                    06  W12-FRAME-JIBAI-N-COUNT   PIC S9(9)   BINARY.
                    06  W12-FRAME-JIBAI-NLST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-JIBAIPRTKBN-COMBO.
                    06  W12-JIBAIPRTKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-JIBAIPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-JIBAIPRTKBNLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-JIBAIPRTKBN-N-COMBO.
                    06  W12-JIBAIPRTKBN-N.
                      07  W12-VALUE   PIC X(24).
                    06  W12-JIBAIPRTKBN-N-COUNT   PIC S9(9)   BINARY.
                    06  W12-JIBAIPRTKBN-NLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-JIBAIPRTKBN1-COMBO.
                    06  W12-JIBAIPRTKBN1.
                      07  W12-VALUE   PIC X(24).
                    06  W12-JIBAIPRTKBN1-COUNT  PIC S9(9)   BINARY.
                    06  W12-JIBAIPRTKBN1LST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-JIBAIPRTKBN1-N-COMBO.
                    06  W12-JIBAIPRTKBN1-N.
                      07  W12-VALUE   PIC X(24).
                    06  W12-JIBAIPRTKBN1-N-COUNT  PIC S9(9)   BINARY.
                    06  W12-JIBAIPRTKBN1-NLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-JIBAI-FRACTION-KBN1-COMBO.
                    06  W12-JIBAI-FRACTION-KBN1.
                      07  W12-VALUE   PIC X(38).
                    06  W12-JIBAI-FRACTION-KBN1-COUNT  PIC S9(9) BINARY.
                    06  W12-JIBAI-FRACTION-KBN1LST
                            PIC X(38)   OCCURS  5 TIMES.
                  05  W12-JIBAI-FRACTION-KBN2-COMBO.
                    06  W12-JIBAI-FRACTION-KBN2.
                      07  W12-VALUE   PIC X(38).
                    06  W12-JIBAI-FRACTION-KBN2-COUNT  PIC S9(9) BINARY.
                    06  W12-JIBAI-FRACTION-KBN2LST
                            PIC X(38)   OCCURS  5 TIMES.
                  05  W12-JIBAI-PDF-COMBO.
                    06  W12-JIBAI-PDF.
                      07  W12-VALUE   PIC X(20).
                    06  W12-JIBAI-PDF-COUNT  PIC S9(9) BINARY.
                    06  W12-JIBAI-PDFLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-ROUPRTNM.
                    06  W12-VALUE   PIC X(16).
                  05  W12-ROUPRTNM-NEXT.
                    06  W12-VALUE   PIC X(16).
                  05  W12-JBAIPRTNM.
                    06  W12-VALUE   PIC X(16).
                  05  W12-JBAIPRTNM-NEXT.
                    06  W12-VALUE   PIC X(16).
                  05  W12-ROUPRTNM-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-ROUPRTNM-NEXT-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-JBAIPRTNM-N.
                    06  W12-VALUE   PIC X(16).
                  05  W12-JBAIPRTNM-NEXT-N.
                    06  W12-VALUE   PIC X(16).
                04  W12-FI28.
                  05  W12-SYARECEKBN-COMBO.
                    06  W12-SYARECEKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-SYARECEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYARECEKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-KKHRECEKBN-COMBO.
                    06  W12-KKHRECEKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-KKHRECEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KKHRECEKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-KOUIKIRECEKBN-COMBO.
                    06  W12-KOUIKIRECEKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-KOUIKIRECEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KOUIKIRECEKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-ROURECEKBN-COMBO.
                    06  W12-ROURECEKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-ROURECEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-ROURECEKBNLST
                            PIC X(24)   OCCURS  4 TIMES.
                  05  W12-RECEDENOUT-STATE   PIC S9(9)   BINARY.
                  05  W12-RECEDENOUT.
                    06  W12-VALUE   PIC X(30).
                  05  W12-RECECOMKBN-COMBO.
                    06  W12-RECECOMKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-RECECOMKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECECOMKBNLST
                            PIC X(26)   OCCURS  2 TIMES.
                  05  W12-NI-JYOKEN1.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-NI-JYOKEN2.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-NI-JYOKEN5.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-NI-JYOKEN3.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-NI-JYOKEN4.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-NI-JYOKEN6.
                    06  W12-VALUE   PIC S9(8).
                  05  W12-RECEDEN-CDRKBN-COMBO.
                    06  W12-RECEDEN-CDRKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-RECEDEN-CDRKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECEDEN-CDRKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-RECEDEN-LST1KBN-COMBO.
                    06  W12-RECEDEN-LST1KBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-RECEDEN-LST1KBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECEDEN-LST1KBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-RECEDEN-LST2KBN-COMBO.
                    06  W12-RECEDEN-LST2KBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-RECEDEN-LST2KBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECEDEN-LST2KBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-RECEDEN-LST2KBN-MEDIA-COMBO.
                    06  W12-RECEDEN-LST2KBN-MEDIA.
                      07  W12-VALUE   PIC X(20).
                    06  W12-RECEDEN-LST2KBN-MEDIA-COUNT
                                                 PIC S9(9) BINARY.
                    06  W12-RECEDEN-LST2KBN-MEDIALST
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W12-RECEDEN-RIREKI-STATE   PIC S9(9)   BINARY.
                  05  W12-RECEDEN-RIREKIKBN-COMBO.
                    06  W12-RECEDEN-RIREKIKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RECEDEN-RIREKIKBN-COUNT PIC S9(9)   BINARY.
                    06  W12-RECEDEN-RIREKIKBNLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-RECEDEN-DATAKBN-COMBO.
                    06  W12-RECEDEN-DATAKBN.
                      07  W12-VALUE   PIC X(40).
                    06  W12-RECEDEN-DATAKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RECEDEN-DATAKBNLST
                            PIC X(40)   OCCURS  10 TIMES.
                  05  W12-RECEDEN-DISASTER-COMBO.
                    06  W12-RECEDEN-DISASTER.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RECEDEN-DISASTER-COUNT PIC S9(9)   BINARY.
                    06  W12-RECEDEN-DISASTERLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-RECEDEN-CHK-COMBO.
                    06  W12-RECEDEN-CHK.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RECEDEN-CHK-COUNT PIC S9(9)   BINARY.
                    06  W12-RECEDEN-CHKLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-KOUIKIKBN-COMBO.
                    06  W12-KOUIKIKBN.
                      07  W12-VALUE   PIC X(34).
                    06  W12-KOUIKIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KOUIKIKBNLST
                            PIC X(34)   OCCURS  4 TIMES.
                  05  W12-KIGO-DELI-FLG-COMBO.
                    06  W12-KIGO-DELI-FLG.
                      07  W12-VALUE   PIC X(30).
                    06  W12-KIGO-DELI-FLG-COUNT PIC S9(9)   BINARY.
                    06  W12-KIGO-DELI-FLGLST
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W12-KIGO-DELI-HKNJA-FLG-COMBO.
                    06  W12-KIGO-DELI-HKNJA-FLG.
                      07  W12-VALUE   PIC X(30).
                    06  W12-KIGO-DELI-HKNJA-FLG-COUNT
                                                     PIC S9(9)   BINARY.
                    06  W12-KIGO-DELI-HKNJA-FLGLST
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W12-NUM-DELI-FLG-COMBO.
                    06  W12-NUM-DELI-FLG.
                      07  W12-VALUE   PIC X(30).
                    06  W12-NUM-DELI-FLG-COUNT  PIC S9(9)   BINARY.
                    06  W12-NUM-DELI-FLGLST
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W12-NUM-DELI-HKNJA-FLG-COMBO.
                    06  W12-NUM-DELI-HKNJA-FLG.
                      07  W12-VALUE   PIC X(30).
                    06  W12-NUM-DELI-HKNJA-FLG-COUNT
                                                    PIC S9(9)   BINARY.
                    06  W12-NUM-DELI-HKNJA-FLGLST
                            PIC X(30)   OCCURS  5 TIMES.
                04  W12-FI26.
                  05  W12-PREFKBN-PRTJYUN-COMBO.
                    06  W12-PREFKBN-PRTJYUN.
                      07  W12-VALUE   PIC X(12).
                    06  W12-PREFKBN-PRTJYUN-COUNT   PIC S9(9)   BINARY.
                    06  W12-PREFKBN-PRTJYUNLST
                            PIC X(12)   OCCURS  5 TIMES.
                  05  W12-HKNJANUM-PRTJYUN-COMBO.
                    06  W12-HKNJANUM-PRTJYUN.
                      07  W12-VALUE   PIC X(46).
                    06  W12-HKNJANUM-PRTJYUN-COUNT   PIC S9(9)   BINARY.
                    06  W12-HKNJANUM-PRTJYUNLST
                            PIC X(46)   OCCURS  6 TIMES.
                  05  W12-RECESYUBETU-PRTJYUN-COMBO.
                    06  W12-RECESYUBETU-PRTJYUN.
                      07  W12-VALUE   PIC X(30).
                    06  W12-RECESYUBETU-PRTJYUN-COUNT
                                      PIC S9(9)   BINARY.
                    06  W12-RECESYUBETU-PRTJYUNLST
                            PIC X(30)   OCCURS  6 TIMES.
                  05  W12-ZAITAKU-COMBO.
                    06  W12-ZAITAKU.
                      07  W12-VALUE   PIC X(26).
                    06  W12-ZAITAKU-COUNT   PIC S9(9)   BINARY.
                    06  W12-ZAITAKULST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W12-CHOUKI-COMBO.
                    06  W12-CHOUKI.
                      07  W12-VALUE   PIC X(26).
                    06  W12-CHOUKI-COUNT   PIC S9(9)   BINARY.
                    06  W12-CHOUKILST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W12-HKNJANUM.
                    06  W12-VALUE   PIC X(8).
                  05  W12-HKNJANAME.
                    06  W12-VALUE   PIC X(50).
                  05  W12-HKNJANUM-PRTKBN-COMBO.
                    06  W12-HKNJANUM-PRTKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-HKNJANUM-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-HKNJANUM-PRTKBNLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-DRCD-PRTKBN-COMBO.
                    06  W12-DRCD-PRTKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-DRCD-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-DRCD-PRTKBNLST
                            PIC X(26)   OCCURS  2 TIMES.
                  05  W12-DRCD-PRTJYUNKBN-COMBO.
                    06  W12-DRCD-PRTJYUNKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-DRCD-PRTJYUNKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-DRCD-PRTJYUNKBNLST
                            PIC X(26)   OCCURS  10 TIMES.
                  05  W12-BTUNUM-PRTKBN-COMBO.
                    06  W12-BTUNUM-PRTKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-BTUNUM-PRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-BTUNUM-PRTKBNLST
                            PIC X(26)   OCCURS  2 TIMES.
                  05  W12-BTUNUM-PRTJYUNKBN-COMBO.
                    06  W12-BTUNUM-PRTJYUNKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-BTUNUM-PRTJYUNKBN-COUNT  PIC S9(9)   BINARY.
                    06  W12-BTUNUM-PRTJYUNKBNLST
                            PIC X(26)   OCCURS  10 TIMES.
                  05  W12-TERMINAL-DIGIT-PRTKBN-COMBO.
                    06  W12-TERMINAL-DIGIT-PRTKBN.
                      07  W12-VALUE   PIC X(38).
                    06  W12-TERMINAL-DIGIT-PRTKBN-COUNT
                                                  PIC S9(9) BINARY.
                    06  W12-TERMINAL-DIGIT-PRTKBNLST
                            PIC X(38)   OCCURS  5 TIMES.
                04  W12-FI25.
                  05  W12-MOJI-COMBO.
                    06  W12-MOJI-KISAIKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-MOJI-KISAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-MOJI-KISAIKBNLST
                            PIC X(20)   OCCURS  5 TIMES.
                  05  W12-KYURATE-COMBO.
                    06  W12-KYURATE-KISAIKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-KYURATE-KISAIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KYURATE-KISAIKBNLST
                            PIC X(26)   OCCURS  5 TIMES.
                  05  W12-RECESYUBETU-COMBO.
                    06  W12-RECESYUBETU-KISAIKBN.
                      07  W12-VALUE   PIC X(26).
                    06  W12-RECESYUBETU-KISAIKBN-COUNT
                                        PIC S9(9)   BINARY.
                    06  W12-RECESYUBETU-KISAIKBNLST
                            PIC X(26)   OCCURS  5 TIMES.
                04  W12-FI24.
                  05  W12-SYAPRTKBN-COMBO.
                    06  W12-SYAPRTKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-SYAPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYAPRTKBNLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-DISASTER-SYAPRTKBN-COMBO.
                    06  W12-DISASTER-SYAPRTKBN.
                      07  W12-VALUE   PIC X(32).
                    06  W12-DISASTER-SYAPRTKBN-COUNT   PIC S9(9) BINARY.
                    06  W12-DISASTER-SYAPRTKBNLST
                            PIC X(32)   OCCURS  2 TIMES.
                  05  W12-KKHPRTKBN-COMBO.
                    06  W12-KKHPRTKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-KKHPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KKHPRTKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-KOUIKIPRTKBN-COMBO.
                    06  W12-KOUIKIPRTKBN.
                      07  W12-VALUE   PIC X(34).
                    06  W12-KOUIKIPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-KOUIKIPRTKBNLST
                            PIC X(34)   OCCURS  5 TIMES.
                  05  W12-RSI-INVOICEKBN-COMBO.
                    06  W12-RSI-INVOICEKBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-RSI-INVOICEKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-RSI-INVOICEKBNLST
                            PIC X(24)   OCCURS  2 TIMES.
                  05  W12-ROUSAIPRTKBN-COMBO.
                    06  W12-ROUSAIPRTKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-ROUSAIPRTKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-ROUSAIPRTKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                  05  W12-FRAME-ROUSAIPRTKBN-COMBO.
                    06  W12-FRAME-ROUSAIPRTKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-FRAME-ROUSAIPRTKBN-COUNT   PIC S9(9) BINARY.
                    06  W12-FRAME-ROUSAIPRTKBNLST
                            PIC X(20)   OCCURS  2 TIMES.
                04  W12-FI27.
                  05  W12-SYUKA-KBN-COMBO.
                    06  W12-SYUKA-KBN.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-KBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-KBNLST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-SYUKA-NYUIN-COMBO.
                    06  W12-SYUKA-NYUIN.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-NYUIN-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-NYUINLST
                            PIC X(22)   OCCURS  5 TIMES.
                  05  W12-SYUKA-GAIRAI-COMBO.
                    06  W12-SYUKA-GAIRAI.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-GAIRAI-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-GAIRAILST
                            PIC X(22)   OCCURS  5 TIMES.
                  05  W12-SYUKA-SELECT-COMBO.
                    06  W12-SYUKA-SELECT.
                      07  W12-VALUE   PIC X(30).
                    06  W12-SYUKA-SELECT-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-SELECTLST
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W12-SYUKA-MODE-COMBO.
                    06  W12-SYUKA-MODE.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-MODE-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-MODELST
                            PIC X(22)   OCCURS  2 TIMES.
                  05  W12-SYUKA-SYAHO-COMBO.
                    06  W12-SYUKA-SYAHO.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-SYAHO-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-SYAHOLST
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W12-SYUKA-KOKUHO-COMBO.
                    06  W12-SYUKA-KOKUHO.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-KOKUHO-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-KOKUHOLST
                            PIC X(22)   OCCURS  3 TIMES.
                  05  W12-SYUKA-KOUIKI-COMBO.
                    06  W12-SYUKA-KOUIKI.
                      07  W12-VALUE   PIC X(22).
                    06  W12-SYUKA-KOUIKI-COUNT   PIC S9(9)   BINARY.
                    06  W12-SYUKA-KOUIKILST
                            PIC X(22)   OCCURS  3 TIMES.
                04  W12-FI31.
                  05  W12-INSPECT-CHARACTER-KBN-COMBO.
                    06  W12-INSPECT-CHARACTER-KBN.
                      07  W12-VALUE   PIC X(36).
                    06  W12-INSPECT-CHARACTER-KBN-COUNT
                                      PIC S9(9)   BINARY.
                    06  W12-INSPECT-CHARACTER-KBNLST
                            PIC X(36)   OCCURS  5 TIMES.
                  05  W12-INSPECT-ZERO-KBN-COMBO.
                    06  W12-INSPECT-ZERO-KBN.
                      07  W12-VALUE   PIC X(24).
                    06  W12-INSPECT-ZERO-KBN-COUNT
                                      PIC S9(9)   BINARY.
                    06  W12-INSPECT-ZERO-KBNLST
                            PIC X(24)   OCCURS  5 TIMES.
                  05  W12-INSPECT-INCLUDE-KBN-COMBO.
                    06  W12-INSPECT-INCLUDE-KBN.
                      07  W12-VALUE   PIC X(32).
                    06  W12-INSPECT-INCLUDE-KBN-COUNT
                                      PIC S9(9)   BINARY.
                    06  W12-INSPECT-INCLUDE-KBNLST
                            PIC X(32)   OCCURS  5 TIMES.
                  05  W12-INSPECT-GENERAL-KBN-COMBO.
                    06  W12-INSPECT-GENERAL-KBN.
                      07  W12-VALUE   PIC X(30).
                    06  W12-INSPECT-GENERAL-KBN-COUNT
                                      PIC S9(9)   BINARY.
                    06  W12-INSPECT-GENERAL-KBNLST
                            PIC X(30)   OCCURS  5 TIMES.
                  05  W12-DRNAME-KSIKBN-COMBO.
                    06  W12-DRNAME-KSIKBN.
                      07  W12-VALUE   PIC X(20).
                    06  W12-DRNAME-KSIKBN-COUNT   PIC S9(9)   BINARY.
                    06  W12-DRNAME-KSIKBNLST
                            PIC X(20)   OCCURS  5 TIMES.
