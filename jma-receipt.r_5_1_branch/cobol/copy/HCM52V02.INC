        01  HCM52V02.
            02  HCM52V02-HEAD.
              03  HCM52V02-NYUGAI PIC X(8).
              03  HCM52V02-HSRYYMD    PIC X(64).
              03  HCM52V02-HSRYKA PIC X(10).
              03  HCM52V02-SAKYMD PIC X(22).
              03  HCM52V02-PAGE   PIC X(5).
              03  HCM52V02-SYOSINMSG  PIC X(10).
              03  HCM52V02-SAISINMSG  PIC X(10).
              03  HCM52V02-SYOSAIMSG  PIC X(10).
              03  HCM52V02-SKYTTLMSG
                      PIC X(12)   OCCURS  15 TIMES.
            02  HCM52V02-SKYTBL         OCCURS  4 TIMES.
              03  HCM52V02-GOKEI  PIC X(8).
              03  HCM52V02-SKYMONEY-G.
                04  HCM52V02-SKYMONEY
                        PIC X(8)    OCCURS  15 TIMES.
            02  HCM52V02-JIHITBL.
              03  HCM52V02-TTLJIHIMSG PIC X(10).
              03  HCM52V02-TTLJIHI    PIC X(8).
              03  HCM52V02-JIHIMSG
                      PIC X(10)   OCCURS  10 TIMES.
              03  HCM52V02-JIHI
                      PIC X(8)    OCCURS  10 TIMES.
            02  HCM52V02-NYUKINTBL.
              03  HCM52V02-TTLHOHOMSG PIC X(10).
              03  HCM52V02-TTLNYUKIN  PIC X(8).
              03  HCM52V02-HOHOMSG
                      PIC X(10)   OCCURS  15 TIMES.
              03  HCM52V02-NYUKIN
                      PIC X(8)    OCCURS  15 TIMES.
