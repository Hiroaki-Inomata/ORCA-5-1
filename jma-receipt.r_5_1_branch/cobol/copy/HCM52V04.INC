        01  HCM52V04.
            02  HCM52V04-HEAD.
              03  HCM52V04-NYUGAI PIC X(8).
              03  HCM52V04-HSRYYMD    PIC X(64).
              03  HCM52V04-HSRYKA PIC X(10).
              03  HCM52V04-SAKYMD PIC X(22).
              03  HCM52V04-PAGE   PIC X(5).
              03  HCM52V04-SYOSINMSG  PIC X(10).
              03  HCM52V04-SAISINMSG  PIC X(10).
              03  HCM52V04-SYOSAIMSG  PIC X(10).
              03  HCM52V04-SKYTTLMSG
                      PIC X(12)   OCCURS  15 TIMES.
            02  HCM52V04-SKYTBL         OCCURS  8 TIMES.
              03  HCM52V04-GOKEI  PIC X(10).
              03  HCM52V04-SKYMONEY-G.
                04  HCM52V04-SKYMONEY
                        PIC X(10)   OCCURS  15 TIMES.
            02  HCM52V04-JIHITBL.
              03  HCM52V04-TTLJIHIMSG PIC X(10).
              03  HCM52V04-TTLJIHI    PIC X(10).
              03  HCM52V04-JIHIMSG
                      PIC X(10)   OCCURS  10 TIMES.
              03  HCM52V04-JIHI
                      PIC X(10)   OCCURS  10 TIMES.
            02  HCM52V04-NYUKINTBL.
              03  HCM52V04-TTLHOHOMSG PIC X(10).
              03  HCM52V04-TTLNYUKIN  PIC X(10).
              03  HCM52V04-HOHOMSG
                      PIC X(10)   OCCURS  15 TIMES.
              03  HCM52V04-NYUKIN
                      PIC X(10)   OCCURS  15 TIMES.
