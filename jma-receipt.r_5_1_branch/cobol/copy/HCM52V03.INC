        01  HCM52V03.
            02  HCM52V03-HEAD.
              03  HCM52V03-NYUGAI PIC X(8).
              03  HCM52V03-HSRYYMD    PIC X(64).
              03  HCM52V03-HSRYKA PIC X(10).
              03  HCM52V03-SAKYMD PIC X(22).
              03  HCM52V03-PAGE   PIC X(5).
              03  HCM52V03-SYOSINMSG  PIC X(10).
              03  HCM52V03-SAISINMSG  PIC X(10).
              03  HCM52V03-SYOSAIMSG  PIC X(10).
              03  HCM52V03-SKYTTLMSG
                      PIC X(12)   OCCURS  15 TIMES.
            02  HCM52V03-SKYTBL         OCCURS  4 TIMES.
              03  HCM52V03-GOKEI  PIC X(10).
              03  HCM52V03-SKYMONEY-G.
                04  HCM52V03-SKYMONEY
                        PIC X(10)   OCCURS  15 TIMES.
            02  HCM52V03-JIHITBL.
              03  HCM52V03-TTLJIHIMSG PIC X(10).
              03  HCM52V03-TTLJIHI    PIC X(10).
              03  HCM52V03-JIHIMSG
                      PIC X(10)   OCCURS  10 TIMES.
              03  HCM52V03-JIHI
                      PIC X(10)   OCCURS  10 TIMES.
            02  HCM52V03-NYUKINTBL.
              03  HCM52V03-TTLHOHOMSG PIC X(10).
              03  HCM52V03-TTLNYUKIN  PIC X(10).
              03  HCM52V03-HOHOMSG
                      PIC X(10)   OCCURS  15 TIMES.
              03  HCM52V03-NYUKIN
                      PIC X(10)   OCCURS  15 TIMES.
