        01  HCM52V05.
            02  HCM52V05-HEAD.
              03  HCM52V05-NYUGAI
                        PIC X(8).
              03  HCM52V05-HSRYYMD
                        PIC X(64).
              03  HCM52V05-HSRYKA
                        PIC X(10).
              03  HCM52V05-SAKYMD
                        PIC X(22).
              03  HCM52V05-PAGE
                        PIC X(5).
              03  HCM52V05-SYOSINMSG
                        PIC X(10).
              03  HCM52V05-SAISINMSG
                        PIC X(10).
              03  HCM52V05-SYOSAIMSG
                        PIC X(10).
              03  HCM52V05-SKYTTLMSG
                        PIC X(12)       OCCURS 15 TIMES.
            02  HCM52V05-SKYTBL
                                        OCCURS 11 TIMES.
              03  HCM52V05-GOKEI
                        PIC X(12).
              03  HCM52V05-SKYMONEY-G.
                04  HCM52V05-SKYMONEY
                        PIC X(12)       OCCURS 10 TIMES.
            02  HCM52V05-JIHITBL.
              03  HCM52V05-TTLJIHIMSG
                        PIC X(10).
              03  HCM52V05-TTLJIHI
                        PIC X(12).
              03  HCM52V05-JIHIMSG
                        PIC X(10)       OCCURS 10 TIMES.
              03  HCM52V05-JIHI
                        PIC X(12)       OCCURS 10 TIMES.
            02  HCM52V05-NYUKINTBL.
              03  HCM52V05-TTLHOHOMSG
                        PIC X(10).
              03  HCM52V05-TTLNYUKIN
                        PIC X(12).
              03  HCM52V05-HOHOMSG
                        PIC X(10)       OCCURS 15 TIMES.
              03  HCM52V05-NYUKIN
                        PIC X(12)       OCCURS 15 TIMES.
            02  HCM52V05-KONNYUKINMSG
                        PIC X(20).
            02  HCM52V05-KONMISYUMSG
                        PIC X(20).
            02  HCM52V05-LOPTION
                        PIC X(500).
            02  HCM52V05-TTLTENSU
                        PIC X(12).
            02  HCM52V05-TENSU
                        PIC X(12)       OCCURS 10 TIMES.
