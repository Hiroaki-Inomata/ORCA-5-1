        01  HCGS139N.
            02  HCGS139N-PAGE
                        PIC X(3).
            02  HCGS139N-SRYYM
                        PIC X(16).
            02  HCGS139N-SYSYMD
                        PIC X(22).
            02  HCGS139N-SRYKANAME
                        PIC X(20).
            02  HCGS139N-NYUGAI
                        PIC X(20).
            02  HCGS139N-ETCHKNMSG
                        PIC X(200).
            02  HCGS139N-ETCMSG
                        PIC X(100).
            02  HCGS139N-MEISAI-AREA.
              03  HCGS139N-KOHKENSU
                        PIC X(4).
              03  HCGS139N-HKN-TBL
                                        OCCURS 31 TIMES.
                04  HCGS139N-KENSU
                        PIC X(4).
                04  HCGS139N-NISSU-DM
                        PIC X(4).
                04  HCGS139N-TENSU
                        PIC X(9).
                04  HCGS139N-FTNMONEY
                        PIC X(9).
                04  HCGS139N-HKNMONEY
                        PIC X(9).
                04  HCGS139N-KOHMONEY
                        PIC X(9).
                04  HCGS139N-TTLMONEY
                        PIC X(9).
                04  HCGS139N-SKJKAISU
                        PIC X(5).
                04  HCGS139N-SKJTTLMONEY
                        PIC X(9).
                04  HCGS139N-SKJHKNMONEY
                        PIC X(9).
                04  HCGS139N-SKJKOHMONEY
                        PIC X(9).
                04  HCGS139N-SKJFTNMONEY
                        PIC X(9).
                04  HCGS139N-TTLTTLMONEY
                        PIC X(9).
                04  HCGS139N-JIHIMONEY
                        PIC X(9).
                04  HCGS139N-GENMEN
                        PIC X(9).
                04  HCGS139N-CHOSEI1
                        PIC X(9).
                04  HCGS139N-CHOSEI2
                        PIC X(9).
                04  HCGS139N-HASU
                        PIC X(9).
            02  HCGS139N-LBLKOUGAI
                        PIC X(20).
            02  HCGS139N-LBLROUSAI
                        PIC X(20).
            02  HCGS139N-LBLJIBAI
                        PIC X(20).
            02  HCGS139N-NISSU
                        PIC X(5)        OCCURS 31 TIMES.
            02  HCGS139N-SKYSBT
                        PIC X(50).
