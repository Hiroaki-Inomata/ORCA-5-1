        01  HCM04.
            02  HCM04-NO
                        PIC X(7).
            02  HCM04-SRYYMD
                        PIC X(30).
            02  HCM04-NAME
                        PIC X(30).
            02  HCM04-PTNUM
                        PIC X(20).
            02  HCM04-SRYKANAME
                        PIC X(20).
            02  HCM04-HAKYMD
                        PIC X(22).
            02  HCM04-HOKEN
                        PIC X(20).
            02  HCM04-FUTAN-OLD
                        PIC X(3).
            02  HCM04-PAGE
                        PIC X(3).
            02  HCM04-NAIYO-TBL
                                        OCCURS 70 TIMES.
              03  HCM04-SRYKBN
                        PIC X(10).
              03  HCM04-SRYMEI
                        PIC X(76).
              03  HCM04-TENSU
                        PIC X(9).
              03  HCM04-TENSU2
                        PIC X(10).
              03  HCM04-KAISU
                        PIC X(3).
              03  HCM04-KAISU2
                        PIC X(38).
              03  HCM04-KINGAKU
                        PIC X(11).
              03  HCM04-KINGAKU2
                        PIC X(16).
            02  HCM04-IRYOADRS
                        PIC X(100).
            02  HCM04-IRYOMEI
                        PIC X(100).
            02  HCM04-IRYOTEL
                        PIC X(15).
            02  HCM04-LABEL1
                        PIC X(16).
            02  HCM04-LABEL2
                        PIC X(126).
            02  HCM04-LABEL3
                        PIC X(10).
            02  HCM04-FUTANLABEL
                        PIC X(2).
            02  HCM04-LOPTION
                        PIC X(50).
            02  HCM04-KANANAME
                        PIC X(50).
            02  HCM04-CHUI
                        PIC X(100).
            02  HCM04-FUTAN
                        PIC X(10).
            02  HCM04-NAME2
                        PIC X(50).
