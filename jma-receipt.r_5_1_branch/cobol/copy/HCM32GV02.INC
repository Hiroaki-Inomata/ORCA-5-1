        01  HCM32GV02.
            02  HCM32GV02-STRSRYYMD
                        PIC X(22).
            02  HCM32GV02-ENDSRYYMD
                        PIC X(22).
            02  HCM32GV02-PTNUM
                        PIC X(20).
            02  HCM32GV02-PTNAME
                        PIC X(20).
            02  HCM32GV02-LBLJNISSU1
                        PIC X(6).
            02  HCM32GV02-LBLJNISSU2
                        PIC X(2).
            02  HCM32GV02-JNISSU
                        PIC X(5).
            02  HCM32GV02-HAKKOYMD
                        PIC X(22).
            02  HCM32GV02-SRYYYMM
                        PIC X(6)        OCCURS 12 TIMES.
            02  HCM32GV02-SRYYY2
                        PIC X(10)       OCCURS 12 TIMES.
            02  HCM32GV02-SRYYYMM2
                        PIC X(16)       OCCURS 12 TIMES.
            02  HCM32GV02-LABELYM
                        PIC X(10).
            02  HCM32GV02-HKNTEN
                        PIC X(9)        OCCURS 12 TIMES.
            02  HCM32GV02-FTNMONEY
                        PIC X(11)       OCCURS 12 TIMES.
            02  HCM32GV02-JIHIMONEY
                        PIC X(11)       OCCURS 12 TIMES.
            02  HCM32GV02-SKYMONEY
                        PIC X(11)       OCCURS 12 TIMES.
            02  HCM32GV02-TEN
                        PIC X(2)        OCCURS 12 TIMES.
            02  HCM32GV02-HYEN
                        PIC X(2)        OCCURS 12 TIMES.
            02  HCM32GV02-JYEN
                        PIC X(2)        OCCURS 12 TIMES.
            02  HCM32GV02-YEN
                        PIC X(2)        OCCURS 12 TIMES.
            02  HCM32GV02-BFNYUKINMSG
                        PIC X(20)       OCCURS 12 TIMES.
            02  HCM32GV02-BFNYUKIN
                        PIC X(11)       OCCURS 12 TIMES.
            02  HCM32GV02-BFYEN
                        PIC X(2)        OCCURS 12 TIMES.
            02  HCM32GV02-GHKNTEN
                        PIC X(9).
            02  HCM32GV02-GJIHIMONEY
                        PIC X(11).
            02  HCM32GV02-GSKYMONEY
                        PIC X(11).
            02  HCM32GV02-GFTNMONEY
                        PIC X(11).
            02  HCM32GV02-HOSPADRS-OLD
                        PIC X(60).
            02  HCM32GV02-HOSPADRS2
                        PIC X(100).
            02  HCM32GV02-HOSPNAME-OLD
                        PIC X(60).
            02  HCM32GV02-HOSPTEL
                        PIC X(15).
            02  HCM32GV02-PAGE
                        PIC X(10).
            02  HCM32GV02-HOSPNAME
                        PIC X(80)       OCCURS 2 TIMES.
            02  HCM32GV02-HOSPADRS
                        PIC X(76)       OCCURS 2 TIMES.
