        01  HCM01.
            02  HCM01-WORK
                        PIC X(16).
            02  HCM01-TUZUKI
                        PIC X(8).
            02  HCM01-HPTNUM
                        PIC X(16).
            02  HCM01-KIGONUM
                        PIC X(28).
            02  HCM01-HIHKNJNAME
                        PIC X(22).
            02  HCM01-PTNUM
                        PIC X(20).
            02  HCM01-KANANAME
                        PIC X(30).
            02  HCM01-KANANAME2
                        PIC X(60).
            02  HCM01-NAME
                        PIC X(30).
            02  HCM01-NAME2
                        PIC X(60).
            02  HCM01-JKOH-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-JKYSNO
                        PIC X(14).
            02  HCM01-FKOH-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-FTNSNO
                        PIC X(16).
            02  HCM01-OTOKO
                        PIC X(2).
            02  HCM01-ONNA
                        PIC X(2).
            02  HCM01-TENKI-TBL
                                        OCCURS 8 TIMES.
              03  HCM01-TENKIG.
                04  HCM01-TENKI
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM01-STRYMD-TBL
                                        OCCURS 8 TIMES.
              03  HCM01-STRYMDG.
                04  HCM01-STRGG
                        PIC X(1).
                04  HCM01-STRYMD
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM01-ENDYMD-TBL
                                        OCCURS 8 TIMES.
              03  HCM01-ENDYMDG.
                04  HCM01-ENDGG
                        PIC X(1).
                04  HCM01-ENDYMD
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM01-TANJYOG
                        PIC X(4).
            02  HCM01-TANJYOY
                        PIC X(2).
            02  HCM01-TANJYOM
                        PIC X(2).
            02  HCM01-TANJYOD
                        PIC X(2).
            02  HCM01-NENREI
                        PIC X(3).
            02  HCM01-YUKOG
                        PIC X(4).
            02  HCM01-YUKOY
                        PIC X(2).
            02  HCM01-YUKOM
                        PIC X(2).
            02  HCM01-YUKOD
                        PIC X(2).
            02  HCM01-SIKAKUG
                        PIC X(4).
            02  HCM01-SIKAKUY
                        PIC X(2).
            02  HCM01-SIKAKUM
                        PIC X(2).
            02  HCM01-SIKAKUD
                        PIC X(2).
            02  HCM01-ADRS
                        PIC X(64).
            02  HCM01-ADRSTEL
                        PIC X(20).
            02  HCM01-KINADRS
                        PIC X(30).
            02  HCM01-KINNAME
                        PIC X(30).
            02  HCM01-KINADRSTEL
                        PIC X(20).
            02  HCM01-HKNADRS
                        PIC X(30).
            02  HCM01-HKNNAME
                        PIC X(30).
            02  HCM01-HKNADRSTEL
                        PIC X(20).
            02  HCM01-BYOMEI-TBL
                                        OCCURS 8 TIMES.
              03  HCM01-BYOMEIG.
                04  HCM01-BYOMEI
                        PIC X(112).
            02  HCM01-JKOHN-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-JKYSNON
                        PIC X(20).
            02  HCM01-FKOHN-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-FTNSNON
                        PIC X(20).
            02  HCM01-JKOH12-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-JKYSNO12
                        PIC X(24).
            02  HCM01-KIGO
                        PIC X(28).
            02  HCM01-NUM
                        PIC X(28).
            02  HCM01-COMMENT
                        PIC X(96)       OCCURS 5 TIMES.
            02  HCM01-JKOH15-TBL
                                        OCCURS 3 TIMES.
              03  HCM01-JKYSNO15
                        PIC X(30).
            02  HCM01-EDABAN
                        PIC X(4).
