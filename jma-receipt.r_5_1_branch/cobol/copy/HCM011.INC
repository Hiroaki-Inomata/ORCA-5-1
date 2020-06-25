        01  HCM011.
            02  HCM011-PTNUM
                        PIC X(20).
            02  HCM011-NAME
                        PIC X(30).
            02  HCM011-NAME2
                        PIC X(60).
            02  HCM011-TENKI-TBL
                                        OCCURS 22 TIMES.
              03  HCM011-TENKIG.
                04  HCM011-TENKI
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM011-STRYMD-TBL
                                        OCCURS 22 TIMES.
              03  HCM011-STRYMDG.
                04  HCM011-STRGG
                        PIC X(1).
                04  HCM011-STRYMD
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM011-ENDYMD-TBL
                                        OCCURS 22 TIMES.
              03  HCM011-ENDYMDG.
                04  HCM011-ENDGG
                        PIC X(1).
                04  HCM011-ENDYMD
                        PIC X(2)        OCCURS 3 TIMES.
            02  HCM011-BYOMEI-TBL
                                        OCCURS 22 TIMES.
              03  HCM011-BYOMEIG.
                04  HCM011-BYOMEI
                        PIC X(112).
