        02  K062.
      ***** 02  K062-FIXED5.
              03  K062-MEISYO.
                04  K062-VALUE  PIC X(70).
              03  K062-DAY-G    OCCURS   31.
                04  K062-DAY-STYLE  PIC X(20).
                04  K062-DAY        PIC S9(2).
              03  K062-SELNUM.
                04  K062-VALUE  PIC S9(2).
              03  K062-COMMENT-G    OCCURS   5.
                04  K062-COMMENT  PIC X(80).
              03  K062-MUN5.
                04  K062-VALUE  PIC X(4).
              03  K062-MUN4.
                04  K062-VALUE  PIC X(4).
              03  K062-MUN3.
                04  K062-VALUE  PIC X(4).
              03  K062-MUN2.
                04  K062-VALUE  PIC X(4).
              03  K062-MUN1.
                04  K062-VALUE  PIC X(4).
              03  K062-SRYCD.
                04  K062-VALUE  PIC X(9).
              03  K062-SRYYM.
                04  K062-VALUE  PIC X(6).
              03  K062-HKNCOMBI.
                04  K062-VALUE  PIC X(4).
              03  K062-HKNCOMBIMEI.
                04  K062-VALUE  PIC X(20).
              03  K062-MIDASI.
                04  K062-MIDASI-STYLE  PIC X(20).
                04  K062-MIDASI-VALUE  PIC X(20).
              03  K062-ENDYMD.
                04  K062-VALUE  PIC X(11).
              03  K062-TENDYMD-G     OCCURS  5.
                04  K062-TENDYMD    PIC X(11).
              03  K062-MIDASI2.
                04  K062-MIDASI2-STYLE  PIC X(20).
                04  K062-MIDASI2-VALUE  PIC X(20).
              03  K062-MSGENDYMD.
                04  K062-MSGENDYMD-STYLE  PIC X(20).
                04  K062-MSGENDYMD-VALUE  PIC X(20).
