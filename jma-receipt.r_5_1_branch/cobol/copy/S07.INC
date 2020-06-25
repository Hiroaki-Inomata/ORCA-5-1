        02  S07.
      *     02  S07-FIXED1.
              03  S07-LBLSKYPRT            PIC X(70).
              03  S07-SRYYM                PIC X(10).
              03  S07-CMB-KIKANKBN.
                04  S07-KIKANKBN           PIC X(20).
                04  S07-KIKANKBN-COUNT     PIC S9(9)   BINARY.
                04  S07-KIKANKBN-ITEM      PIC X(20)   OCCURS  5 TIMES.
              03  S07-STDAY-STATE          PIC S9(9)   BINARY.
              03  S07-STDAY                PIC S9(2).
              03  S07-EDDAY-STATE          PIC S9(9)   BINARY.
              03  S07-EDDAY                PIC S9(2).
              03  S07-CMB-TUKIGPRKBN.
                04  S07-TUKIGRPKBN         PIC X(30).
                04  S07-TUKIGRPKBN-COUNT   PIC S9(9)   BINARY.
                04  S07-TUKIGRPKBN-ITEM    PIC X(30)   OCCURS  5 TIMES.
