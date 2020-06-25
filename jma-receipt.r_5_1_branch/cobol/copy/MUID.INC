        02  MUID.
      ***** 02  MUID-FIXED3.
              03  MUID-HOSPID          PIC X(24).
              03  MUID-SINSEIYMD       PIC X(09).
              03  MUID-KANANAME        PIC X(50).
              03  MUID-NAME            PIC X(50).
              03  MUID-EMAIL           PIC X(256).
              03  MUID-HOSPSBTCOMBO.
                  05  MUID-HOSPSBT         PIC X(30).
                  05  MUID-HOSPSBT-COUNT   PIC S9(9)   BINARY.
                  05  MUID-HOSPSBT-ITEM    PIC X(30)   OCCURS  5.
              03  MUID-HOSPSBT1COMBO.
                  05  MUID-HOSPSBT1        PIC X(30).
                  05  MUID-HOSPSBT1-COUNT  PIC S9(9)   BINARY.
                  05  MUID-HOSPSBT1-ITEM   PIC X(30)   OCCURS  2.
              03  MUID-BEDSUIPPAN      PIC S9(4).
              03  MUID-BEDSURYOYO      PIC S9(4).
              03  MUID-BEDSUSEISIN     PIC S9(4).
              03  MUID-BEDSUKEKKAKU    PIC S9(4).
              03  MUID-BEDSU           PIC S9(4).
              03  MUID-SETTICOMBO.
                  05  MUID-SETTI           PIC X(30).
                  05  MUID-SETTI-COUNT     PIC S9(9)   BINARY.
                  05  MUID-SETTI-ITEM      PIC X(30)   OCCURS  6.
              03  MUID-SYUKACOMBO.
                  05  MUID-SYUKA           PIC X(30).
                  05  MUID-SYUKA-COUNT     PIC S9(9)   BINARY.
                  05  MUID-SYUKA-ITEM      PIC X(30)   OCCURS  50.
              03  MUID-CIIKICOMBO.
                  05  MUID-CIIKI           PIC X(30).
                  05  MUID-CIIKI-COUNT     PIC S9(9)   BINARY.
                  05  MUID-CIIKI-ITEM      PIC X(30)   OCCURS  2.
              03  MUID-DPCKBNCOMBO.
                  05  MUID-DPCKBN          PIC X(30).
                  05  MUID-DPCKBN-COUNT    PIC S9(9)   BINARY.
                  05  MUID-DPCKBN-ITEM     PIC X(30)   OCCURS  2.
              03  MUID-ZAITAKUCOMBO.
                  05  MUID-ZAITAKU         PIC X(30).
                  05  MUID-ZAITAKU-COUNT   PIC S9(9)   BINARY.
                  05  MUID-ZAITAKU-ITEM    PIC X(30)   OCCURS  2.
              03  MUID-KAIINYM         PIC X(06).
              03  MUID-POST            PIC X(07).
              03  MUID-PREF            PIC X(18).
              03  MUID-ADRS            PIC X(34).
              03  MUID-USER            PIC X(07).
              03  MUID-PASSWORD        PIC X(100).
              03  MUID-DURATION        PIC S9(09) BINARY.
