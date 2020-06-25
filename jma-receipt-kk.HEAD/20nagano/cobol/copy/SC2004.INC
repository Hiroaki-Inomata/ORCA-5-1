      *******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    萩野     18/12/12   修正履歴コメント削除
      *  01.00.02    井上     19/04/09   元号対応
      *  01.00.03    吉川     19/04/25   傷病名欄開始日の元号改正対応
      *******************************************************************
        01  SC04.
            02  SC04-PRTYM  PIC X(16).
            02  SC04-HOSPCDN    PIC X(20).
            02  SC04-HKNKBN-TBL         OCCURS  4 TIMES.
              03  SC04-HKNKBN PIC X(2).
            02  SC04-HEIKBN-TBL         OCCURS  3 TIMES.
              03  SC04-HEIKBN PIC X(2).
            02  SC04-HONKZKKBN-TBL      OCCURS  3 TIMES.
              03  SC04-HONKZKKBN  PIC X(2).
            02  SC04-KOREIKBN-TBL       OCCURS  2 TIMES.
              03  SC04-KOREIKBN   PIC X(2).
            02  SC04-RJNFTNJANUM    PIC X(16).
            02  SC04-FTNJANUM1  PIC X(16).
            02  SC04-FTNJANUM2  PIC X(16).
            02  SC04-RJNJKYSNUM PIC X(14).
            02  SC04-JKYSNUM1   PIC X(14).
            02  SC04-JKYSNUM2   PIC X(14).
            02  SC04-HKNJANUM   PIC X(16).
            02  SC04-KYURATE10  PIC X(2).
            02  SC04-KYURATE9   PIC X(2).
            02  SC04-KYURATE8   PIC X(2).
            02  SC04-KYURATE7   PIC X(2).
            02  SC04-KIGO   PIC X(34).
            02  SC04-BANGO  PIC X(34).
            02  SC04-PTNUM  PIC X(20).
            02  SC04-NAME   PIC X(30).
            02  SC04-BIRTHDAY   PIC X(8).
            02  SC04-SEX-TBL            OCCURS  2 TIMES.
              03  SC04-SEX    PIC X(2).
            02  SC04-NENGO-TBL          OCCURS  4 TIMES.
              03  SC04-NENGO  PIC X(2).
            02  SC04-TOKKI-TBL          OCCURS  3 TIMES.
              03  SC04-TOKKI PIC X(8).
            02  SC04-ZAISOU PIC X(4).
            02  SC04-ZAIMARU    PIC X(2).
            02  SC04-ADRS   PIC X(100).
            02  SC04-HOSPNAME   PIC X(100).
            02  SC04-KAISETUNAME    PIC X(40).
            02  SC04-BYOMEI-TBL         OCCURS  5 TIMES.
              03  SC04-BYOMEI PIC X(54).
            02  SC04-SRYYMD-TBL         OCCURS  5 TIMES.
              03  SC04-SRYYMD PIC X(26).
            02  SC04-SNISSU1    PIC X(4).
            02  SC04-SNISSU2    PIC X(4).
            02  SC04-SNISSU3    PIC X(4).
            02  SC04-CITYCD PIC X(3).
            02  SC04-JIGYONUM   PIC X(2).
            02  SC04-JKYSNUM    PIC X(10).
            02  SC04-STENSU1    PIC X(7).
            02  SC04-STENSU2    PIC X(7).
            02  SC04-STENSU3    PIC X(7).
            02  SC04-ITBFTN1    PIC X(7).
            02  SC04-ITBFTN2    PIC X(7).
            02  SC04-ITBFTN3    PIC X(7).
            02  SC04-SNISSU
                    PIC X(2)    OCCURS  3 TIMES.
            02  SC04-SKINGK
                    PIC X(9)    OCCURS  3 TIMES.
            02  SC04-SFTNGK
                    PIC X(9)    OCCURS  3 TIMES.
            02  SC04-OCR1   PIC X(67).
            02  SC04-OCR2   PIC X(67).
            02  SC04-OCR3   PIC X(67).
            02  SC04-BIRTHDAY2  PIC X(22).
