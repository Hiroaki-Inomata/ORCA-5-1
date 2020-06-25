        01  SC10.
            02  SC10-SRYY   PIC X(2).
            02  SC10-SRYM   PIC X(2).
            02  SC10-TEISYUTUSAKI   PIC X(4).
            02  SC10-PREFNUM    PIC X(2).
            02  SC10-HOSPCDN    PIC X(20).
            02  SC10-RJNFTNJANUM    PIC X(16).
            02  SC10-FTNJANUM1  PIC X(16).
            02  SC10-FTNJANUM2  PIC X(16).
            02  SC10-RJNJKYSNUM PIC X(14).
            02  SC10-JKYSNUM1   PIC X(14).
            02  SC10-JKYSNUM2   PIC X(14).
            02  SC10-HKNJANUM   PIC X(16).
            02  SC10-KIGO   PIC X(34).
            02  SC10-NUM    PIC X(34).
            02  SC10-PTNUM  PIC X(20).
            02  SC10-KANANAME   PIC X(40).
            02  SC10-NAME   PIC X(30).
            02  SC10-SEXNUM PIC X(2).
            02  SC10-SEXN   PIC X(2).
            02  SC10-BIRTHDAY   PIC X(24).
      *=== 特記事項印字追加 by Hagino(2012/09/19) start
      *      02  SC10-TOKKI  PIC X(6).
           02  SC10-TOKKI-TBL OCCURS 3.
               03  SC10-TOKKI              PIC  X(08).
      *=== 特記事項印字追加 by Hagino(2012/09/19) end
            02  SC10-ZAISOU PIC X(4).
            02  SC10-ZAIMARU    PIC X(2).
            02  SC10-ADRS   PIC X(100).
            02  SC10-HOSPNAME   PIC X(100).
            02  SC10-KAISETUNAME    PIC X(40).
            02  SC10-BYOMEI-TBL         OCCURS  5 TIMES.
              03  SC10-BYOMEI PIC X(54).
            02  SC10-SRY-TBL            OCCURS  5 TIMES.
              03  SC10-SRYYMD PIC X(26).
            02  SC10-SNISSU1    PIC X(4).
            02  SC10-SNISSU2    PIC X(4).
            02  SC10-SNISSU3    PIC X(4).
            02  SC10-STENSU1    PIC X(7).
            02  SC10-STENSU2    PIC X(7).
            02  SC10-STENSU3    PIC X(7).
            02  SC10-YKZFTN1    PIC X(7).
            02  SC10-YKZFTN2    PIC X(7).
            02  SC10-YKZFTN3    PIC X(7).
            02  SC10-ITBFTN1    PIC X(7).
            02  SC10-ITBFTN2    PIC X(7).
            02  SC10-ITBFTN3    PIC X(7).
            02  SC10-NYUIN-TBL.
              03  SC10-HKNSBT1    PIC X(6).
              03  SC10-HKNSBT2    PIC X(6).
              03  SC10-HKNKZK PIC X(10).
              03  SC10-KYURATE    PIC X(8).
              03  SC10-SHOKUJI-TBL        OCCURS  3 TIMES.
                04  SC10-SNISSU PIC X(2).
                04  SC10-SKJRYOHOHI PIC X(9).
                04  SC10-SKJFTN PIC X(9).
      *=== 置換コメント追加 by Yoshikawa(2013/04/03) start
      *検診コメント
           02  SC10-KENSIN             PIC  X(60).
      *=== 置換コメント追加 by Yoshikawa(2013/04/03) end
            02  SC10-BIRTHDAY2  PIC X(22).
            02  SC10-SRYG       PIC X(4).
