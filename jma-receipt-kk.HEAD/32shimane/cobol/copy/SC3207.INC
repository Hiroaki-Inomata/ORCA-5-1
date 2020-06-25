        01  SC07.
      *印刷年
            02  SC07-PRTYY  PIC X(4).
      *印刷月
            02  SC07-PRTMM  PIC X(4).
      *医療機関所在地
            02  SC07-ADRS   PIC X(100).
      *医療機関名称
            02  SC07-HOSPNAME   PIC X(100).
      *開設者氏名
            02  SC07-KANRINAME  PIC X(30).
      *電話番号
            02  SC07-TEL    PIC X(30).
      *公費再掲番号
            02  SC07-KOHBANGO   PIC X(2).
            02  SC07-ZAIKEI
                    PIC X(2)    OCCURS  3 TIMES.
      *福祉医療負担者番号
            02  SC07-KOHFTNJANUM.
              03  SC07-KOHFTNJANUM1   PIC X(2).
              03  SC07-KOHFTNJANUM2   PIC X(2).
              03  SC07-KOHFTNJANUM3   PIC X(2).
              03  SC07-KOHFTNJANUM4   PIC X(2).
      *市町村番号
            02  SC07-TOWNNUM.
              03  SC07-TOWNNUM1   PIC X(2).
              03  SC07-TOWNNUM2   PIC X(2).
              03  SC07-TOWNNUM3   PIC X(2).
              03  SC07-TOWNNUM4   PIC X(2).
      *福祉医療負担者名
            02  SC07-TOWNNAME   PIC X(16).
      *請求年月日
            02  SC07-SEIYMD PIC X(22).
      *合計
            02  SC07-GOUKEI-TBL         OCCURS  2 TIMES.
      *合計件数
              03  SC07-KENSU  PIC X(5).
      *合計日数
              03  SC07-GNISSU PIC X(5).
      *合計点数
              03  SC07-GTENSU PIC X(9).
      *合計一部負担金額
              03  SC07-GRJNFTN    PIC X(9).
      *合計02長件数
              03  SC07-GTYOKI PIC X(2).
      *合計公費再掲
            02  SC07-KOHBG.
      *合計公費再掲件数
              03  SC07-BKENSU PIC X(5).
      *合計公費再掲点数
              03  SC07-BTENSU PIC X(9).
      *合計公費再掲負担金額
              03  SC07-BRJNFTN    PIC X(9).
            02  SC07-KOHZK-TBL          OCCURS  3 TIMES.
              03  SC07-ZKENSU PIC X(5).
              03  SC07-ZTENSU PIC X(9).
              03  SC07-ZRJFTN PIC X(9).
      *医療機關コード
            02  SC07-HOSPCD.
              03  SC07-HOSPCD1    PIC X(2).
              03  SC07-HOSPCD2    PIC X(2).
              03  SC07-HOSPCD3    PIC X(2).
              03  SC07-HOSPCD4    PIC X(2).
              03  SC07-HOSPCD5    PIC X(2).
              03  SC07-HOSPCD6    PIC X(2).
              03  SC07-HOSPCD7    PIC X(2).
      *明細部分
            02  SC07-KOBETU-TBL         OCCURS  14 TIMES.
      *福祉医療受給者番号
              03  SC07-JKYSNUM    PIC X(7).
      *福祉医療対象者氏名
              03  SC07-NAME   PIC X(18).
      *保険者番号
              03  SC07-HKNJANUM   PIC X(18).
      *被保険者証記号番号
              03  SC07-KIGONUM    PIC X(36).
      *老人医療受給者番号
              03  SC07-RJNNUM PIC X(7).
      *公費負担者番号１
              03  SC07-KOH1FTNJANUM   PIC X(8).
      *公費受給者番号１
              03  SC07-KOH1JKYJANUM   PIC X(7).
      *診療年月
              03  SC07-SRYYM  PIC X(4).
      *本家入外
              03  SC07-NYUGAIKBN  PIC X(1).
      *生年月日
              03  SC07-BIRTHDAY   PIC X(9).
      *給付割合
              03  SC07-KYUFURATE  PIC X(1).
      *０２長
              03  SC07-TYOKI  PIC X(2).
      *〇在
              03  SC07-ZAITAKU    PIC X(1).
      *日数
              03  SC07-NISSU  PIC X(2).
      *保険点数
              03  SC07-TENSU  PIC X(7).
      *公費１点数
              03  SC07-KOH1TEN    PIC X(7).
      *公費併用時福祉点数
              03  SC07-FKSTEN PIC X(7).
      *一部負担金額
              03  SC07-FTNMONEY   PIC X(7).
      *公費１一部負担金額
              03  SC07-KOH1FTN    PIC X(7).
      *控除額
              03  SC07-KOUJO  PIC X(6).
