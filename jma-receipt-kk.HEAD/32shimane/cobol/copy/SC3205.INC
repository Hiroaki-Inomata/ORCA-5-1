      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    井上      2019/01/10   修正履歴コメント削除
      ********************************************************************
       01  SC05.
      *診療年月
              03  SC05-PRTYM            PIC  X(16).
      *福祉医療負担者番号
              03  SC05-FTNJANUM.
                05  SC05-FTNJANUM1      PIC  X(02).
                05  SC05-FTNJANUM2      PIC  X(02).
                05  SC05-FTNJANUM3      PIC  X(02).
                05  SC05-FTNJANUM4      PIC  X(02).
      *市町村名
              03  SC05-TOWNNAME	        PIC  X(16).
      *請求年月日
              03  SC05-SEIYMD           PIC  X(22).
      *医療機関コード
              03  SC05-HOSPCD.
                05 SC05-HOSPCD1         PIC  X(02).
                05 SC05-HOSPCD2         PIC  X(02).
                05 SC05-HOSPCD3         PIC  X(02).
                05 SC05-HOSPCD4         PIC  X(02).
                05 SC05-HOSPCD5         PIC  X(02).
                05 SC05-HOSPCD6         PIC  X(02).
                05 SC05-HOSPCD7         PIC  X(02).
      *住所
              03  SC05-ADRS             PIC  X(100).
      *医療機関名
              03  SC05-HOSPNAME         PIC  X(100).
      *管理者名
              03  SC05-KANRINAME         PIC  X(30).
      *電話番号
              03  SC05-TEL         PIC  X(30).
      *合計
             03  SC05-GOUKEI-TBL       OCCURS  2.
                  05  SC05-KENSU        PIC  X(5).
                  05  SC05-GNISSU       PIC  X(6).
                  05  SC05-GTENSU       PIC  X(9).
                  05  SC05-GYKZFTN      PIC  X(7).
                  05  SC05-GKYUFU        PIC  X(9).
                  05  SC05-GKOUJO       PIC  X(9).
                  05  SC05-GTYOKI       PIC  X(2).
              03  SC05-KOHBTNUM     PIC  X(02).
              03  SC05-KOHKENSU     PIC  X(05).
              03  SC05-KOHTENSU     PIC  X(09).
              03  SC05-KOHKYUFU     PIC  X(09).
              03  SC05-HYGOUKEI-TBL          OCCURS 3.
                  05  SC05-KBTNUM     PIC  X(02).
                  05  SC05-KKENSU      PIC  X(05).
                  05  SC05-KTENSU     PIC  X(09).
                  05  SC05-KKYUFU     PIC  X(09).
      *総合計
              03  SC05-GKENSU           PIC  X(5).
      *個別
                  03  SC05-KOBETU-TBL       OCCURS  16.
                  05  SC05-JKYSNUM      PIC  X(7).
                  05  SC05-NAME	        PIC  X(20).
                  05  SC05-HKNJANUM     PIC  X(9).
                  05  SC05-KIGONUM      PIC  X(36).
                  05  SC05-KOH1FTNJANUM    PIC  X(8).
                  05  SC05-KOH1JKYJANUM    PIC  X(7).
                  05  SC05-SRYYM        PIC  X(4).
                  05  SC05-NYUGAIKBN    PIC  X(1).
                  05  SC05-BIRTHDAY    PIC  X(9).
                  05  SC05-KYUFURATE    PIC  X(1).
                  05  SC05-TYOKI        PIC  X(2).
                  05  SC05-ZAITAKU    PIC  X(1).
                  05  SC05-NISSU        PIC  X(2).
                  05  SC05-KOH1TEN    PIC  X(7).
                  05  SC05-FKSTEN     PIC  X(7).
                  05  SC05-FTNMONEY   PIC  X(7).
                  05  SC05-KOH1FTNKT  PIC  X(7).
                  05  SC05-KESEMARU     PIC  X(2).
                  05  SC05-TENSU        PIC  X(7).
                  05  SC05-YKZFTN       PIC  X(5).
                  05  SC05-KYUFUGAKU    PIC  X(7).
                  05  SC05-KOUJO        PIC  X(6).
