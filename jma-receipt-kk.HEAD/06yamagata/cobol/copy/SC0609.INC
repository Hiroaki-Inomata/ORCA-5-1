      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    井上    2018/10/12  修正履歴コメント削除
      *  01.00.02    井上    2019/04/25  改元対応
      *                                  傷病名欄開始日の元号改正対応
      ********************************************************************
       01  SC06.
      *印刷日付
           03  SC06-PRTYM              PIC  X(16).
      *医療機関コード
           03  SC06-HOSPCDN            PIC  X(20).
      *保険区分
           03  SC06-HKNKBN-TBL OCCURS 4.
               05  SC06-HKNKBN         PIC  X(02).
      *単独併用区分
           03  SC06-HEIKBN-TBL OCCURS 2.
               05  SC06-HEIKBN         PIC  X(02).
      *本人家族区分
           03  SC06-HONKZKKBN-TBL OCCURS 3.
               05  SC06-HONKZKKBN      PIC  X(02).
      *高齢区分
           03  SC06-KOREIKBN-TBL OCCURS 2.
               05  SC06-KOREIKBN       PIC  X(02).
      *老人市町村番号
           03  SC06-RJNFTNJANUM        PIC  X(16).
      *公費負担者番号
           03  SC06-FTNJANUM1          PIC  X(16).
           03  SC06-FTNJANUM2          PIC  X(16).
      *老人受給者番号
           03  SC06-RJNJKYSNUM         PIC  X(14).
      *公費受給者番号
           03  SC06-JKYSNUM1           PIC  X(14).
           03  SC06-JKYSNUM2           PIC  X(14).
      *保険者番号
           03  SC06-HKNJANUM           PIC  X(16).
      *給付割合(NEW)
           03  SC06-KYURATE10          PIC  X(02).
           03  SC06-KYURATE9           PIC  X(02).
           03  SC06-KYURATE8           PIC  X(02).
           03  SC06-KYURATE7           PIC  X(02).
      *被保険者証記号番号
           03  SC06-KIGO               PIC  X(34).
           03  SC06-BANGO              PIC  X(34).
      *患者番号
           03  SC06-PTNUM              PIC  X(20).
      *受給者氏名
           03  SC06-KANANAME           PIC  X(40).
           03  SC06-NAME               PIC  X(30).
      *生年月日
           03  SC06-BIRTHDAY           PIC  X(08).
      *性別
           03  SC06-SEX-TBL OCCURS 2.
               05  SC06-SEX            PIC  X(02).
      *生年月日：年号
           03  SC06-NENGO-TBL OCCURS 4.
               05  SC06-NENGO          PIC  X(02).
      *特記事項
           03  SC06-TOKKI-TBL OCCURS 3.
               05  SC06-TOKKI              PIC  X(08).
           03  SC06-ZAISOU             PIC  X(04).
           03  SC06-ZAIMARU            PIC  X(02).
      *医療機関所在地
           03  SC06-ADRS               PIC  X(100).
      *医療機関名称
           03  SC06-HOSPNAME           PIC  X(100).
      *開設者氏名
           03  SC06-KAISETUNAME          PIC  X(40).
      *傷病名
           03  SC06-BYOMEI-TBL  OCCURS  5.
               05  SC06-BYOMEI         PIC  X(54).
      *診療開始日
           03  SC06-SRY-TBL  OCCURS  5.
               05  SC06-SRYYMD           PIC  X(26).
      *診療実日数
           03  SC06-SNISSU1            PIC  X(04).
           03  SC06-SNISSU2            PIC  X(04).
           03  SC06-SNISSU3            PIC  X(04).
      *市町村コード
           03  SC06-CITYCD             PIC  X(16).
      *医療証番号
           03  SC06-IRYONUM            PIC  X(20).
      *有
           03  SC06-ARI                PIC  X(02).
      *無
           03  SC06-NASHI              PIC  X(02).
      *一部負担金
           03  SC06-ITBFTNARI          PIC  X(07).
      *請求点数
           03  SC06-STENSU1            PIC  X(07).
           03  SC06-STENSU2            PIC  X(07).
           03  SC06-STENSU3            PIC  X(07).
      *薬剤一部負担金
           03  SC06-YKZFTN1            PIC  X(07).
           03  SC06-YKZFTN2            PIC  X(07).
           03  SC06-YKZFTN3            PIC  X(07).
      *一部負担金
           03  SC06-ITBFTN1            PIC  X(07).
           03  SC06-ITBFTN2            PIC  X(07).
           03  SC06-ITBFTN3            PIC  X(07).
      *検診コメント
           03  SC06-KENSIN             PIC  X(60).
      *生年月日2
           03  SC06-BIRTHDAY2          PIC  X(22).
