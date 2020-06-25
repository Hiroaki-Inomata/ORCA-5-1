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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    井上         19/04/09  元号対応
      ************************************************************
       01  SC03.
      *印刷日付
           03  SC03-PRTYM              PIC  X(16).
      *医療機関コード
           03  SC03-HOSPCDN            PIC  X(40).
      *保険区分
           03  SC03-HKNKBN-TBL OCCURS 4.
               05  SC03-HKNKBN         PIC  X(02).
      *単独併用区分
           03  SC03-HEIKBN-TBL OCCURS 3.
               05  SC03-HEIKBN         PIC  X(02).
      *本人家族区分
           03  SC03-HONKZKKBN-TBL OCCURS 5.
               05  SC03-HONKZKKBN      PIC  X(02).
      *老人市町村番号
           03  SC03-RJNFTNJANUM        PIC  X(16).
      *公費負担者番号
           03  SC03-FTNJANUM           PIC  X(16) OCCURS 2.
      *老人受給者番号
           03  SC03-RJNJKYSNUM         PIC  X(14).
      *公費受給者番号
           03  SC03-JKYSNUM            PIC  X(14) OCCURS 2.
      *保険者番号
           03  SC03-HKNJANUM           PIC  X(16).
      *給付割合
           03  SC03-KYURATE-TBL OCCURS 4.
               05  SC03-KYURATE        PIC  X(02).
      *被保険者証記号番号
           03  SC03-KIGO               PIC  X(34).
           03  SC03-BANGO              PIC  X(34).
      *患者番号
           03  SC03-PTNUM              PIC  X(20).
      *受給者氏名
           03  SC03-KANANAME           PIC  X(40).
           03  SC03-NAME               PIC  X(30).
      *生年月日
           03  SC03-BIRTHDAY           PIC  X(08).
      *性別
           03  SC03-SEX-TBL OCCURS 2.
               05  SC03-SEX            PIC  X(02).
      *生年月日：年号
           03  SC03-NENGO-TBL OCCURS 4.
               05  SC03-NENGO          PIC  X(02).
      *特記事項
           03  SC03-TOKKI-TBL OCCURS 3.
               05  SC03-TOKKI          PIC  X(08).
      *在宅
           03  SC03-ZAISOU             PIC  X(04).
           03  SC03-ZAIMARU            PIC  X(02).
      *医療機関所在地
           03  SC03-ADRS               PIC  X(100).
      *医療機関名称
           03  SC03-HOSPNAME           PIC  X(100).
      *開設者名
           03  SC03-KAISETUNAME        PIC  X(80).
      *傷病名テーブル
           03  SC03-BYOMEI-TBL.
               05  SC03-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
               05  SC03-SRYYMD   PIC X(24)   OCCURS  5 TIMES.
               05  SC03-TENKI1   PIC X(04)   OCCURS  5 TIMES.
               05  SC03-TENKI2   PIC X(04)   OCCURS  5 TIMES.
               05  SC03-TENKI3   PIC X(04)   OCCURS  5 TIMES.
               05  SC03-TEKIYO   PIC X(52)   OCCURS  35 TIMES.
      *減免テーブル
           03  SC03-GENMEN-TBL OCCURS 3.
               05  SC03-GENMEN         PIC  X(02).
      *低所得テーブル
           03  SC03-TEISHO-TBL OCCURS 3.
               05  SC03-TEISHO         PIC  X(02).
      *診療実日数
           03  SC03-NISSU              PIC  X(02) OCCURS 3.
      *請求点数
           03  SC03-TENSU              PIC  X(09) OCCURS 3.
      *一部負担金
           03  SC03-KOHFTN             PIC  X(09) OCCURS 3.
      *食事療養
           03  SC03-SHOKUJI-TBL OCCURS 3.
      *食事日数
               05  SC03-SNISSU         PIC  X(02).
      *請求金額
               05  SC03-SKINGK         PIC  X(09).
      *標準負短顎
               05  SC03-SFTNGK         PIC  X(09).
      *生年月日
           03  SC03-BIRTHDAY2          PIC  X(22).
