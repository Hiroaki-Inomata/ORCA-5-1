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
       01  SC05.
      *印刷日付
           03  SC05-PRTYM              PIC  X(16).
      *医療機関コード
           03  SC05-HOSPCDN            PIC  X(40).
      *保険区分
           03  SC05-HKNKBN-TBL OCCURS 4.
               05  SC05-HKNKBN         PIC  X(02).
      *単独併用区分
           03  SC05-HEIKBN-TBL OCCURS 2.
               05  SC05-HEIKBN         PIC  X(02).
      *本人家族区分
           03  SC05-HONKZKKBN-TBL OCCURS 5.
               05  SC05-HONKZKKBN      PIC  X(02).
      *老人市町村番号
           03  SC05-RJNFTNJANUM        PIC  X(16).
      *公費負担者番号
           03  SC05-FTNJANUM           PIC  X(16) OCCURS 2.
      *老人受給者番号
           03  SC05-RJNJKYSNUM         PIC  X(14).
      *公費受給者番号
           03  SC05-JKYSNUM            PIC  X(14) OCCURS 2.
      *保険者番号
           03  SC05-HKNJANUM           PIC  X(16).
      *被保険者証記号番号
           03  SC05-KIGO               PIC  X(34).
           03  SC05-BANGO              PIC  X(34).
      *患者番号
           03  SC05-PTNUM              PIC  X(20).
      *受給者氏名
           03  SC05-KANANAME           PIC  X(40).
           03  SC05-NAME               PIC  X(30).
      *生年月日
           03  SC05-BIRTHDAY           PIC  X(08).
      *性別
           03  SC05-SEX-TBL OCCURS 2.
               05  SC05-SEX            PIC  X(02).
      *生年月日：年号
           03  SC05-NENGO-TBL OCCURS 4.
               05  SC05-NENGO          PIC  X(02).
      *特記事項
           03  SC05-TOKKI              PIC  X(08).
           03  SC05-ZAISOU             PIC  X(04).
           03  SC05-ZAIMARU            PIC  X(02).
      *医療機関所在地
           03  SC05-ADRS               PIC  X(100).
      *医療機関名称
           03  SC05-HOSPNAME           PIC  X(100).
      *管理者名
           03  SC05-KANRINAME          PIC  X(80).
      *傷病名テーブル
           03  SC05-BYOMEI-TBL.
               05  SC05-BYOMEI   PIC X(54)   OCCURS  5 TIMES.
               05  SC05-SRYYMD   PIC X(24)   OCCURS  5 TIMES.
               05  SC05-TENKI1   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TENKI2   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TENKI3   PIC X(04)   OCCURS  5 TIMES.
               05  SC05-TEKIYO   PIC X(52)   OCCURS  35 TIMES.
      *診療実日数
           03  SC05-NISSU             PIC  X(02) OCCURS 3.
      *請求点数
           03  SC05-TENSU             PIC  X(07) OCCURS 3.
      *一部負担金
           03  SC05-KOHFTN            PIC  X(07) OCCURS 3.
      *食事療養
           03  SC05-SHOKUJI-TBL OCCURS 3.
      *食事日数
               05  SC05-SNISSU         PIC  X(02).
      *請求金額
               05  SC05-SKINGK         PIC  X(07).
      *標準負短顎
               05  SC05-SFTNGK         PIC  X(07).
