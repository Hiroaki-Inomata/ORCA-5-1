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
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    萩野     15/12/10    特記事項要素数追加
      ********************************************************************
       01  SC09.
      *印刷日付
           03  SC09-PRTYM              PIC  X(16).
      *医療機関コード
           03  SC09-HOSPCDN            PIC  X(40).
      *保険区分
           03  SC09-HKNKBN-TBL OCCURS 4.
               05  SC09-HKNKBN         PIC  X(02).
      *単独併用区分
           03  SC09-HEIKBN-TBL OCCURS 3.
               05  SC09-HEIKBN         PIC  X(02).
      *本人家族区分
           03  SC09-HONKZKKBN-TBL OCCURS 5.
               05  SC09-HONKZKKBN      PIC  X(02).
      *老人市町村番号
           03  SC09-RJNFTNJANUM        PIC  X(16).
      *公費負担者番号
           03  SC09-FTNJANUM           PIC  X(16) OCCURS 2.
      *老人受給者番号
           03  SC09-RJNJKYSNUM         PIC  X(14).
      *公費受給者番号
           03  SC09-JKYSNUM            PIC  X(14) OCCURS 2.
      *保険者番号
           03  SC09-HKNJANUM           PIC  X(16).
      *被保険者証記号番号
           03  SC09-KIGO               PIC  X(34).
           03  SC09-BANGO              PIC  X(34).
      *患者番号
           03  SC09-PTNUM              PIC  X(20).
      *受給者氏名
           03  SC09-NAME               PIC  X(30).
      *生年月日
           03  SC09-BIRTHDAY           PIC  X(08).
      *性別
           03  SC09-SEX-TBL OCCURS 2.
               05  SC09-SEX            PIC  X(02).
      *生年月日：年号
           03  SC09-NENGO-TBL OCCURS 4.
               05  SC09-NENGO          PIC  X(02).
      *特記事項
      *=== 特記事項要素数追加 by hagino(2015/12/10) start
      *     03  SC09-TOKKI              PIC  X(08).
           03  SC09-TOKKI-TBL          OCCURS 3.
               05  SC09-TOKKI          PIC  X(08).
      *=== 特記事項要素数追加 by hagino(2015/12/10) end
           03  SC09-ZAISOU             PIC  X(04).
           03  SC09-ZAIMARU            PIC  X(02).
      *医療機関所在地
           03  SC09-ADRS               PIC  X(100).
      *医療機関名称
           03  SC09-HOSPNAME           PIC  X(100).
      *管理者名
           03  SC09-KAISETUNAME        PIC  X(80).
      *傷病名
           03  SC09-BYOMEI-TBL.
               05  SC09-BYOMEI         PIC  X(54) OCCURS 5.
               05  SC09-SRYYMD         PIC  X(24) OCCURS 5.
               05  SC09-TEKIYO         PIC  X(52) OCCURS 35.
      *診療実日数
           03  SC09-NISSU              PIC  X(02).
      *請求点数
           03  SC09-TENSU              PIC  X(07).
      *薬剤一部負担金
           03  SC09-YKZFTN             PIC  X(07).
      *一部負担金
           03  SC09-ITBFTN             PIC  X(07).
      *一部負担金（公費）
           03  SC09-ITBFTN2            PIC  X(07).
      *食事療養(SC1410用エリア)
           03  SC09-SHOKUJI-TBL.
      *食事日数
               05  SC09-SNISSU         PIC  X(02).
      *請求金額
               05  SC09-SKINGK         PIC  X(07).
      *標準負短顎
               05  SC09-SFTNGK         PIC  X(07).
