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
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         15/01/05  担当者欄の追加
      *  01.00.02    井上         19/04/17  元号対応
      ********************************************************************
       01  SC09.
      *    請求年月日
           03  SC09-SEIYMD             PIC  X(22).
      *    提出年
           03  SC09-SEIY               PIC  X(04).
      *    提出月
           03  SC09-SEIM               PIC  X(04).
      *    医療機関コード
           03  SC09-HOSPCD             PIC  X(20).
      *    郵便番号
           03  SC09-POST               PIC  X(10).
      *    医療機関住所
           03  SC09-ADRS               PIC  X(100).
      *    医療機関名称
           03  SC09-HOSPNAME           PIC  X(100).
      *    開設者氏名
           03  SC09-KAISETUNAME        PIC  X(40).
      *    電話番号
           03  SC09-TEL                PIC  X(15).
      *    請求テーブル
           03  SC09-SEIKYU-TBL  OCCURS  27.
      *    公費負担者番号
               05  SC09-FTNJANUM       PIC  X(04).
      *    市町村名
               05  SC09-CITYNAME       PIC  X(12).
      *    合計件数
               05  SC09-KENSU          PIC  X(05).
      *    合計点数
               05  SC09-TENSU          PIC  X(09).
      *    総合計ー件数
           03  SC09-GKENSU             PIC  X(06).
      *    総合計ー点数
           03  SC09-GTENSU             PIC  X(10).
      *    担当者
           03  SC09-TANTOU             PIC  X(40).
      *    元号
           03  SC09-SEIGENGO           PIC  X(04).
