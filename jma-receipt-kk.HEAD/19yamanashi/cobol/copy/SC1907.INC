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
       01  SC07.
      *    請求年月日
           03  SC07-SEIYMD             PIC  X(22).
      *    提出年
           03  SC07-SEIY               PIC  X(04).
      *    提出月
           03  SC07-SEIM               PIC  X(04).
      *    医療機関コード
           03  SC07-HOSPCD             PIC  X(20).
      *    郵便番号
           03  SC07-POST               PIC  X(10).
      *    医療機関住所
           03  SC07-ADRS               PIC  X(100).
      *    医療機関名称
           03  SC07-HOSPNAME           PIC  X(100).
      *    開設者氏名
           03  SC07-KAISETUNAME        PIC  X(40).
      *    電話番号
           03  SC07-TEL                PIC  X(15).
      *    請求テーブル
           03  SC07-SEIKYU-TBL  OCCURS 4.
      *    レセプト写し件数
               05  SC07-KENSU          PIC  X(06).
      *    合計点数
               05  SC07-TENSU          PIC  X(10).
      *    担当者
           03  SC07-TANTOU             PIC  X(40).
      *    元号
           03  SC07-SEIGENGO           PIC  X(04).
