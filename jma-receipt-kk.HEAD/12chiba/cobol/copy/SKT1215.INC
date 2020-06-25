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
      ********************************************************************
      *  後期高齢者請求書(入院)（千葉県）     COPY (SKT1215.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/16    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT1215.
      *    診療年月
           03  SKT1215-SRYYM                PIC  X(16).
      *    請求年月日
           03  SKT1215-SEIYMD               PIC  X(22).
      *    医療機関住所
           03  SKT1215-ADRS                 PIC  X(100).
      *    医療機関名
           03  SKT1215-HOSPNAME             PIC  X(100).
      *    電話番号
           03  SKT1215-TEL                  PIC  X(15).
      *    開設者名
           03  SKT1215-KAISETUNAME          PIC  X(40).
      *    保険者番号
           03  SKT1215-HKNJANUM             PIC  X(4).
      *    医療機関コード
           03  SKT1215-HOSPCD               PIC  X(14).
      *    ページ番号
           03  SKT1215-NO                   PIC  X(04).
      *    請求テーブル
           03  SKT1215-SEIKYU-TBL     OCCURS  2.
      *    件数
               05  SKT1215-KENSU            PIC  X(05).
      *    実日数
               05  SKT1215-NISSU            PIC  X(05).
      *    点数
               05  SKT1215-TENSU            PIC  X(09).
      *    一部負担金
               05  SKT1215-ITBFTN           PIC  X(09).
      *    食事件数
               05  SKT1215-S-KENSU          PIC  X(05).
      *    食事実日数
               05  SKT1215-S-NISSU          PIC  X(05).
      *    食事金額
               05  SKT1215-S-KINGK          PIC  X(09).
      *    食事標準負担額
               05  SKT1215-S-FTNGK          PIC  X(09).
      *    公費テーブル
           03  SKT1215-KOH-TBL       OCCURS 6.
      *    法別番号
               05  SKT1215-HBTNUM           PIC  X(02).
      *    件数
               05  SKT1215-KKENSU           PIC  X(05).
      *    実日数
               05  SKT1215-KNISSU           PIC  X(05).
      *    点数
               05  SKT1215-KTENSU           PIC  X(09).
      *    一部負担金
               05  SKT1215-KITBFTN          PIC  X(09).
      *    食事件数
               05  SKT1215-S-KKENSU         PIC  X(05).
      *    食事実日数
               05  SKT1215-S-KNISSU         PIC  X(05).
      *    食事金額
               05  SKT1215-S-KKINGK         PIC  X(09).
      *    食事標準負担額
               05  SKT1215-S-KFTNGK         PIC  X(09).

