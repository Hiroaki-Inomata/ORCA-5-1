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
      *    後期高齢者請求書(外来)（東京都）     COPY (SKT1325.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/16    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付        内容
      *  01.00.01    門間         2019/09/02  開設者氏名の印字文字数の増加
      ********************************************************************
       01  SKT1325.
      *    診療年月
           03  SKT1325-SRYYM             PIC  X(16).
      *    医療機関住所
           03  SKT1325-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT1325-HOSPNAME          PIC  X(100).
      *    電話番号
           03  SKT1325-TEL               PIC  X(15).
      *    開設者名
           03  SKT1325-KAISETUNAME       PIC  X(40).
      *    請求年月日
           03  SKT1325-SEIYMD            PIC  X(22).
      *    保険者名
           03  SKT1325-HKNJANANME        PIC  X(100).
      *    保険者番号
           03  SKT1325-HKNJANUM          PIC  X(12).
      *    医療機関コード
           03  SKT1325-HOSPCD            PIC  X(14).
      *    在総診
           03  SKT1325-ZAI1              PIC  X(02).
           03  SKT1325-ZAI2              PIC  X(02).
      *    ページ番号
           03  SKT1325-NO                PIC  X(04).
      *    請求テーブル
           03  SKT1325-SEIKYU-TBL     OCCURS  2.
      *    件数
               05  SKT1325-KENSU         PIC  X(05).
      *    実日数
               05  SKT1325-NISSU         PIC  X(05).
      *    点数
               05  SKT1325-TENSU         PIC  X(11).
      *    一部負担金
               05  SKT1325-ITBFTN        PIC  X(09).
      *    公費名
           03  SKT1325-KOHNAME           PIC  X(16).
      *    件数
           03  SKT1325-KKENSU            PIC  X(05).
      *    日数
           03  SKT1325-KNISSU            PIC  X(05).
      *    点数
           03  SKT1325-KTENSU            PIC  X(11).
      *    一部負担金
           03  SKT1325-KITBFTN           PIC  X(09).
      *    開設者名２
           03  SKT1325-KAISETUNAME2      PIC  X(60).
