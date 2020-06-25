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
      *    後期高齢者請求書・在総診用（佐賀県）     COPY (SKT4125.INC) 
      *
      *  作成日付    作業者        記述
      *  08/05/21    吉川          新規作成
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    萩野         17/04/14  点数の桁数増加
      *  01.00.02    萩野         19/02/07  修正履歴コメント削除
      ********************************************************************
       01  SKT4125.
      *    ページ番号
           03  SKT4125-NO                  PIC  X(04).
      *    診療年月
           03  SKT4125-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4125-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4125-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4125-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4125-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4125-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT4125-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT4125-HOSPCD              PIC  X(14).
      *    保険者名
           03  SKT4125-HKNJANAME           PIC  X(100).
      *    給付割合
           03  SKT4125-KYURATE-TBL  OCCURS  4.
               05  SKT4125-KYURATE         PIC  X(02).
      *    請求テーブル
           03  SKT4125-SEIKYU-TBL  OCCURS  2.
      *    件数
               05  SKT4125-KENSU           PIC  X(05).
      *    診療実日数
               05  SKT4125-NISSU           PIC  X(05).
      *    点数
               05  SKT4125-TENSU           PIC  X(10).
      *    一部負担金
               05  SKT4125-ITBFTN          PIC  X(09).
      *
      *    公費請求テーブル
           03  SKT4125-KOUHI-TBL   OCCURS  9.
      *    法別番号
               05  SKT4125-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT4125-KOHNAME         PIC  X(10).
      *    件数
               05  SKT4125-KKENSU          PIC  X(05).
      *    診療実日数
               05  SKT4125-KNISSU          PIC  X(05).
      *    点数
               05  SKT4125-KTENSU          PIC  X(10).
      *    一部負担金
               05  SKT4125-KITBFTN         PIC  X(09).
      *    患者負担金
               05  SKT4125-KPTFTN          PIC  X(09).
