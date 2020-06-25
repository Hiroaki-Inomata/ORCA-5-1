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
      *    後期高齢者請求書 （愛媛県）     COPY (SKT3825.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/28    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付         内容
      *  01.00.01    吉川    2008/05/09   桁ズレ修正
      *  01.00.02    吉川    2011/08/19   開設者名のバイト数増加
      *  01.00.03    門間    2018/09/05   不要コメントを削除
      ********************************************************************
       01  SKT3825.
      *    印刷年月
           03  SKT3825-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT3825-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT3825-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT3825-KAISETUNAME         PIC X(60).
      *    保険者番号
           03  SKT3825-HKNJANUM            PIC X(16).
      *    医療機関コード
           03  SKT3825-HOSPCD              PIC X(40).
      *    保険者名
           03  SKT3825-HKNJANAME           PIC X(100).
      *    請求年月日
           03  SKT3825-SEIYMD              PIC X(22).
      *    在宅
           03  SKT3825-ZAI1                PIC X(02).
           03  SKT3825-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT3825-NO                  PIC X(04).
      *    請求テーブル
           03  SKT3825-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT3825-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT3825-KENSU       PIC X(05).
      *    日数
                   07  SKT3825-NISSU       PIC X(05).
      *    点数
                   07  SKT3825-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT3825-ITBFTN      PIC X(09).
      *    食事件数
               05  SKT3825-S-KENSU         PIC X(05).
      *    食事日数
               05  SKT3825-S-NISSU         PIC X(05).
      *    金額
               05  SKT3825-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT3825-S-FTNGK         PIC X(09).
      *    公費請求テーブル
           03   SKT3825-KOUHI-TBL.
      *    公費入外テーブル
               05  SKT3825-K-NYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT3825-KKENSU      PIC X(05).
      *    公費日数
                   07  SKT3825-KNISSU      PIC X(05).
      *    公費点数
                   07  SKT3825-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT3825-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT3825-S-KKENSU        PIC X(05).
      *    公費食事日数
               05  SKT3825-S-KNISSU        PIC X(05).
      *    公費食事金額
               05  SKT3825-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT3825-S-KFTNGK        PIC X(09).
