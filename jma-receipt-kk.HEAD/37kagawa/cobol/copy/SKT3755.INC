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
      *    後期高齢者請求書 （香川県）     COPY (SKT3755.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/23    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3755.
      *    印刷年月
           03  SKT3755-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT3755-ADRS                PIC X(100).
      *    医療機関TEL
           03  SKT3755-TEL                 PIC X(15).
      *    医療機関名称
           03  SKT3755-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT3755-KAISETUNAME         PIC X(40).
      *    医療機関コード
           03  SKT3755-HOSPCD              PIC X(14).
      *    保険者名
           03  SKT3755-HKNJANAME           PIC X(20).
      *    請求年月日
           03  SKT3755-SEIYMD              PIC X(22).
      *    在宅
           03  SKT3755-ZAI1                PIC X(02).
           03  SKT3755-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT3755-NO                  PIC X(04).
      *    請求テーブル
           03  SKT3755-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT3755-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT3755-KENSU       PIC X(05).
      *    日数
                   07  SKT3755-NISSU       PIC X(05).
      *    点数
                   07  SKT3755-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT3755-ITBFTN      PIC X(09).
      *    食事件数
               05  SKT3755-S-KENSU         PIC X(05).
      *    食事日数
               05  SKT3755-S-NISSU         PIC X(05).
      *    金額
               05  SKT3755-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT3755-S-FTNGK         PIC X(09).
      *    公費請求テーブル
           03   SKT3755-KOUHI-TBL  OCCURS 4.
      *    法別番号
               05  SKT3755-HBTNUM          PIC  X(04).
      *    公費名
               05  SKT3755-KOHNAME         PIC  X(10).
      *    公費入外テーブル
               05  SKT3755-KNYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT3755-KKENSU      PIC X(05).
      *    公費日数
                   07  SKT3755-KNISSU      PIC X(05).
      *    公費点数
                   07  SKT3755-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT3755-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT3755-S-KKENSU        PIC X(05).
      *    公費食事日数
               05  SKT3755-S-KNISSU        PIC X(05).
      *    公費食事金額
               05  SKT3755-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT3755-S-KFTNGK        PIC X(09).
