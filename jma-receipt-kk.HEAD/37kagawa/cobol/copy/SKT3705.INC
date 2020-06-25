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
      *    後期高齢者請求書 （香川県）     COPY (SKT3705.INC) 
      *
      *  作成日付    作業者        記述
      *  08/04/15    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3705.
      *    印刷年月
           03  SKT3705-SRYYM               PIC X(16).
      *    医療機関住所
           03  SKT3705-ADRS                PIC X(100).
      *    医療機関名称
           03  SKT3705-HOSPNAME            PIC X(100).
      *    開設者名
           03  SKT3705-KAISETUNAME         PIC X(40).
      *    医療機関コード
           03  SKT3705-HOSPCD              PIC X(14).
      *    在総診・在医総
           03  SKT3705-ZAI1                PIC X(02).
           03  SKT3705-ZAI2                PIC X(02).
      *    ページ番号
           03  SKT3705-NO                  PIC X(04).
      *    請求テーブル
           03  SKT3705-SEIKYU-TBL   OCCURS 2.
      *    入外テーブル
               05  SKT3705-NYUGAI-TBL   OCCURS 2.
      *    件数
                   07  SKT3705-KENSU       PIC X(05).
      *    点数
                   07  SKT3705-TENSU       PIC X(09).
      *    一部負担金
                   07  SKT3705-ITBFTN      PIC X(09).
      *    食事件数
               05  SKT3705-S-KENSU         PIC X(05).
      *    金額
               05  SKT3705-S-KINGK         PIC X(09).
      *    標準負担金
               05  SKT3705-S-FTNGK         PIC X(09).
      *    公費（感染・精神）テーブル
           03  SKT3705-KZN-TBL   OCCURS 2.
      *    件数
               05  SKT3705-KZKENSU         PIC X(5).
      *    点数
               05  SKT3705-KZTENSU         PIC X(9).
      *    一部負担金
               05  SKT3705-KZITBFTN        PIC X(9).
      *    食事件数
               05  SKT3705-S-KZKENSU       PIC X(5).
      *    金額
               05  SKT3705-S-KZKINGK       PIC X(9).
      *    標準負担金
               05  SKT3705-S-KZFTNGK       PIC X(9).
      *    公費請求テーブル
           03   SKT3705-KOUHI-TBL  OCCURS 4.
      *    法別番号
               05  SKT3705-HBTNUM          PIC  X(04).
      *    公費名
               05  SKT3705-KOHNAME         PIC  X(10).
      *    公費入外テーブル
               05  SKT3705-KNYUGAI-TBL  OCCURS  2.
      *    公費件数
                   07  SKT3705-KKENSU      PIC X(05).
      *    公費点数
                   07  SKT3705-KTENSU      PIC X(09).
      *    公費一部負担金
                   07  SKT3705-KITBFTN     PIC X(09).
      *    公費食事件数
               05  SKT3705-S-KKENSU        PIC X(05).
      *    公費食事金額
               05  SKT3705-S-KKINGK        PIC X(09).
      *    公費標準負担金
               05  SKT3705-S-KFTNGK        PIC X(09).
