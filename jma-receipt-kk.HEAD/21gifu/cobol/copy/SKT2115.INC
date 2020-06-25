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
      *  後期高齢者請求書（岐阜県）     COPY (SKT2115.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/16    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT2115.
      *    NO
           03  SKT2115-NO                  PIC  X(04).
      *    診療年月
           03  SKT2115-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT2115-SEIYMD              PIC  X(22).
      *    住所
           03  SKT2115-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT2115-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT2115-KAISETUNAME         PIC  X(60).
      *    電話番号
           03  SKT2115-TEL                 PIC  X(15).
      *    保険者名
           03  SKT2115-HKNJANAME           PIC  X(100).
      *    保険者番号
           03  SKT2115-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT2115-HOSPCD              PIC  X(14).
      *    在宅
           03  SKT2115-ZAI1                PIC  X(02).
           03  SKT2115-ZAI2                PIC  X(02).
      *    診療科
           03  SKT2115-SRYKA               PIC  X(04).
      *    請求テーブル
           03  SKT2115-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT2115-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2115-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT2115-NISSU       PIC  X(05).
      *    点数
                   07  SKT2115-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT2115-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT2115-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT2115-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT2115-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2115-S-FTNGK         PIC  X(09).
      *    公費テーブル
           03  SKT2115-KOH-TBL  OCCURS  3.
      *    公費名
               05  SKT2115-KOHNAME         PIC  X(10).
      *    入外テーブル
               05  SKT2115-KNYUGAI-TBL   OCCURS  2.
      *    件数
                   07  SKT2115-KKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT2115-KNISSU      PIC  X(05).
      *    点数
                   07  SKT2115-KTENSU      PIC  X(09).
      *    一部負担金
                   07  SKT2115-KITBFTN     PIC  X(09).
      *    食事療養：件数
               05  SKT2115-S-KKENSU        PIC  X(05).
      *    食事療養：日数
               05  SKT2115-S-KNISSU        PIC  X(05).
      *    食事療養：金額
               05  SKT2115-S-KKINGK        PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2115-S-KFTNGK        PIC  X(09).
      *    長期：件数
           03  SKT2115-TYOKI-TBL  OCCURS  2.
               05  SKT2115-TKENSU          PIC  X(02).
      *    長処：件数
           03  SKT2115-TYOKIT-TBL  OCCURS  2.
               05  SKT2115-TTKENSU         PIC  X(02).
