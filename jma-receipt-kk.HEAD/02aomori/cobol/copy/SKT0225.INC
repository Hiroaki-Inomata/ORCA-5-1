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
      *  後期高齢者請求書（青森県）     COPY (SKT0225.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/17    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT0225.
      *    診療年月
           03  SKT0225-SRYYM             PIC  X(16).
      *    請求年月日
           03  SKT0225-SEIYMD            PIC  X(22).
      *    保険者名
           03  SKT0225-HKNJANAME         PIC  X(20).
      *    医療機関コード
           03  SKT0225-HOSPCD            PIC  X(40).
      *    医療機関住所
           03  SKT0225-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT0225-HOSPNAME          PIC  X(100).
      *    開設者名
           03  SKT0225-KAISETUNAME       PIC  X(40).
      *    電話番号
           03  SKT0225-TEL               PIC  X(15).
      *    在宅
           03  SKT0225-ZAI1              PIC X(02).
           03  SKT0225-ZAI2              PIC X(02).
      *    ページ番号
           03  SKT0225-NO                PIC  X(04).
      *    請求テーブル
           03  SKT0225-SEIKYU-TBL     OCCURS  2.
      *    入外テーブル
               05  SKT0225-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0225-KENSU     PIC  X(05).
      *    実日数
                   07  SKT0225-NISSU     PIC  X(05).
      *    点数
                   07  SKT0225-TENSU     PIC  X(09).
      *    一部負担金
                   07  SKT0225-ITBFTN    PIC  X(09).
      *    食事療養：件数
               05  SKT0225-S-KENSU       PIC  X(05).
      *    食事療養：日数
               05  SKT0225-S-NISSU       PIC  X(05).
      *    食事療養：金額
               05  SKT0225-S-KINGK       PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0225-S-FTNGK       PIC  X(09).
      *    公費テーブル
           03  SKT0225-KOH-TBL     OCCURS  5.
      *    公費名
               05  SKT0225-KOHNAME       PIC  X(10).
      *    法別番号
               05  SKT0225-HBTNUM        PIC  X(02).
      *    入外テーブル
               05  SKT0225-KNYUGAI-TBL      OCCURS  2.
      *    件数
                   07  SKT0225-KKENSU    PIC  X(05).
      *    実日数
                   07  SKT0225-KNISSU    PIC  X(05).
      *    点数
                   07  SKT0225-KTENSU    PIC  X(09).
      *    一部負担金
                   07  SKT0225-KITBFTN   PIC  X(09).
      *    食事療養：件数
               05  SKT0225-S-KKENSU      PIC  X(05).
      *    食事療養：日数
               05  SKT0225-S-KNISSU      PIC  X(05).
      *    食事療養：金額
               05  SKT0225-S-KKINGK      PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0225-S-KFTNGK      PIC  X(09).
