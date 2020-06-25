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
      *  後期高齢者請求書（山口県）     COPY (SKT3515.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/18    佐藤          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3515.
      *    診療年月
           03  SKT3515-SRYYM                    PIC X(16).
      *    請求年月日
           03  SKT3515-SEIYMD                   PIC X(22).
      *    医療機関コード
           03  SKT3515-HOSPCD                   PIC X(20).
      *    保険者番号
           03  SKT3515-HKNJANUM                 PIC X(04).
      *    保険者名
           03  SKT3515-HKNJANAME                PIC X(24).
      *    医療機関住所
           03  SKT3515-ADRS                     PIC X(100).
      *    医療機関名
           03  SKT3515-HOSPNAME                 PIC X(100).
      *    電話番号
           03  SKT3515-TEL                      PIC X(20).
      *    開設者名
           03  SKT3515-KAISETUNAME              PIC X(40).
      *    診療科コード
           03  SKT3515-SRYKACD                  PIC X(02).
      *    診療科名
           03  SKT3515-SRYKANAME                PIC X(20).
      *    在総診
           03  SKT3515-ZAI1                     PIC X(02).
           03  SKT3515-ZAI2                     PIC X(02).
      *    ページ番号
           03  SKT3515-NO                       PIC X(04).
      *    請求テーブル
           03  SKT3515-SEIKYU          OCCURS  2.
      *    入外テーブル
               05  SKT3515-NYUGAI-TBL      OCCURS  2.
      *    件数
                   07  SKT3515-KENSU            PIC X(05).
      *    日数
                   07  SKT3515-NISSU            PIC X(05).
      *    点数
                   07  SKT3515-TENSU            PIC X(09).
      *    一部負担金
                   07  SKT3515-ITBFTN           PIC X(09).
      *    食事療養：件数
               05  SKT3515-SKENSU               PIC X(05).
      *    食事療養：日数
               05  SKT3515-SNISSU               PIC X(05).
      *    食事療養：金額
               05  SKT3515-SKINGK               PIC X(09).
      *    食事療養：標準負担額
               05  SKT3515-SFTNGK               PIC X(09).
      *    公費請求テーブル
           03  SKT3515-K-SEIKYU        OCCURS  5.
      *    公費名
               05  SKT3515-K-KOHNAME            PIC X(10).
      *    公費：入外テーブル
               05  SKT3515-K-NYUGAI-TBL    OCCURS  2.
      *    公費：件数
                   07  SKT3515-K-KENSU          PIC X(05).
      *    公費：日数
                   07  SKT3515-K-NISSU          PIC X(05).
      *    公費：点数
                   07  SKT3515-K-TENSU          PIC X(09).
      *    公費：一部負担金
                   07  SKT3515-K-ITBFTN         PIC X(09).
      *    公費：食事療養：件数
               05  SKT3515-K-SKENSU             PIC X(05).
      *    公費：食事療養：日数
               05  SKT3515-K-SNISSU             PIC X(05).
      *    公費：食事療養：金額
               05  SKT3515-K-SKINGK             PIC X(09).
      *    公費：食事療養：標準負担額
               05  SKT3515-K-SFTNGK             PIC X(09).
