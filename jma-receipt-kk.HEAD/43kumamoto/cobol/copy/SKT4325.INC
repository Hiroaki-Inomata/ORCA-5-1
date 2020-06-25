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
      *  後期高齢者請求表（熊本県）     COPY (SKT4325.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/18    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         12/03/14  点数と金額の桁数増加
      *  01.00.02    萩野         14/07/10  食事日数の桁数増加
      *  01.00.03    門間         18/09/05  不要コメントを削除
      ********************************************************************
       01  SKT4325.
      *    ページ番号
           03  SKT4325-NO                  PIC  X(04).
      *    診療年月
           03  SKT4325-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4325-SEIYMD              PIC  X(22).
      *    医療機関所在地
           03  SKT4325-ADRS                PIC  X(100).
      *    医療機関名称
           03  SKT4325-HOSPNAME            PIC  X(100).
      *    電話番号
           03  SKT4325-TEL                 PIC  X(15).
      *    開設者氏名
           03  SKT4325-KAISETUNAME         PIC  X(40).
      *    医療機関コード
           03  SKT4325-HOSPCD              PIC  X(40).
      *    保険者名
           03  SKT4325-HKNJANAME           PIC  X(20).
      *    一般・在宅メッセージ
           03  SKT4325-ZAI                 PIC  X(32).
      *    療養の給付テーブル
           03  SKT4325-SEIKYU-TBL  OCCURS  2.
               05  SKT4325-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07 SKT4325-KENSU        PIC  X(05).
      *    日数
                   07 SKT4325-NISSU        PIC  X(05).
      *    点数
                   07 SKT4325-TENSU        PIC  X(10).
      *    一部負担金
                   07 SKT4325-ITBFTN       PIC  X(09).
      *    食事件数
               05  SKT4325-S-KENSU         PIC  X(05).
      *    食事日数
               05  SKT4325-S-NISSU         PIC  X(06).
      *    食事療養費
               05  SKT4325-S-KINGK         PIC  X(10).
      *    標準負担額
               05  SKT4325-S-FTNGK         PIC  X(09).
      *    公費テーブル
           03  SKT4325-KOUHI-TBL  OCCURS  5.
      *    法別番号
               05  SKT4325-HBTNUM          PIC  X(02).
      *    公費入外テーブル
               05  SKT4325-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07 SKT4325-KKENSU       PIC  X(05).
      *    日数
                   07 SKT4325-KNISSU       PIC  X(05).
      *    点数
                   07 SKT4325-KTENSU       PIC  X(10).
      *    一部負担金
                   07 SKT4325-KITBFTN      PIC  X(09).
      *    食事件数
               05  SKT4325-S-KKENSU        PIC  X(05).
      *    食事日数
               05  SKT4325-S-KNISSU        PIC  X(06).
      *    食事療養費
               05  SKT4325-S-KKINGK        PIC  X(10).
      *    標準負担額
               05  SKT4325-S-KFTNGK        PIC  X(09).
