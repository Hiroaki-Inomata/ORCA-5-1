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
      *    後期高齢者総括表（高知県）     COPY (SKT3905.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/09    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3905.
      *    診療年月
           03  SKT3905-SRYYM                    PIC X(16).
      *    合計ページ数
           03  SKT3905-GPAGE                    PIC X(2).
      *    ページ数
           03  SKT3905-PAGE                     PIC X(2).
      *    請求年月日
           03  SKT3905-SEIYMD                   PIC X(22).
      *    医療機関コード
           03  SKT3905-HOSPCD                   PIC X(40).
      *    医療機関住所
           03  SKT3905-ADRS                     PIC X(100).
      *    医療機関名称
           03  SKT3905-HOSPNAME                 PIC X(100).
      *    開設者氏名
           03  SKT3905-KAISETUNAME              PIC X(40).
      *    電話番号
           03  SKT3905-TEL                      PIC X(15).
      *    請求テーブル
           03  SKT3905-SEIKYU-TBL      OCCURS  14.
      *    給付割合
               05  SKT3905-KYURATE              PIC X(02).
      *    保険者番号
               05  SKT3905-HKNJANUM             PIC X(12).
      *    入外テーブル
               05  SKT3905-NYUGAI-TBL      OCCURS  2.
      *    件数
                   07  SKT3905-KENSU            PIC X(05).
      *    点数
                   07  SKT3905-TENSU            PIC X(09).
      *    一部負担金
                   07  SKT3905-ITBFTN           PIC X(09).
      *    原爆テーブル
                   07  SKT3905-GENBK-TBL.
      *    原爆
                       09  SKT3905-GENMARU      PIC X(02).
                       09  SKT3905-GENBK1       PIC X(12).
                       09  SKT3905-GENBK2       PIC X(03).
      *    食事金額
               05  SKT3905-S-KINGK              PIC X(09).
      *    標準負担額
               05  SKT3905-S-FTNGK              PIC X(09).
      *    合計テーブル
           03  SKT3905-GOKEI-TBL       OCCURS  2.
      *    合計件数
               05  SKT3905-GKENSU               PIC X(05).
      *    合計点数
               05  SKT3905-GTENSU               PIC X(09).
