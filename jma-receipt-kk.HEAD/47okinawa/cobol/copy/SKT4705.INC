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
      *    後期高齢者総括表 （沖縄県）     COPY (SKT4705.INC) 
      *
      *  作成日付    作業者        記述
      *  08/05/03    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT4705.
      *    診療年月
           03  SKT4705-SRYYM                    PIC X(16).
      *    請求年月日
           03  SKT4705-SEIYMD                   PIC X(22).
      *    医療機関コード
           03  SKT4705-HOSPCD                   PIC X(14).
      *    医療機関住所
           03  SKT4705-ADRS                     PIC X(100).
      *    医療機関名称
           03  SKT4705-HOSPNAME                 PIC X(100).
      *    開設者名
           03  SKT4705-KAISETUNAME              PIC X(40).
      *    電話番号
           03  SKT4705-TEL                      PIC X(15).
      *    レセ科
           03  SKT4705-RECEKA                   PIC X(20).
      *    請求テーブル
           03  SKT4705-SEIKYU-TBL      OCCURS  2.
      *    給付割合テーブル
               05  SKT4705-KYURATE-TBL     OCCURS  2.
      *    件数
                   07  SKT4705-KENSU            PIC X(05).
      *    小計テーブル
           03  SKT4705-SYOKEI-TBL      OCCURS  2.
      *    小計件数
               05  SKT4705-SKENSU               PIC X(06).
      *    合計件数
           03  SKT4705-GKENSU                   PIC X(06).
      *    公費テーブル
           03  SKT4705-KOH-TBL         OCCURS  7.
      *    法別番号
               05  SKT4705-HBTNUM               PIC X(02).
      *    公費名
               05  SKT4705-KOHNAME              PIC X(10).
      *    公費入外テーブル
               05  SKT4705-KNYUGAI-TBL     OCCURS  2.
      *    公費件数
                   07  SKT4705-KKENSU           PIC X(05).
      *    公費合計件数
           03  SKT4705-GKKENSU                  PIC X(06).
