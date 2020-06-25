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
      *  後期高齢者医療総括表（群馬県）     COPY (SKT1005.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/17    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT1005.
      *    診療年月
              03  SKT1005-SRYYM            PIC  X(16).
      *    医療機関コード
              03  SKT1005-HOSPCD           PIC  X(07).
      *    住所
              03  SKT1005-ADRS             PIC  X(100).
      *    医療機関名
              03  SKT1005-HOSPNAME         PIC  X(100).
      *    電話番号
              03  SKT1005-TEL              PIC  X(15).
      *    開設者名
              03  SKT1005-KAISETUNAME      PIC  X(40).
      *    開設者名
              03  SKT1005-HYOBETU          PIC  X(02).
      *    請求テーブル
              03  SKT1005-SEIKYU-TBL    OCCURS  20.
      *    在医総
                  05  SKT1005-ZAI          PIC  X(02).
      *    保険者名
                  05  SKT1005-HKNJANAME    PIC  X(20).
      *    割合
                  05  SKT1005-KYURATE      PIC  X(02).
      *    入外テーブル
                  05  SKT1005-NYUGAI OCCURS  2.
      *    件数
                      07  SKT1005-KENSU        PIC  X(05).
      *    点数
                      07  SKT1005-TENSU        PIC  X(09).
      *    合計
              03  SKT1005-SUM-AREA  OCCURS  2.
                  05  SKT1005-G-NYUGAI  OCCURS  2.
      *    合計：件数
                      07  SKT1005-GKENSU       PIC  X(05).
      *    合計：点数
                      07  SKT1005-GTENSU       PIC  X(10).
