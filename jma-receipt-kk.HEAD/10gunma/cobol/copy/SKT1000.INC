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
       01  SKT1000.
      *    診療年月
              03  SKT1000-SRYYM            PIC  X(16).
      *    点数表区分
              03  SKT1000-TENHYOKBN-TBL  OCCURS  4.
                  05  SKT1000-TENHYOKBN    PIC  X(02).
      *    医療機関コード
              03  SKT1000-HOSPCDN          PIC  X(07).
      *    県内／県外
              03  SKT1000-KENKBN1          PIC  X(02).
              03  SKT1000-KENKBN2          PIC  X(02).
      *    郵便番号
      *        03  SKT1000-POST             PIC  X(18).
      *    住所
              03  SKT1000-ADRS             PIC  X(100).
      *    医療機関名
              03  SKT1000-HOSPNAME         PIC  X(100).
      *    電話番号
              03  SKT1000-TEL              PIC  X(15).
      *    開設者名
              03  SKT1000-KAISETUNAME      PIC  X(40).
      *    特別療養費
              03  SKT1000-TOKUBETU         PIC  X(10).
      *    請求テーブル
              03  SKT1000-SEIKYU-TBL    OCCURS  25.
      *    保険者名
                  05  SKT1000-HKNJANAME    PIC  X(20).
      *    在医総
                  05  SKT1000-ZAI          PIC  X(02).
      *    保険者番号
                  05  SKT1000-HKNJANUM     PIC  X(12).
      *    保険種別
                  05  SKT1000-SYUBETU-TBL   OCCURS  3.
                      07  SKT1000-SYUBETU  PIC  X(02).
      *    区分
                  05  SKT1000-KBN          PIC  X(01).
      *    割合
                  05  SKT1000-KYURATE      PIC  X(02).
      *    件数
                  05  SKT1000-KENSU        PIC  X(05).
      *    件数（公費用）
                  05  SKT1000-KKENSU       PIC  X(03).
      *    件数（福祉用）
                  05  SKT1000-FKENSU       PIC  X(03).
      *    点数
                  05  SKT1000-TENSU        PIC  X(09).
      *    合計
              03  SKT1000-SUM-AREA  OCCURS  2.
      *    合計：件数
                  05  SKT1000-GKENSU       PIC  X(05).
      *    合計：点数
                  05  SKT1000-GTENSU       PIC  X(10).
