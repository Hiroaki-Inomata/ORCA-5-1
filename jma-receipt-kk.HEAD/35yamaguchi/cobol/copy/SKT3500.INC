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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2010/12/09   特別療養費国保対応
      *  01.00.02    萩野      2019/01/11   修正履歴コメント削除
      ********************************************************************
       01  SKT3500.
      *    診療年月
           03         SKT3500-SRYYM            PIC  X(16).
      *    医療機関コード
           03         SKT3500-HOSPCD           PIC  X(20).
      *    医療機関所在地
           03         SKT3500-ADRS             PIC  X(100).
      *    医療機関名称
           03         SKT3500-HOSPNAME         PIC  X(100).
      *    開設者氏名
           03         SKT3500-KAISETU          PIC  X(40).
      *    診療科コード
           03         SKT3500-SRYKACD          PIC  X(02).
      *    診療科名 
           03         SKT3500-SRYKANAME        PIC  X(20).
      *    特別療養費
           03         SKT3500-TOKUBETU         PIC  X(10).
      *    請求エリア
           03         SKT3500-SEIKYU-AREA OCCURS 6.
      *    件数
               05     SKT3500-KENSU            PIC  X(05).
      *    点数
               05     SKT3500-TENSU            PIC  X(09).
