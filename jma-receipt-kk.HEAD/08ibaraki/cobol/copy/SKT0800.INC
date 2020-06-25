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
      *  プログラム修正履歴
      * Maj/Min/Rev     修正者   日付         内容
      *  01.00.01       長屋   08/04/26    帳票様式に伴う修正
      *  01.00.02       吉川   09/02/06    開設者名バイト数増加
      *  01.00.03       吉川   11/01/11    特別療養費国保対応
      *  01.00.04       門間   18/10/09    不要コメントを削除
      *****************************************************************
       01  SKT0800.
      *    診療年月
           03    SKT0800-SRYYM          PIC  X(16).
      *    医療機関コード
           03    SKT0800-HOSPCDN        PIC  X(20).
      *    医療機関住所
           03    SKT0800-ADRS           PIC  X(100).
      *    医療機関名称
           03    SKT0800-HOSPNAME       PIC  X(100).
      *    開設者名
           03    SKT0800-KAISETUNAME    PIC  X(60).
      *    特別療養費
           03    SKT0800-TOKUBETU       PIC  X(10).
           03       SKT0800-SEIKYU-TBL OCCURS 4.
                 05   SKT0800-NYUGAI-TBL OCCURS 2.
      *    件数
                      07  SKT0800-KENSU      PIC  X(07).
      *    点数
                      07  SKT0800-TENSU      PIC  X(14).
      *    食事件数
                 05   SKT0800-S-KENSU        PIC  X(07).
      *    食事金額
                 05   SKT0800-S-KINGK        PIC  X(11).
           03    SKT0800-KOH-TBL OCCURS 5.
      *    公費件数
                 05    SKT0800-KKENSU        PIC  X(07).
      *    公費点数
                 05    SKT0800-KTENSU        PIC  X(14).
