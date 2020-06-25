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
      *  01.00.01    田中      2008/04/26   新規作成(後期高齢者対応)
      *  01.00.02    吉川      2008/05/16   県名を印字
      *  01.00.03    萩野      2018/11/07   修正履歴コメント削除
      ************************************************************
       01  SKT1815.
           03  SKT1815-SRYYM           PIC X(16).
           03  SKT1815-HKNJANAME       PIC X(20).
           03  SKT1815-ZAI1            PIC X(2).
           03  SKT1815-ZAI2            PIC X(2).
           03  SKT1815-SEIYMD          PIC X(22).
           03  SKT1815-HOSPCD          PIC X(14).
           03  SKT1815-ADRS            PIC X(100).
           03  SKT1815-HOSPNAME        PIC X(100).
           03  SKT1815-KAISETUNAME     PIC X(40).
           03  SKT1815-NO              PIC X(4).
           03  SKT1815-SEIKYU-TBL      OCCURS  2 TIMES.
             05  SKT1815-NYUGAI-TBL    OCCURS  2 TIMES.
               07  SKT1815-KENSU       PIC X(5).
               07  SKT1815-NISSU       PIC X(5).
               07  SKT1815-TENSU       PIC X(9).
               07  SKT1815-ITBFTN      PIC X(9).
             05  SKT1815-S-KENSU       PIC X(5).
             05  SKT1815-S-NISSU       PIC X(5).
             05  SKT1815-S-KINGK       PIC X(9).
             05  SKT1815-S-FTNGK       PIC X(9).
           03  SKT1815-KOUHI-TBL       OCCURS  2 TIMES.
             05  SKT1815-KOHNAME       PIC X(10).
             05  SKT1815-HBTNUM        PIC X(2).
             05  SKT1815-KNYUGAIKBN    OCCURS  2 TIMES.
               07  SKT1815-KKENSU      PIC X(5).
               07  SKT1815-KNISSU      PIC X(5).
               07  SKT1815-KTENSU      PIC X(9).
               07  SKT1815-KITBFTN     PIC X(9).
             05  SKT1815-S-KKENSU      PIC X(5).
             05  SKT1815-S-KNISSU      PIC X(5).
             05  SKT1815-S-KKINGK      PIC X(9).
             05  SKT1815-S-KFTNGK      PIC X(9).
