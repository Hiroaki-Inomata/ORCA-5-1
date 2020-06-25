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
      *  01.00.01    吉川      2008/04/29   新規作成(後期高齢者対応)
      ************************************************************
       01  SKT1825.
           03  SKT1825-SRYYM           PIC X(16).
           03  SKT1825-HKNJANUM        PIC X(16).
           03  SKT1825-ZAI1            PIC X(2).
           03  SKT1825-ZAI2            PIC X(2).
           03  SKT1825-HOSPCD          PIC X(14).
           03  SKT1825-HOSPNAME        PIC X(100).
           03  SKT1825-NO              PIC X(4).
           03  SKT1825-SEIKYU-TBL      OCCURS  2 TIMES.
             05  SKT1825-NYUGAI-TBL    OCCURS  2 TIMES.
               07  SKT1825-KENSU       PIC X(5).
               07  SKT1825-NISSU       PIC X(5).
               07  SKT1825-TENSU       PIC X(9).
               07  SKT1825-ITBFTN      PIC X(9).
             05  SKT1825-S-KENSU       PIC X(5).
             05  SKT1825-S-NISSU       PIC X(5).
             05  SKT1825-S-KINGK       PIC X(9).
             05  SKT1825-S-FTNGK       PIC X(9).
