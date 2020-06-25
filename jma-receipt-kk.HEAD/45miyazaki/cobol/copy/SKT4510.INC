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
      *  01.00.01    吉川         11/02/10  特別療養費国保対応
      *  01.00.02    門間         19/02/01  修正履歴コメント削除
      ********************************************************************
        01  SKT4510.
            03  SKT4510-NO              PIC X(04).
            03  SKT4510-SRYYM           PIC X(16).
            03  SKT4510-SEIYMD          PIC X(22).
            03  SKT4510-ADRS            PIC X(100).
            03  SKT4510-HOSPNAME        PIC X(100).
            03  SKT4510-TEL             PIC X(15).
            03  SKT4510-KAISETU         PIC X(80).
            03  SKT4510-HKNJANUM        PIC X(12).
            03  SKT4510-HOSPCD          PIC X(14).
            03  SKT4510-HKNJANAME       PIC X(40).
            03  SKT4510-KYURATE         PIC X(02).
            03  SKT4510-ZAI1            PIC X(02).
            03  SKT4510-ZAI2            PIC X(02).
            03  SKT4510-PREFKBN         PIC X(12).
            03  SKT4510-TOKUBETU        PIC X(10).
            03  SKT4510-SEIKYU-TBL      OCCURS  9 TIMES.
                05  SKT4510-NYUGAI-TBL  OCCURS  2 TIMES.
                    07  SKT4510-KENSU   PIC X(05).
                    07  SKT4510-NISSU   PIC X(05).
                    07  SKT4510-TENSU   PIC X(09).
                    07  SKT4510-ITBFTN  PIC X(09).
                05  SKT4510-SKENSU      PIC X(05).
                05  SKT4510-SNISSU      PIC X(05).
                05  SKT4510-SKINGK      PIC X(09).
                05  SKT4510-SFTNGK      PIC X(09).
            03  SKT4510-HBTNUM          PIC X(2)  OCCURS  2 TIMES. 
            03  SKT4510-FTNRATE         PIC X(1)  OCCURS  2 TIMES.
