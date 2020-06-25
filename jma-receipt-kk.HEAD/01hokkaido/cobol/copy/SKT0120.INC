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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    萩野    2018/09/10  修正履歴コメント削除
      ********************************************************************
        01  SKT0120.
            03  SKT0120-NO                      PIC X(04).
            03  SKT0120-SRYYM                   PIC X(16).
            03  SKT0120-HKNJANUM                PIC X(12).
            03  SKT0120-HOSPCD                  PIC X(07).
            03  SKT0120-ZAI1                    PIC X(02).
            03  SKT0120-ZAI2                    PIC X(02).
            03  SKT0120-TOKUBETU                PIC X(10).
            03  SKT0120-SEIKYU-TBL              OCCURS  2 TIMES.
                05  SKT0120-NYUGAI-TBL          OCCURS  2 TIMES.
                    07  SKT0120-KENSU           PIC X(05).
                    07  SKT0120-NISSU           PIC X(05).
                    07  SKT0120-TENSU           PIC X(09).
                    07  SKT0120-ITBFTN          PIC X(10).
                05  SKT0120-SKENSU              PIC X(05).
                05  SKT0120-SNISSU              PIC X(05).
                05  SKT0120-SKINGK              PIC X(09).
                05  SKT0120-SFTNGK              PIC X(09).
            03  SKT0120-DORO-TBL.
                05  SKT0120-D-KENSU             PIC X(05).
                05  SKT0120-D-NISSU             PIC X(05).
                05  SKT0120-D-TENSU             PIC X(09).
                05  SKT0120-D-ITBFTN            PIC X(09).
                05  SKT0120-D-SKENSU            PIC X(05).
                05  SKT0120-D-SNISSU            PIC X(05).
                05  SKT0120-D-SKINGK            PIC X(09).
                05  SKT0120-D-SFTNGK            PIC X(09).
            03  SKT0120-TYOKI-TBL               OCCURS  2 TIMES.
                05  SKT0120-T-RATE              PIC X(02).
                05  SKT0120-T-KENSU             PIC X(05).
                05  SKT0120-T-NISSU             PIC X(05).
                05  SKT0120-T-TENSU             PIC X(09).
                05  SKT0120-T-ITBFTN            PIC X(09).
            03  SKT0120-KOH-TBL                 OCCURS  5 TIMES.
                05  SKT0120-KOHNUM              PIC X(02).
                05  SKT0120-KOHMARU             PIC X(02).
                05  SKT0120-GENBAKU1            PIC X(04).
                05  SKT0120-GENBAKU2            PIC X(02).
                05  SKT0120-K-RATE-TBL          OCCURS  2 TIMES.
                    07  SKT0120-K-NYUGAI-TBL    OCCURS  2 TIMES.
                        09  SKT0120-K-RATE      PIC X(01).
                        09  SKT0120-K-KENSU     PIC X(05).
                        09  SKT0120-K-NISSU     PIC X(05).
                        09  SKT0120-K-TENSU     PIC X(09).
                        09  SKT0120-K-ITBFTN    PIC X(09).
                    07  SKT0120-K-SKENSU        PIC X(05).
                    07  SKT0120-K-SNISSU        PIC X(05).
                    07  SKT0120-K-SKINGK        PIC X(09).
                    07  SKT0120-K-SFTNGK        PIC X(09).
            03  SKT0120-BIKO-TBL.
                05  SKT0120-TYOKA               PIC X(04).
                05  SKT0120-TYOKA-MARU          PIC X(02).
                05  SKT0120-HYOKETU             PIC X(04).
                05  SKT0120-HYOKETU-MARU        PIC X(02).
