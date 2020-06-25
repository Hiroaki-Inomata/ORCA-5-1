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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    半田     03.03.07  新規
      ************************************************************
       01  SKT1120.
           03 SKT1120-NO             PIC X(04).
           03 SKT1120-HKNJANUM       PIC X(06).
           03 SKT1120-HOSPCDN        PIC X(07).
           03 SKT1120-ZAI1           PIC X(02).
           03 SKT1120-ZAI2           PIC X(02).
           03 SKT1120-RJN-TBL OCCURS 2.
              05 SKT1120-RJN-NYUGAI-TBL OCCURS 2.
                07 SKT1120-KENSU     PIC X(05).
                07 SKT1120-NISSU     PIC X(05).
                07 SKT1120-TENSU     PIC X(09).
                07 SKT1120-ITBFTN    PIC X(09).
              05 SKT1120-SKENSU      PIC X(05).
              05 SKT1120-SNISSU      PIC X(05).
              05 SKT1120-SKINGK      PIC X(09).
              05 SKT1120-SFTNGK      PIC X(09).
           03 SKT1120-KOH-TBL OCCURS 3.
              05 SKT1120-KOHNAME       PIC X(10).
              05 SKT1120-KOHNUM        PIC X(02).
              05 SKT1120-KOHMARU       PIC X(02).
              05 SKT1120-KOH-NYUGAI-TBL OCCURS 2.
                07 SKT1120-K-KENSU     PIC X(05).
                07 SKT1120-K-NISSU     PIC X(05).
                07 SKT1120-K-TENSU     PIC X(09).
                07 SKT1120-K-ITBFTN    PIC X(09).
              05 SKT1120-K-SKENSU      PIC X(05).
              05 SKT1120-K-SNISSU      PIC X(05).
              05 SKT1120-K-SKINGK      PIC X(09).
              05 SKT1120-K-SFTNGK      PIC X(09).
           03 SKT1120-T-KENSU        PIC X(05).
           03 SKT1120-T-NISSU        PIC X(05).
           03 SKT1120-T-TENSU        PIC X(09).
