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
      *    診療報酬請求書     COPY (CPSKT1700TMPTMP.INC)              
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT1700TMP-REC.
           03  SKT1700TMP-KEY.
               05  SKT1700TMP-RECEKA           PIC  X(02).
            03  SKT1700TMP-SEIKYU-TBL      OCCURS  3 TIMES.
              05  SKT1700TMP-NYUGAI-TBL      OCCURS  2 TIMES.
                07  SKT1700TMP-KENSU   PIC 9(5).
                07  SKT1700TMP-NISSU   PIC 9(5).
                07  SKT1700TMP-TENSU   PIC 9(9).
                07  SKT1700TMP-ITBFTN  PIC 9(9).
              05  SKT1700TMP-SKENSU  PIC 9(5).
              05  SKT1700TMP-SKAISU  PIC 9(5).
              05  SKT1700TMP-SKINGK  PIC 9(9).
              05  SKT1700TMP-SFTNGK  PIC 9(9).
            03  SKT1700TMP-KOU-TBL.
              05  SKT1700TMP-NYUGAI-KOU-TBL          OCCURS  2 TIMES.
                07  SKT1700TMP-KOUKENSU    PIC 9(5).
                07  SKT1700TMP-KOUNISSU    PIC 9(5).
                07  SKT1700TMP-KOUTENSU    PIC 9(9).
                07  SKT1700TMP-KOUITBFTN   PIC 9(9).
              05  SKT1700TMP-KOUSKENSU   PIC 9(5).
              05  SKT1700TMP-KOUSKAISU   PIC 9(5).
              05  SKT1700TMP-KOUSKINGK   PIC 9(9).
              05  SKT1700TMP-KOUSFTNGK   PIC 9(9).
            03  SKT1700TMP-ROU-TBL.
              05  SKT1700TMP-NYUGAI-ROU-TBL          OCCURS  2 TIMES.
                07  SKT1700TMP-ROUKENSU    PIC 9(5).
                07  SKT1700TMP-ROUNISSU    PIC 9(5).
                07  SKT1700TMP-ROUTENSU    PIC 9(9).
                07  SKT1700TMP-ROUITBFTN   PIC 9(9).
              05  SKT1700TMP-ROUSKENSU   PIC 9(5).
              05  SKT1700TMP-ROUSKAISU   PIC 9(5).
              05  SKT1700TMP-ROUSKINGK   PIC 9(9).
              05  SKT1700TMP-ROUSFTNGK   PIC 9(9).
            03  SKT1700TMP-KOH-TBL.
              05  SKT1700TMP-NYUGAI-KOH-TBL          OCCURS  2 TIMES.
                07  SKT1700TMP-KKENSU  PIC 9(5).
                07  SKT1700TMP-KNISSU  PIC 9(5).
                07  SKT1700TMP-KTENSU  PIC 9(9).
                07  SKT1700TMP-KITBFTN PIC 9(9).
              05  SKT1700TMP-KSKENSU PIC 9(5).
              05  SKT1700TMP-KSKAISU PIC 9(5).
              05  SKT1700TMP-KSKINGK PIC 9(9).
              05  SKT1700TMP-KSFTNGK PIC 9(9).
