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
      *    診療報酬請求書     COPY (CPSKT1610TMP.INC)
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/02/17  open-cobol1.0対応
      *  01.00.02    門間         20/01/07  修正履歴コメント削除
      ********************************************************************
       01  SKT1610TMP-REC.
           03  SKT1610TMP-KEY.
               05  SKT1610TMP-SYUTURYOKU              PIC 9(01).
               05  SKT1610TMP-HKNJANUM                PIC X(06).
               05  SKT1610TMP-KYURATE                 PIC 9(03).
           03  SKT1610TMP-TBL             OCCURS  11.
               05  SKT1610TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT1610TMP-KENSU               PIC 9(05).
                   07  SKT1610TMP-NISSU               PIC 9(05).
                   07  SKT1610TMP-TENSU               PIC 9(08).
                   07  SKT1610TMP-ITBFTN              PIC 9(08).
                   07  SKT1610TMP-TYOKI               PIC 9(04).
               05  SKT1610TMP-SKENSU                  PIC 9(05).
               05  SKT1610TMP-SNISSU                  PIC 9(05).
               05  SKT1610TMP-SKINGK                  PIC 9(08).
               05  SKT1610TMP-SFTNGK                  PIC 9(08).
