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
      *    診療報酬請求書     COPY (CPSKT0800TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01       長屋   08/04/26    帳票様式に伴う修正
      *  01.00.02       吉川   11/04/04    東北地方太平洋沖地震対応
      *  01.00.03       門間   18/10/09    不要コメントを削除
      ************************************************************
       01  SKT0800TMP-REC.
           03  SKT0800TMP-KEY.
               05  SKT0800TMP-PREFKBN           PIC  X(01).
               05  SKT0800TMP-GENMEN            PIC  X(01).
           03       SKT0800TMP-SEIKYU-TBL OCCURS 4.
                 05   SKT0800TMP-NYUGAI-TBL OCCURS 2.
                      07  SKT0800TMP-KENSU      PIC  9(07).
                      07  SKT0800TMP-TENSU      PIC  9(14).
                 05   SKT0800TMP-S-KENSU        PIC  9(07).
                 05   SKT0800TMP-S-KINGK        PIC  9(11).
           03  SKT0800TMP-SYOKBN-TBL OCCURS 2.
                 05    SKT0800TMP-KOH-TBL OCCURS 5.
                      07    SKT0800TMP-KKENSU        PIC  9(07).
                      07    SKT0800TMP-KTENSU        PIC  9(14).
