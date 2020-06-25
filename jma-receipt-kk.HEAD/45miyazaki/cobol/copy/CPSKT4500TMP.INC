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
      *  国保総括表         COPY (CPSKT4500TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      *  01.00.01    長屋     08/05/03    帳票様式に伴う修正
      *  01.00.02    吉川     09/05/07    帳票様式の変更に伴う修正
      *  01.00.03    門間     19/02/01    修正履歴コメント削除
      ************************************************************
       01  SKT4500TMP-REC.
           03  SKT4500TMP-KEY.
               05  SKT4500TMP-KENKBN                PIC  9(01).
               05  SKT4500TMP-RECEKA                PIC  X(02).
           03  SKT4500TMP-SEIKYU-TBL      OCCURS  6.
               05  SKT4500TMP-NYUGAIKBN   OCCURS  2.
                   07  SKT4500TMP-KENSU             PIC  9(06).
                   07  SKT4500TMP-TENSU             PIC  9(10).
           03  SKT4500TMP-GKENSU      PIC  9(06)   OCCURS  2 TIMES.
           03  SKT4500TMP-GTENSU      PIC  9(10)   OCCURS  2 TIMES.
