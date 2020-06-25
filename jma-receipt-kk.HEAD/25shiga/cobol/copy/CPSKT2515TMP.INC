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
      *                     COPY (CPSKT2515TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/09/16  都道府県番号追加
      *  01.00.02    井上         18/12/12  修正履歴コメント削除
      ************************************************************
       01  SKT2515TMP-REC.
           03  SKT2515TMP-KEY.
               05  SKT2515TMP-KBN                   PIC  9(01).
               05  SKT2515TMP-PREFNUM               PIC  X(02).
               05  SKT2515TMP-HKNJANUM              PIC  X(06).
               05  SKT2515TMP-KYURATE               PIC  9(03).
               05  SKT2515TMP-IDX                   PIC  9(02).
           03  SKT2515TMP-SEIKYU.
               07  SKT2515TMP-KENSU                 PIC  9(06).
