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
      *----------------------------------------------------------*
      *    予約一覧・未院一覧出力パラメタ      COPY (CPORCHC14.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCHC14AREA.
      *    （１：予約一覧、２：未院一覧）
           03  ORCHC14-KBN             PIC X(01).
      *    予約日
           03  ORCHC14-YYKYMD          PIC X(08).
      *    診療内容区分
           03  ORCHC14-SRYNAIYO        PIC X(02).
      *    ドクターコード
           03  ORCHC14-DRCD-G.
               05  ORCHC14-DRCDKEY         PIC X(01).
               05  ORCHC14-DRCD            PIC X(04).
      *
