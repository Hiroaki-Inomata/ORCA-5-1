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
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
       01  M01.
           03  M01-PAGE               PIC X(03).
           03  M01-SRYYM              PIC X(16).
           03  M01-SYSYMD             PIC X(22).
           03  M01-NYUGAI             PIC X(04).
           03  M01-SRYKANAME          PIC X(20).
           03  M01-NENREIKBN          PIC X(18)  OCCURS 2.
           03  M01-NENREIKBN2         PIC X(12)  OCCURS 2.
           03  M01-MEISAI-TBL         OCCURS 47.
               05  M01-KENSU          PIC X(06).
               05  M01-NISSU          PIC X(04).
               05  M01-TENSU          PIC X(09).
               05  M01-SYUEKI         PIC X(09).

