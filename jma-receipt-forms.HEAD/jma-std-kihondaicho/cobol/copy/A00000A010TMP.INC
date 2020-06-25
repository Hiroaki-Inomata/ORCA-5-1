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
      * 01.00.01                  03.07.19  新規作成
      *              @@@@-門間    10/12/15  コメント履歴削除
      ********************************************************************
       01  A010T.
           03  A010T-KEY.
               05  A010T-OUTTYPE     PIC  X(02).
               05  A010T-NYUGAIKBN   PIC  X(01).
               05  A010T-PRSORTNUM   PIC  9(09)V9(02).
               05  A010T-KANANAME    PIC  X(200).
               05  A010T-BUNRUISUB   PIC  X(02).
               05  A010T-SRYCD       PIC  X(09).
               05  A010T-BUNRUICD    PIC  X(02).
               05  A010T-TEN2         PIC  9(08)V9(02).
           03  A010T-TENSIKIBETU     PIC  9(01).
           03  A010T-TANI            PIC  9(01).
           03  A010T-NAME            PIC  X(96).
           03  A010T-TEN             PIC  9(08)V9(02).
           03  A010T-KAISU           PIC  9(09)V9(03).
           03  A010T-TTEN            PIC  9(12)V9(02).
           03  A010T-KZMWAKE         PIC  9(01).
