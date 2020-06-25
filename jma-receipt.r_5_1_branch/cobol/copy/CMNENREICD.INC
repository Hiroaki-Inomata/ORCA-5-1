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
      *-------------------------------------------------*
      *    年令設定テーブル   COPY (CMNENREICD.INC)    *
      *-------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  NENREI-DATA-REC.
      *
           03  FILLER          PIC X(02)  VALUE   "AA".
           03  FILLER          PIC X(05)  VALUE   "28 00".
      *
           03  FILLER          PIC X(02)  VALUE   "M1".
           03  FILLER          PIC X(05)  VALUE   "00 01".
      *
           03  FILLER          PIC X(02)  VALUE   "M2".
           03  FILLER          PIC X(05)  VALUE   "00 02".
      *
           03  FILLER          PIC X(02)  VALUE   "M3".
           03  FILLER          PIC X(05)  VALUE   "00 03".
      *
           03  FILLER          PIC X(02)  VALUE   "M4".
           03  FILLER          PIC X(05)  VALUE   "00 04".
      *
           03  FILLER          PIC X(02)  VALUE   "M5".
           03  FILLER          PIC X(05)  VALUE   "00 05".
      *
           03  FILLER          PIC X(02)  VALUE   "M6".
           03  FILLER          PIC X(05)  VALUE   "00 06".
      *
           03  FILLER          PIC X(02)  VALUE   "M7".
           03  FILLER          PIC X(05)  VALUE   "00 07".
      *
           03  FILLER          PIC X(02)  VALUE   "M8".
           03  FILLER          PIC X(05)  VALUE   "00 08".
      *
           03  FILLER          PIC X(02)  VALUE   "M9".
           03  FILLER          PIC X(05)  VALUE   "00 09".
      *
           03  FILLER          PIC X(02)  VALUE   "MA".
           03  FILLER          PIC X(05)  VALUE   "00 10".
      *
           03  FILLER          PIC X(02)  VALUE   "MB".
           03  FILLER          PIC X(05)  VALUE   "00 11".
      *
       01  NENREI-DATA-REC-R       REDEFINES   NENREI-DATA-REC.
           03  NENREI-TBL-OCC      OCCURS  12
                                       INDEXED NENREI-IDX.
               05  NENREI-NENCD        PIC X(02).
               05  NENREI-DD           PIC 9(02).
               05  NENREI-YOBI1        PIC X(01).
               05  NENREI-MM           PIC 9(02).
      *
       01  NENREI-MAX                  PIC 9(04)   VALUE   12.
      *
