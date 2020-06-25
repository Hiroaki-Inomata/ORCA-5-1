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
      *    診療種別テーブルパラメタ                     *
      *                         COPY  (CPORCSSRYSYU.INC) *
      *
      *-------------------------------------------------*
      *  修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/10/09  最大数を１００へ
      ******************************************************************
       01  MSYU-DATA-REC.
      *****03  MSYU-TBL-OCC        OCCURS    70   INDEXED   MSYU-IDX.
           03  MSYU-TBL-OCC        OCCURS    200  INDEXED   MSYU-IDX.
               05  MSYU-SRYKBN         PIC X(02).
               05  MSYU-FILLER         PIC X(01).
               05  MSYU-SRYSYUKBN      PIC X(03).
               05  MSYU-SRYMEI         PIC X(40).
      *
