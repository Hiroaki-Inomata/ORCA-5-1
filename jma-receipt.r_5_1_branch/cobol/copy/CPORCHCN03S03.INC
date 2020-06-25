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
      *    入院請求書まとめサブパラメタ    COPY (CPORCHCN03S03.INC)
      *                                         (ORCHCN03S03 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCN03S03AREA.
           03  ORCHCN03S03-PTID            PIC 9(10).
           03  ORCHCN03S03-DENPNUM-G.
               05  ORCHCN03S03-DENPNUM     PIC 9(07)   OCCURS 10.
           03  ORCHCN03S03-SYUSTYMD        PIC X(08).
           03  ORCHCN03S03-SYUEDYMD        PIC X(08).
           03  ORCHCN03S03-OT.
               05  ORCHCN03S03-SKYSTYMD    PIC X(08).
               05  ORCHCN03S03-SKYEDYMD    PIC X(08).
               05  ORCHCN03S03-SRYYMD-MAX  PIC 9(03).
