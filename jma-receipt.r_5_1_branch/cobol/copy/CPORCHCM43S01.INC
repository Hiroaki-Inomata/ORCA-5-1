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
      *    外来月別請求書サブ      COPY (CPORCHCM43S01.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  HCM43S01-AREA.
      *返却項目
           03  HCM43S01-HENKYAKU-AREA.
      *
               05  HCM43S01-GRP-MAX            PIC 9(05).
               05  HCM43S01-GRP-OCC   OCCURS  50.
                   07  HCM43S01-GRP-SRYKA-MAX  PIC 9(05).
                   07  HCM43S01-GRP-SRYKA      PIC X(02)
                                      OCCURS  50.
                   07  HCM43S01-GRP-HKNCOMBI-MAX  PIC 9(05).
                   07  HCM43S01-GRP-HKNCOMBI-OCC  OCCURS 50.
                       09  HCM43S01-GRP-HKNCOMBI  PIC X(04).
                       09  HCM43S01-GRP-PTFTNRATE PIC 9(03).
      *
