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
      *  レセプト作成管理               COPY (CPRECEUPD.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *    01  RECEUPD-REC.
            02  RECEUPD-HOSPNUM  PIC 9(2).
            02  RECEUPD-PTID    PIC S9(10).
            02  RECEUPD-NYUGAIKBN   PIC X(1).
            02  RECEUPD-SRYKA   PIC X(2).
            02  RECEUPD-SRYYM   PIC X(6).
            02  RECEUPD-HKNKBN  PIC X(1).
            02  RECEUPD-HKNKBN2 PIC X(1).
            02  RECEUPD-RECEYMD PIC X(8).
            02  RECEUPD-ONLUPD  PIC X(1).
            02  RECEUPD-ONLUPD1 PIC X(1).
            02  RECEUPD-ONLUPD2 PIC X(1).
            02  RECEUPD-ONLUPD3 PIC X(1).
            02  RECEUPD-ONLUPD4 PIC X(1).
            02  RECEUPD-ONLUPD5 PIC X(1).
            02  RECEUPD-ONLUPD6 PIC X(1).
            02  RECEUPD-ONLUPD7 PIC X(1).
            02  RECEUPD-ONLUPD8 PIC X(1).
            02  RECEUPD-ONLUPD9 PIC X(1).
            02  RECEUPD-ONLUPD10    PIC X(1).
            02  RECEUPD-TERMID  PIC X(64).
            02  RECEUPD-OPID    PIC X(16).
            02  RECEUPD-CREYMD  PIC X(8).
            02  RECEUPD-UPYMD   PIC X(8).
            02  RECEUPD-UPHMS   PIC X(6).
