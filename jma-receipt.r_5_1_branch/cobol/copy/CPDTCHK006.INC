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
      *    データチェック中間ファイル     COPY (CPDTCHK006.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHK006-REC.
           03  DTCHK006-KEY.
               05  DTCHK006-PTID-X.
                   07  DTCHK006-PTID       PIC 9(10).
               05  DTCHK006-ZAINUM         PIC 9(08).
               05  DTCHK006-SRYRENNUM      PIC 9(03).
           03  DTCHK006-SRYSYUKBN          PIC X(03).
           03  DTCHK006-SRYCD              PIC X(09).
           03  DTCHK006-SURYO              PIC 9(05)V9(05).
           03  DTCHK006-MEISKYFLG          PIC X(01).
           03  DTCHK006-058-TANICD         PIC X(03).
           03  DTCHK006-058-SRYCD          PIC X(09).
           03  DTCHK006-INPUTNUM           PIC 9(03).
