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
      *    データチェック中間ファイル     COPY (CPDTCHK005.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHK005-REC.
           03  DTCHK005-KEY.
               05  DTCHK005-PTID-X.
                   07  DTCHK005-PTID   PIC 9(10).
               05  DTCHK005-ZAINUM     PIC 9(08).
               05  DTCHK005-ACCTRENNUM PIC 9(03).
           03  DTCHK005-SRYYMD         PIC X(08).
           03  DTCHK005-SRYKA          PIC X(02).
           03  DTCHK005-SRYKBN         PIC X(02).
           03  DTCHK005-SRYSYUKBN      PIC X(03).
           03  DTCHK005-JIHOKBN        PIC X(01).
           03  DTCHK005-HKNCOMBI       PIC X(04).
           03  DTCHK005-ZAITEN         PIC 9(08).
           03  DTCHK005-ZAIKAISU       PIC 9(07).
           03  DTCHK005-HKNNUM         PIC X(03).
           03  DTCHK005-HKNKBN         PIC X(01).
           03  DTCHK005-RECESYUBETU    PIC X(04).
           03  DTCHK005-DAY            PIC 9(02).
           03  DTCHK005-DAYCNT         PIC 9(03).
           03  DTCHK005-DAYCNT2-G.
               05  DTCHK005-DAYCNT2-OCC  OCCURS 9.
                   07  DTCHK005-DAYCNT2  PIC 9(03).
                   07  DTCHK005-DAYCNT2-GROUP.
                       09  DTCHK005-DAYCNT2-SRYKA
                                       PIC X(02).
                       09  DTCHK005-DAYCNT2-DENPNUM
                                       PIC 9(07).
                   07  DTCHK005-DAYCNT2-IDX
                                       PIC 9(03).
           03  DTCHK005-STATUS         PIC 9(05).
           03  DTCHK005-NYUGAIKBN      PIC X(01).
           03  DTCHK005-ZAIKBN         PIC 9(03).
           03  DTCHK005-INGAITEN       PIC 9(07).
           03  DTCHK005-MINUS          PIC 9(01).
