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
      ******************************************************************
      *    データチェック診療会計テーブル      COPY  (CPDCACCT.INC)   *
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ************************************************************
      *01  DCACCT-REC.
           03  DCACCT-KEY.
               05  DCACCT-PTID         PIC 9(10).
               05  DCACCT-ZAINUM       PIC 9(08).
               05  DCACCT-ACCTKBN      PIC X(01).
               05  DCACCT-ACCTRENNUM   PIC 9(03).
           03  DCACCT-SRYYMD           PIC X(08).
           03  DCACCT-SRYKA            PIC X(02).
           03  DCACCT-SRYKBN           PIC X(02).
           03  DCACCT-SRYSYUKBN        PIC X(03).
           03  DCACCT-JIHOKBN          PIC X(01).
           03  DCACCT-HKNCOMBI         PIC 9(04).
           03  DCACCT-ZAITEN           PIC 9(08).
           03  DCACCT-ZAIKAISU         PIC 9(07).
           03  DCACCT-HKNNUM           PIC X(03).
           03  DCACCT-KOHNUM           PIC X(03).
           03  DCACCT-KOHID            PIC 9(10).
           03  DCACCT-RSI-HKNKBN       PIC X(01).
           03  DCACCT-RECESYUBETU      PIC X(04).
           03  DCACCT-DAY              PIC 9(02).
           03  DCACCT-DAYCNT           PIC 9(03).
           03  DCACCT-DAYCNT2-G.
               05  DCACCT-DAYCNT2-OCC  OCCURS 9.
                   07  DCACCT-DAYCNT2  PIC 9(03).
                   07  DCACCT-DAYCNT2-GROUP.
                       09  DCACCT-DAYCNT2-SRYKA
                                       PIC X(02).
                       09  DCACCT-DAYCNT2-DENPNUM
                                       PIC 9(07).
                   07  DCACCT-DAYCNT2-IDX
                                       PIC 9(03).
           03  DCACCT-NYUGAIKBN        PIC X(01).
           03  DCACCT-ZAIKBN           PIC 9(03).
           03  DCACCT-STATUS-G.
               05  DCACCT-STS-NYUKHN   PIC 9(01).
               05  DCACCT-STS-GAIHAKU  PIC 9(01).
               05  DCACCT-STS-NYUINDAY PIC 9(01).
               05  DCACCT-STS-NYUKSN   PIC 9(01).
               05  DCACCT-STS-NYUGEN   PIC 9(01).
               05  DCACCT-STS-JITDAY   PIC 9(01).
               05  DCACCT-STS-SNTDAY   PIC 9(01).
               05  DCACCT-STS-NODATA   PIC 9(01).
           03  DCACCT-INGAITEN         PIC 9(07).
           03  DCACCT-MINUS            PIC 9(01).
           03  DCACCT-TERMID           PIC X(16).
           03  DCACCT-OPID             PIC X(16).
           03  DCACCT-CREYMD           PIC X(08).
           03  DCACCT-UPYMD            PIC X(08).
           03  DCACCT-UPHMS            PIC X(06).
      *
           03  DCACCT-SELHOSPID        PIC X(24).
