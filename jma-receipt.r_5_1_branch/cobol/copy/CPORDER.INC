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
      *****************************************************************
      *    入院オーダ                COPY (CPORDER.INC)               *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  ORDER-REC.
           03  ORDER-KEY.
               05  ORDER-HOSPNUM        PIC 9(2).
               05  ORDER-KARTE-KEY     PIC X(36).
               05  ORDER-ORDERNUM      PIC 9(02).
           03  ORDER-NYUGAIKBN         PIC X(01).
           03  ORDER-PTNUM             PIC X(20).
           03  ORDER-PTID              PIC 9(10).
           03  ORDER-SRYKA             PIC X(02).
           03  ORDER-SRYYMD            PIC X(08).
           03  ORDER-HKNCOMBI          PIC X(04).
           03  ORDER-ORDERID           PIC X(04).
           03  ORDER-UKEYMD            PIC X(08).
           03  ORDER-UKEHMS            PIC X(08).
           03  ORDER-ERRCD1            PIC X(04).
           03  ORDER-ERRCD2            PIC X(04).
           03  ORDER-STATUS            PIC 9(01).
           03  ORDER-MODIFY            PIC 9(01).
           03  ORDER-KEYGENERATE       PIC 9(01).
           03  ORDER-RRKNUM            PIC 9(03).
           03  ORDER-RRKEDANUM         PIC 9(03).
           03  ORDER-TERMID            PIC X(16).
           03  ORDER-OPID              PIC X(16).
           03  ORDER-CREYMD            PIC X(08).
           03  ORDER-UPYMD             PIC X(08).
           03  ORDER-UPHMS             PIC X(06).
           03  ORDER-SELSTUKEYMD       PIC X(08).
           03  ORDER-SELEDUKEYMD       PIC X(08).
           03  ORDER-SELSTATUS         PIC X(01).
           03  ORDER-SELLIMIT          PIC 9(05).
           03  ORDER-SELOFFSET         PIC 9(05).
           03  ORDER-SELSORT           PIC X(02).
           03  ORDER-SELNUMBER         PIC 9(08).
