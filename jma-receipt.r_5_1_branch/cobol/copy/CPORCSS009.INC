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
      *    科別・保険別入院会計編集サブ　COPY (CPORCSS009.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS009-AREA.
           03  SS009-REQUEST               PIC X(01).
           03  SS009-RRKNUM                PIC 9(03).
           03  SS009-SKYKBN                PIC X(01).
           03  SS009-STYMD                 PIC X(08).
           03  SS009-EDYMD                 PIC X(08).
           03  SS009-EXGAIHAKU             PIC X(01).
           03  SS009-OT-AREA.
               05  SS009-HENKYAKU-MAX      PIC 9(05).
               05  SS009-HENKYAKU-TBL      OCCURS  200.
                   07  SS009-SRYKA         PIC  X(02).
                   07  SS009-HKNCOMBI      PIC  X(04).
                   07  SS009-SRYKBN        PIC  X(02).
                   07  SS009-SRYSYUKBN     PIC  X(03).
                   07  SS009-ZAISKBKBN     PIC  X(01).
                   07  SS009-ZAITEN        PIC  9(08).
                   07  SS009-ZAIKAISU      PIC  9(07).
                   07  SS009-MEISAISU      PIC  9(07).
                   07  SS009-SRYCDTOTAL    PIC  9(14).
                   07  SS009-SURYOUTOTAL   PIC  9(07)V9(03).
                   07  SS009-RUIKEITOTAL   PIC  9(15)V9(03).
                   07  SS009-SAGAKUCD      PIC  X(02).
                   07  SS009-BRMNUM        PIC  X(08).
                   07  SS009-SRYCD-OCC     OCCURS  20.
                       09  SS009-SURYO     PIC  9(05).
                       09  SS009-SRYCD     PIC  X(09).
                   07  SS009-DAY-TBL.
                       09  SS009-DAY       PIC  9(03)
                                           OCCURS  31.
               05  SS009-DAYINF-G.
                   07  SS009-DAYINF        OCCURS  31.
                       09  SS009-SRYKA-DAY PIC  X(04).
                       09  SS009-BRMNUM-DAY
                                           PIC  X(08).
                       09  SS009-HKNCOMBI-DAY
                                           PIC  X(04).
                       09  SS009-SAGAKUCD-DAY
                                           PIC  X(02).
           03  SS009-RC                    PIC S9(9) BINARY.
