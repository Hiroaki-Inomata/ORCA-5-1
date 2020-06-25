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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  SYUPARA03-AREA.
           03  SYUPARA03-DENPNUM           PIC 9(07).
           03  SYUPARA03-SEIKYU            PIC S9(10).
           03  SYUPARA03-SEIKYU-TAX-SAI    PIC S9(10).
           03  SYUPARA03-MISYU             PIC S9(10).
           03  SYUPARA03-MISYU-P           PIC S9(10).
           03  SYUPARA03-MISYU-M           PIC S9(10).
           03  SYUPARA03-NYU-MISYU         PIC S9(10).
           03  SYUPARA03-HENKIN            PIC S9(10).
           03  SYUPARA03-NYUKIN            PIC S9(10).
           03  SYUPARA03-KIKANKBN          PIC X(01).
           03  SYUPARA03-STSRYYMD          PIC X(08).
           03  SYUPARA03-EDSRYYMD          PIC X(08).
           03  SYUPARA03-JNISSU            PIC 9(02).
           03  SYUPARA03-HKNCOMBIMEI       PIC X(40).
           03  SYUPARA03-SRYKAMEI          PIC X(62).
           03  SYUPARA03-SRYKAMEI2         PIC X(30).
           03  SYUPARA03-SRYKA-G.
               05  SYUPARA03-SRYKA-MAX     PIC 9(03).
               05  SYUPARA03-SRYKA         PIC X(02)
                                           OCCURS  50.
           03  SYUPARA03-HKNCOMBI-G.
               05  SYUPARA03-HKNCOMBI-MAX  PIC 9(03).
               05  SYUPARA03-HKNCOMBI-OCC  OCCURS  50.
                   07  SYUPARA03-HKNCOMBI  PIC X(04).
                   07  SYUPARA03-PTFTNRATE PIC 9(03).
           03  SYUPARA03-GRP-DENPNUM-G.
               05  SYUPARA03-GRP-DENPNUM-MAX PIC 9(03).
               05  SYUPARA03-GRP-DENPNUM   PIC 9(07)
                                           OCCURS  100.
           03  SYUPARA03-SYU-DATA          PIC X(3000).
           03  SYUPARA03-HAKKOFLG              PIC 9(01).
           03  SYUPARA03-ORG-SEIKYU            PIC S9(10).
           03  SYUPARA03-ORG-SEIKYU-TAX-SAI    PIC S9(10).
           03  SYUPARA03-ORG-MISYU             PIC S9(10).
           03  SYUPARA03-ORG-NYUKIN            PIC S9(10).
