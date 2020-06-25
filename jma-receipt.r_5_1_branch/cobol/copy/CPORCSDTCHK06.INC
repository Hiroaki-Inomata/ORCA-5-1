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
       01  SDTCHK06AREA.
           03  SDTCHK06-CHKPTN-G.
               05  SDTCHK06-CHKPTN-OCC         OCCURS  5.
                   07  SDTCHK06-CHKPTN         PIC X(01).
           03  SDTCHK06-IN.
               05  SDTCHK06-HOSPNUM            PIC 9(02).
               05  SDTCHK06-PTID               PIC 9(10).
               05  SDTCHK06-SRYKA              PIC X(02).
               05  SDTCHK06-CHKKBN             PIC X(01).
               05  SDTCHK06-SRYCD              PIC X(09).
               05  SDTCHK06-KJNYMD             PIC X(08).
               05  SDTCHK06-CDKBN              PIC X(01).
               05  SDTCHK06-NYUGAIKBN          PIC X(01).
               05  SDTCHK06-HKNKBN             PIC X(01).
           03  SDTCHK06-OT.
               05  SDTCHK06-O-MAX              PIC 9(05).
               05  SDTCHK06-O-OCC              OCCURS 100.
                   07  SDTCHK06-O-ID           PIC 9(09).
                   07  SDTCHK06-O-SRYKA        PIC X(02).
                   07  SDTCHK06-O-SRYYMD       PIC X(08).
                   07  SDTCHK06-O-PTBYOMEI     PIC X(160).
                   07  SDTCHK06-O-BYODAY-G.
                       09  SDTCHK06-O-BYODAY   PIC 9(01)   OCCURS  31.
