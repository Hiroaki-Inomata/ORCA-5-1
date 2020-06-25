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
      *
       01  SDTCHK04AREA.
           03  SDTCHK04-IN.
               05  SDTCHK04-KIGONUM        PIC X(100).
           03  SDTCHK04-OT.
               05  SDTCHK04-SUJI           PIC 9(01).
               05  SDTCHK04-KANJI          PIC 9(01).
               05  SDTCHK04-ALPHABET       PIC 9(01).
               05  SDTCHK04-HYPHEN         PIC 9(01).
               05  SDTCHK04-TYOON          PIC 9(01).
               05  SDTCHK04-KAKKO          PIC 9(01).
               05  SDTCHK04-ETC            PIC 9(01).
               05  SDTCHK04-SPACE          PIC 9(01).
               05  SDTCHK04-KETA.
                   07  SDTCHK04-KETA9      PIC 9(02).
