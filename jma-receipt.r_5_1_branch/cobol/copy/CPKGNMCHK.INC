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
      *         記号チェック初期パターン     (CPKGNMCHK.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  KGNMCHK-AREA.
         03  KGNMCHK-VAL.
           05   KGNMCHK-H001-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-H001-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGKANJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGABT        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGHYPN       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGKAKKO      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGETC        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-KGKANJINASHI PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H001-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H001-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-NM1KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM1ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM1KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H001-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H001-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-NM2KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM2ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H001-NM2KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H001-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-H031-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-H031-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGKANJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGABT        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGHYPN       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGKAKKO      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGETC        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGKANJINASHI PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H031-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H031-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM1KANJI     PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM1ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H031-NM1KAKKO     PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H031-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H031-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H031-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H031-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM2KANJI     PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM2ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM2KAKKO     PIC X(01)   VALUE   "1".
           05   KGNMCHK-H031-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-H033-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-H033-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGKANJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGABT        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGHYPN       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGKAKKO      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGETC        PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-KGKANJINASHI PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H033-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H033-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-NM1KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NM1ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NM1KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H033-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H033-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-NM2KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NM2ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H033-NM2KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H033-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-H006-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-H006-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H006-KGKANJI      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-KGABT        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-KGHYPN       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-KGKAKKO      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-KGETC        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-KGKANJINASHI PIC X(01)   VALUE   "1".
           05   KGNMCHK-H006-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H006-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H006-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H006-NM1KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM1ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM1KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H006-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H006-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H006-NM2KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM2ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H006-NM2KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H006-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-H039-KGCHECK      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGSUJI       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGKANJI      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGABT        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGHYPN       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGKAKKO      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGETC        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-KGKANJINASHI PIC X(01)   VALUE   "1".
           05   KGNMCHK-H039-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H039-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H039-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H039-NM1KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM1ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM1KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NMKETAMIN    PIC X(02)   VALUE   "08".
           05   KGNMCHK-H039-NMKETAMAX    PIC X(02)   VALUE   "08".
      *
           05   KGNMCHK-H039-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H039-NM2KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM2ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM2HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM2KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H039-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-H009-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-H009-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-KGKANJI      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-KGABT        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-KGHYPN       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-KGKAKKO      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-KGETC        PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-KGKANJINASHI PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H009-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H009-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-NM1KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM1ABT       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM1HYPN      PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM1KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM1ETC       PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-H009-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-H009-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-NM2KANJI     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM2ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-H009-NM2KAKKO     PIC X(01)   VALUE   "2".
           05   KGNMCHK-H009-NM2ETC       PIC X(01)   VALUE   "2".
      ****
           05   KGNMCHK-HDEF-KGCHECK      PIC X(01)   VALUE   "0".
           05   KGNMCHK-HDEF-KGSUJI       PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGKANJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGABT        PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGHYPN       PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGKAKKO      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGETC        PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGKANJINASHI PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-KGKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-HDEF-KGKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-HDEF-NM1SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM1KANJI     PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM1ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM1HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM1KAKKO     PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM1ETC       PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NMSUJINASHI  PIC X(01)   VALUE   "2".
           05   KGNMCHK-HDEF-NMKETAMIN    PIC X(02)   VALUE   SPACE.
           05   KGNMCHK-HDEF-NMKETAMAX    PIC X(02)   VALUE   SPACE.
      *
           05   KGNMCHK-HDEF-NM2SUJI      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM2KANJI     PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM2ABT       PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM2HYPN      PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM2KAKKO     PIC X(01)   VALUE   "1".
           05   KGNMCHK-HDEF-NM2ETC       PIC X(01)   VALUE   "1".
         03  KGNMCHK-R REDEFINES   KGNMCHK-VAL.
           05   KGNMCHK-OCC               OCCURS 7.
             07   KGNMCHK-KGCHECK         PIC X(01).
             07   KGNMCHK-KGSUJI          PIC X(01).
             07   KGNMCHK-KGKANJI         PIC X(01).
             07   KGNMCHK-KGABT           PIC X(01).
             07   KGNMCHK-KGHYPN          PIC X(01).
             07   KGNMCHK-KGKAKKO         PIC X(01).
             07   KGNMCHK-KGETC           PIC X(01).
             07   KGNMCHK-KGKANJINASHI    PIC X(01).
             07   KGNMCHK-KGKETAMIN       PIC X(02).
             07   KGNMCHK-KGKETAMAX       PIC X(02).
      *
             07   KGNMCHK-NM1SUJI         PIC X(01).
             07   KGNMCHK-NM1KANJI        PIC X(01).
             07   KGNMCHK-NM1ABT          PIC X(01).
             07   KGNMCHK-NM1HYPN         PIC X(01).
             07   KGNMCHK-NM1KAKKO        PIC X(01).
             07   KGNMCHK-NM1ETC          PIC X(01).
             07   KGNMCHK-NMSUJINASHI     PIC X(01).
             07   KGNMCHK-NMKETAMIN       PIC X(02).
             07   KGNMCHK-NMKETAMAX       PIC X(02).
      *
             07   KGNMCHK-NM2SUJI         PIC X(01).
             07   KGNMCHK-NM2KANJI        PIC X(01).
             07   KGNMCHK-NM2ABT          PIC X(01).
             07   KGNMCHK-NM2HYPN         PIC X(01).
             07   KGNMCHK-NM2KAKKO        PIC X(01).
             07   KGNMCHK-NM2ETC          PIC X(01).
         03  KGNMCHK-H001-IDX             PIC 9(03)  VALUE 1.
         03  KGNMCHK-H031-IDX             PIC 9(03)  VALUE 2.
         03  KGNMCHK-H033-IDX             PIC 9(03)  VALUE 3.
         03  KGNMCHK-H006-IDX             PIC 9(03)  VALUE 4.
         03  KGNMCHK-H039-IDX             PIC 9(03)  VALUE 5.
         03  KGNMCHK-H009-IDX             PIC 9(03)  VALUE 6.
         03  KGNMCHK-HDEF-IDX             PIC 9(03)  VALUE 7.
      *
