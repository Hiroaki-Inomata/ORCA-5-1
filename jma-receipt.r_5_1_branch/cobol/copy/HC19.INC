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
       01  HC19.
              03  HC19-PTNUM    PIC  X(20).
              03  HC19-FTNJA    PIC  X(16).
              03  HC19-JKYSNUM    PIC  X(14).
              03  HC19-HKNJA    PIC  X(16).
              03  HC19-KIGONUM    PIC  X(32).
              03  HC19-BIRTH    PIC  X(22).
              03  HC19-NAME    PIC  X(24).
              03  HC19-SEX    PIC  X(2).
              03  HC19-HONKZKKBN    PIC  X(16).
              03  HC19-KOFYMD    PIC  X(22).
              03  HC19-KIKYY    PIC  X(4).
              03  HC19-KIKMM    PIC  X(4).
              03  HC19-KIKDD    PIC  X(4).
              03  HC19-HOSPADRS    PIC  X(30).
              03  HC19-HOSPNAME    PIC  X(30).
              03  HC19-HOSPTEL    PIC  X(30).
              03  HC19-DRNAME    PIC  X(26).
              03  HC19-FTNKBN    PIC  X(20).
              03  HC19-FTNJA2    PIC  X(16).
              03  HC19-JKYSNUM2    PIC  X(14).
              03  HC19-NAIYO     PIC  X(86)   OCCURS  20.
      ******* 03  HC19-NAIYO1    PIC  X(86).
      *       03  HC19-NAIYO2    PIC  X(86).
      *       03  HC19-NAIYO3    PIC  X(86).
      *       03  HC19-NAIYO4    PIC  X(86).
      *       03  HC19-NAIYO5    PIC  X(86).
      *       03  HC19-NAIYO6    PIC  X(86).
      *       03  HC19-NAIYO7    PIC  X(86).
      *       03  HC19-NAIYO8    PIC  X(86).
      *       03  HC19-NAIYO9    PIC  X(86).
      *       03  HC19-NAIYO10    PIC  X(86).
      *       03  HC19-NAIYO11    PIC  X(86).
      *       03  HC19-NAIYO12    PIC  X(86).
      *       03  HC19-NAIYO13    PIC  X(86).
      *       03  HC19-NAIYO14    PIC  X(86).
      *       03  HC19-NAIYO15    PIC  X(86).
      *       03  HC19-NAIYO16    PIC  X(86).
      *       03  HC19-NAIYO17    PIC  X(86).
      *       03  HC19-NAIYO18    PIC  X(86).
      *       03  HC19-NAIYO19    PIC  X(86).
      ********03  HC19-NAIYO20    PIC  X(86).
              03  HC19-MENJYO     PIC  X(6).
              03  HC19-BIKOH      PIC  X(86)  OCCURS  3.
