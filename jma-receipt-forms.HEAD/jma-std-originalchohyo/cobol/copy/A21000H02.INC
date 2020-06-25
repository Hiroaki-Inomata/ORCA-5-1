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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
        01  HCM19.
            02  HCM19-FTNJA       PIC X(16).
            02  HCM19-FTNJAN      PIC X(20).
            02  HCM19-JKYSNUM     PIC X(14).
            02  HCM19-JKYSNUMN    PIC X(20).
            02  HCM19-HKNJA       PIC X(16).
            02  HCM19-KIGONUM     PIC X(32).
            02  HCM19-KIGO        PIC X(32).
            02  HCM19-NUM         PIC X(32).
            02  HCM19-BIRTH-G1    PIC X(2).
            02  HCM19-BIRTH-G2    PIC X(2).
            02  HCM19-BIRTH-G3    PIC X(2).
            02  HCM19-BIRTH-G4    PIC X(2).
            02  HCM19-BIRTH-YY    PIC X(4).
            02  HCM19-BIRTH-MM    PIC X(4).
            02  HCM19-BIRTH-DD    PIC X(4).
            02  HCM19-KANANAME    PIC X(40).
            02  HCM19-NAME        PIC X(30).
            02  HCM19-SEX1        PIC X(2).
            02  HCM19-SEX2        PIC X(2).
            02  HCM19-HONKZKKBN1  PIC X(12).
            02  HCM19-HONKZKKBN2  PIC X(12).
            02  HCM19-KOFYY       PIC X(4).
            02  HCM19-KOFMM       PIC X(4).
            02  HCM19-KOFDD       PIC X(4).
            02  HCM19-KIKYY       PIC X(4).
            02  HCM19-KIKMM       PIC X(4).
            02  HCM19-KIKDD       PIC X(4).
            02  HCM19-HOSPADRS    PIC X(80).
            02  HCM19-HOSPNAME    PIC X(80).
            02  HCM19-HOSPTEL     PIC X(15).
            02  HCM19-DRNAME      PIC X(26).
            02  HCM19-FTNJA2      PIC X(16).
            02  HCM19-FTNJA2N     PIC X(20).
            02  HCM19-JKYSNUM2    PIC X(14).
            02  HCM19-JKYSNUM2N   PIC X(20).
            02  HCM19-NAIYO       PIC X(86)   OCCURS 20 TIMES.
            02  HCM19-MENJYO      PIC X(6).
            02  HCM19-BIKOH       PIC X(120)  OCCURS  4 TIMES.
            02  HCM19-JKYSNUMB    PIC X(20)   OCCURS  2 TIMES.
            02  HCM19-HKNINAME       PIC X(16).
            02  HCM19-PREFNUM.
              05  HCM19-PREFNUM-G    PIC X(02)   OCCURS  2 TIMES.
            02  HCM19-TENHYOKBN      PIC X(02).
            02  HCM19-HOSPCD.
              05  HCM19-HOSPCD-G     PIC X(02)   OCCURS  7 TIMES.
