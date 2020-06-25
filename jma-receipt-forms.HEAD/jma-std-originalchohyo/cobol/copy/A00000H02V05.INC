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
      * 01.00.01     @@@@-門間    18/03/30  Ｈ３０年４月改正対応
      ********************************************************************
       01  H02V05.
          02 H02V05-TYPE-A.
            03  H02V05-PTNUM PIC X(20).
            03  H02V05-FTNJA PIC X(16).
            03  H02V05-FTNJAN    PIC X(20).
            03  H02V05-JKYSNUM   PIC X(14).
            03  H02V05-JKYSNUMN  PIC X(20).
            03  H02V05-JKYSNUM12 PIC X(24).
            03  H02V05-HKNJA PIC X(16).
            03  H02V05-KIGONUM   PIC X(32).
            03  H02V05-BIRTH PIC X(22).
            03  H02V05-KANANAME  PIC X(30).
            03  H02V05-KANANAME2 PIC X(60).
            03  H02V05-NAME  PIC X(26).
            03  H02V05-NAME2 PIC X(52).
            03  H02V05-SEX   PIC X(2).
            03  H02V05-HONKZKKBN PIC X(16).
            03  H02V05-KOFYMD    PIC X(22).
            03  H02V05-KIKYY PIC X(4).
            03  H02V05-KIKMM PIC X(4).
            03  H02V05-KIKDD PIC X(4).
            03  H02V05-HOSPADRS  PIC X(80).
            03  H02V05-HOSPNAME  PIC X(80).
            03  H02V05-HOSPTEL   PIC X(15).
            03  H02V05-DRNAME    PIC X(26).
            03  H02V05-FTNJA2    PIC X(16).
            03  H02V05-FTNJA2N   PIC X(20).
            03  H02V05-JKYSNUM2  PIC X(14).
            03  H02V05-JKYSNUM2N PIC X(20).
            03  H02V05-JKYSNUM212    PIC X(24).
            03  H02V05-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V05-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V05-BIKOH     PIC X(86)  OCCURS 5 TIMES.
            03  H02V05-BIKOH2    PIC X(40)  OCCURS 2 TIMES.
            03  H02V05-HKNINAME  PIC X(30).
            03  H02V05-SRYKAMEI  PIC X(10).
            03  H02V05-PAGE  PIC X(5).
            03  H02V05-FTNRATE   PIC X(3).
            03  H02V05-FTNWARI   PIC X(2).
            03  H02V05-KIGO  PIC X(32).
            03  H02V05-NUM   PIC X(32).
            03  H02V05-PREFNUM.
              04  H02V05-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V05-TENHYOKBN PIC X(2).
            03  H02V05-HOSPCD.
              04  H02V05-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V05-JKYSNUM15  PIC X(30).
            03  H02V05-JKYSNUM215 PIC X(30).
            03  H02V05-CHK1       PIC X(2).
            03  H02V05-CHK2       PIC X(2).
            03  H02V05-BUNKATU    PIC X(1).
            03  H02V05-KAISUU     PIC X(1).
            03  H02V05-BUNKATULBL PIC X(38).
          02 H02V05-TYPE-B.
            03  H02V05-B-PTNUM   PIC X(20).
            03  H02V05-B-FTNJA   PIC X(16).
            03  H02V05-B-FTNJAN  PIC X(20).
            03  H02V05-B-JKYSNUM PIC X(14).
            03  H02V05-B-JKYSNUMN    PIC X(20).
            03  H02V05-B-JKYSNUM12   PIC X(24).
            03  H02V05-B-HKNJA   PIC X(16).
            03  H02V05-B-KIGONUM PIC X(32).
            03  H02V05-B-BIRTH   PIC X(22).
            03  H02V05-B-KANANAME    PIC X(30).
            03  H02V05-B-KANANAME2   PIC X(60).
            03  H02V05-B-NAME    PIC X(26).
            03  H02V05-B-NAME2   PIC X(52).
            03  H02V05-B-SEX PIC X(2).
            03  H02V05-B-HONKZKKBN   PIC X(16).
            03  H02V05-B-KOFYMD  PIC X(22).
            03  H02V05-B-KIKYY   PIC X(4).
            03  H02V05-B-KIKMM   PIC X(4).
            03  H02V05-B-KIKDD   PIC X(4).
            03  H02V05-B-HOSPADRS    PIC X(80).
            03  H02V05-B-HOSPNAME    PIC X(80).
            03  H02V05-B-HOSPTEL PIC X(15).
            03  H02V05-B-DRNAME  PIC X(26).
            03  H02V05-B-FTNJA2  PIC X(16).
            03  H02V05-B-FTNJA2N PIC X(20).
            03  H02V05-B-JKYSNUM2    PIC X(14).
            03  H02V05-B-JKYSNUM2N   PIC X(20).
            03  H02V05-B-JKYSNUM212  PIC X(24).
            03  H02V05-B-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V05-B-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V05-B-BIKOH   PIC X(86)  OCCURS 5 TIMES.
            03  H02V05-B-BIKOH2  PIC X(40)  OCCURS 2 TIMES.
            03  H02V05-B-HKNINAME    PIC X(30).
            03  H02V05-B-SRYKAMEI    PIC X(10).
            03  H02V05-B-PAGE    PIC X(5).
            03  H02V05-B-FTNRATE PIC X(3).
            03  H02V05-B-FTNWARI PIC X(2).
            03  H02V05-B-KIGO    PIC X(32).
            03  H02V05-B-NUM PIC X(32).
            03  H02V05-B-PREFNUM.
              04  H02V05-B-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V05-B-TENHYOKBN   PIC X(2).
            03  H02V05-B-HOSPCD.
              04  H02V05-B-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V05-B-JKYSNUM15  PIC X(30).
            03  H02V05-B-JKYSNUM215 PIC X(30).
            03  H02V05-B-CHK1     PIC X(2).
            03  H02V05-B-CHK2     PIC X(2).
            03  H02V05-B-BUNKATU    PIC X(1).
            03  H02V05-B-KAISUU     PIC X(1).
            03  H02V05-B-BUNKATULBL PIC X(38).
          02  H02V05-FTNRATE2    PIC X(4).
          02  H02V05-B-FTNRATE2  PIC X(4).
