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
      * 01.00.01     @@@@-門間    12/03/23  新規作成（Ｈ２４年４月改正対応）
      ********************************************************************
       01  H02V03.
          02 H02V03-TYPE-A.
            03  H02V03-PTNUM PIC X(20).
            03  H02V03-FTNJA PIC X(16).
            03  H02V03-FTNJAN    PIC X(20).
            03  H02V03-JKYSNUM   PIC X(14).
            03  H02V03-JKYSNUMN  PIC X(20).
            03  H02V03-JKYSNUM12 PIC X(24).
            03  H02V03-HKNJA PIC X(16).
            03  H02V03-KIGONUM   PIC X(32).
            03  H02V03-BIRTH PIC X(22).
            03  H02V03-KANANAME  PIC X(30).
            03  H02V03-KANANAME2 PIC X(60).
            03  H02V03-NAME  PIC X(26).
            03  H02V03-NAME2 PIC X(52).
            03  H02V03-SEX   PIC X(2).
            03  H02V03-HONKZKKBN PIC X(16).
            03  H02V03-KOFYMD    PIC X(22).
            03  H02V03-KIKYY PIC X(4).
            03  H02V03-KIKMM PIC X(4).
            03  H02V03-KIKDD PIC X(4).
            03  H02V03-HOSPADRS  PIC X(80).
            03  H02V03-HOSPNAME  PIC X(80).
            03  H02V03-HOSPTEL   PIC X(15).
            03  H02V03-DRNAME    PIC X(26).
            03  H02V03-FTNJA2    PIC X(16).
            03  H02V03-FTNJA2N   PIC X(20).
            03  H02V03-JKYSNUM2  PIC X(14).
            03  H02V03-JKYSNUM2N PIC X(20).
            03  H02V03-JKYSNUM212    PIC X(24).
            03  H02V03-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V03-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V03-BIKOH
                    PIC X(86)   OCCURS  7 TIMES.
            03  H02V03-HKNINAME  PIC X(30).
            03  H02V03-SRYKAMEI  PIC X(10).
            03  H02V03-PAGE  PIC X(5).
            03  H02V03-FTNRATE   PIC X(3).
            03  H02V03-FTNWARI   PIC X(2).
            03  H02V03-KIGO  PIC X(32).
            03  H02V03-NUM   PIC X(32).
            03  H02V03-PREFNUM.
              04  H02V03-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V03-TENHYOKBN PIC X(2).
            03  H02V03-HOSPCD.
              04  H02V03-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V03-JKYSNUM15  PIC X(30).
            03  H02V03-JKYSNUM215 PIC X(30).
          02 H02V03-TYPE-B.
            03  H02V03-B-PTNUM   PIC X(20).
            03  H02V03-B-FTNJA   PIC X(16).
            03  H02V03-B-FTNJAN  PIC X(20).
            03  H02V03-B-JKYSNUM PIC X(14).
            03  H02V03-B-JKYSNUMN    PIC X(20).
            03  H02V03-B-JKYSNUM12   PIC X(24).
            03  H02V03-B-HKNJA   PIC X(16).
            03  H02V03-B-KIGONUM PIC X(32).
            03  H02V03-B-BIRTH   PIC X(22).
            03  H02V03-B-KANANAME    PIC X(30).
            03  H02V03-B-KANANAME2   PIC X(60).
            03  H02V03-B-NAME    PIC X(26).
            03  H02V03-B-NAME2   PIC X(52).
            03  H02V03-B-SEX PIC X(2).
            03  H02V03-B-HONKZKKBN   PIC X(16).
            03  H02V03-B-KOFYMD  PIC X(22).
            03  H02V03-B-KIKYY   PIC X(4).
            03  H02V03-B-KIKMM   PIC X(4).
            03  H02V03-B-KIKDD   PIC X(4).
            03  H02V03-B-HOSPADRS    PIC X(80).
            03  H02V03-B-HOSPNAME    PIC X(80).
            03  H02V03-B-HOSPTEL PIC X(15).
            03  H02V03-B-DRNAME  PIC X(26).
            03  H02V03-B-FTNJA2  PIC X(16).
            03  H02V03-B-FTNJA2N PIC X(20).
            03  H02V03-B-JKYSNUM2    PIC X(14).
            03  H02V03-B-JKYSNUM2N   PIC X(20).
            03  H02V03-B-JKYSNUM212  PIC X(24).
            03  H02V03-B-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V03-B-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V03-B-BIKOH
                    PIC X(86)   OCCURS  7 TIMES.
            03  H02V03-B-HKNINAME    PIC X(30).
            03  H02V03-B-SRYKAMEI    PIC X(10).
            03  H02V03-B-PAGE    PIC X(5).
            03  H02V03-B-FTNRATE PIC X(3).
            03  H02V03-B-FTNWARI PIC X(2).
            03  H02V03-B-KIGO    PIC X(32).
            03  H02V03-B-NUM PIC X(32).
            03  H02V03-B-PREFNUM.
              04  H02V03-B-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V03-B-TENHYOKBN   PIC X(2).
            03  H02V03-B-HOSPCD.
              04  H02V03-B-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V03-B-JKYSNUM15  PIC X(30).
            03  H02V03-B-JKYSNUM215 PIC X(30).
