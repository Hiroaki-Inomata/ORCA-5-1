      * Project code name "ORCA"
      * ì˙à„ïWèÄÉåÉZÉvÉgÉ\ÉtÉgÅiJMA standard receipt softwareÅj
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
      *  ÉRÉsÅ[ãÂèCê≥óöó
      * Maj/Min/Rev  èCê≥é“       ì˙ït      ì‡óe
      * 01.00.01     @@@@-ñÂä‘    16/03/16  ÇgÇQÇWîNÇSåéâ¸ê≥ëŒâû
      * 02.00.01     @@@@-ñÂä‘    17/03/15  ïâíSäÑçáÇSåÖÇ÷èCê≥
      ********************************************************************
       01  H02V04.
          02 H02V04-TYPE-A.
            03  H02V04-PTNUM PIC X(20).
            03  H02V04-FTNJA PIC X(16).
            03  H02V04-FTNJAN    PIC X(20).
            03  H02V04-JKYSNUM   PIC X(14).
            03  H02V04-JKYSNUMN  PIC X(20).
            03  H02V04-JKYSNUM12 PIC X(24).
            03  H02V04-HKNJA PIC X(16).
            03  H02V04-KIGONUM   PIC X(32).
            03  H02V04-BIRTH PIC X(22).
            03  H02V04-KANANAME  PIC X(30).
            03  H02V04-KANANAME2 PIC X(60).
            03  H02V04-NAME  PIC X(26).
            03  H02V04-NAME2 PIC X(52).
            03  H02V04-SEX   PIC X(2).
            03  H02V04-HONKZKKBN PIC X(16).
            03  H02V04-KOFYMD    PIC X(22).
            03  H02V04-KIKYY PIC X(4).
            03  H02V04-KIKMM PIC X(4).
            03  H02V04-KIKDD PIC X(4).
            03  H02V04-HOSPADRS  PIC X(80).
            03  H02V04-HOSPNAME  PIC X(80).
            03  H02V04-HOSPTEL   PIC X(15).
            03  H02V04-DRNAME    PIC X(26).
            03  H02V04-FTNJA2    PIC X(16).
            03  H02V04-FTNJA2N   PIC X(20).
            03  H02V04-JKYSNUM2  PIC X(14).
            03  H02V04-JKYSNUM2N PIC X(20).
            03  H02V04-JKYSNUM212    PIC X(24).
            03  H02V04-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V04-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V04-BIKOH     PIC X(86)  OCCURS 5 TIMES.
            03  H02V04-BIKOH2    PIC X(40)  OCCURS 2 TIMES.
            03  H02V04-HKNINAME  PIC X(30).
            03  H02V04-SRYKAMEI  PIC X(10).
            03  H02V04-PAGE  PIC X(5).
            03  H02V04-FTNRATE   PIC X(3).
            03  H02V04-FTNWARI   PIC X(2).
            03  H02V04-KIGO  PIC X(32).
            03  H02V04-NUM   PIC X(32).
            03  H02V04-PREFNUM.
              04  H02V04-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V04-TENHYOKBN PIC X(2).
            03  H02V04-HOSPCD.
              04  H02V04-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V04-JKYSNUM15  PIC X(30).
            03  H02V04-JKYSNUM215 PIC X(30).
            03  H02V04-CHK1       PIC X(2).
            03  H02V04-CHK2       PIC X(2).
          02 H02V04-TYPE-B.
            03  H02V04-B-PTNUM   PIC X(20).
            03  H02V04-B-FTNJA   PIC X(16).
            03  H02V04-B-FTNJAN  PIC X(20).
            03  H02V04-B-JKYSNUM PIC X(14).
            03  H02V04-B-JKYSNUMN    PIC X(20).
            03  H02V04-B-JKYSNUM12   PIC X(24).
            03  H02V04-B-HKNJA   PIC X(16).
            03  H02V04-B-KIGONUM PIC X(32).
            03  H02V04-B-BIRTH   PIC X(22).
            03  H02V04-B-KANANAME    PIC X(30).
            03  H02V04-B-KANANAME2   PIC X(60).
            03  H02V04-B-NAME    PIC X(26).
            03  H02V04-B-NAME2   PIC X(52).
            03  H02V04-B-SEX PIC X(2).
            03  H02V04-B-HONKZKKBN   PIC X(16).
            03  H02V04-B-KOFYMD  PIC X(22).
            03  H02V04-B-KIKYY   PIC X(4).
            03  H02V04-B-KIKMM   PIC X(4).
            03  H02V04-B-KIKDD   PIC X(4).
            03  H02V04-B-HOSPADRS    PIC X(80).
            03  H02V04-B-HOSPNAME    PIC X(80).
            03  H02V04-B-HOSPTEL PIC X(15).
            03  H02V04-B-DRNAME  PIC X(26).
            03  H02V04-B-FTNJA2  PIC X(16).
            03  H02V04-B-FTNJA2N PIC X(20).
            03  H02V04-B-JKYSNUM2    PIC X(14).
            03  H02V04-B-JKYSNUM2N   PIC X(20).
            03  H02V04-B-JKYSNUM212  PIC X(24).
            03  H02V04-B-HENKOU
                    PIC X(2)    OCCURS  19 TIMES.
            03  H02V04-B-NAIYO
                    PIC X(82)   OCCURS  19 TIMES.
            03  H02V04-B-BIKOH   PIC X(86)  OCCURS 5 TIMES.
            03  H02V04-B-BIKOH2  PIC X(40)  OCCURS 2 TIMES.
            03  H02V04-B-HKNINAME    PIC X(30).
            03  H02V04-B-SRYKAMEI    PIC X(10).
            03  H02V04-B-PAGE    PIC X(5).
            03  H02V04-B-FTNRATE PIC X(3).
            03  H02V04-B-FTNWARI PIC X(2).
            03  H02V04-B-KIGO    PIC X(32).
            03  H02V04-B-NUM PIC X(32).
            03  H02V04-B-PREFNUM.
              04  H02V04-B-PREFNUM-S
                      PIC X(2)    OCCURS  2 TIMES.
            03  H02V04-B-TENHYOKBN   PIC X(2).
            03  H02V04-B-HOSPCD.
              04  H02V04-B-HOSPCD-S
                      PIC X(2)    OCCURS  7 TIMES.
            03  H02V04-B-JKYSNUM15  PIC X(30).
            03  H02V04-B-JKYSNUM215 PIC X(30).
            03  H02V04-B-CHK1     PIC X(2).
            03  H02V04-B-CHK2     PIC X(2).
          02  H02V04-FTNRATE2    PIC X(4).
          02  H02V04-B-FTNRATE2  PIC X(4).
