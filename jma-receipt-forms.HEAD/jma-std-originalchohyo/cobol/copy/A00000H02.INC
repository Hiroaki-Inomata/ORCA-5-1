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
         03 HCM19-TYPE-A.
            05  HCM19-PTNUM PIC X(20).
            05  HCM19-FTNJA PIC X(16).
            05  HCM19-FTNJAN    PIC X(20).
            05  HCM19-JKYSNUM   PIC X(14).
            05  HCM19-JKYSNUMN  PIC X(20).
            05  HCM19-HKNJA PIC X(16).
            05  HCM19-KIGONUM   PIC X(32).
            05  HCM19-KIGO      PIC X(32).
            05  HCM19-NUM       PIC X(32).
            05  HCM19-MIDASHI   PIC X(20).
            05  HCM19-MIDASHI2   PIC X(88).
            05  HCM19-BIRTH PIC X(22).
            05  HCM19-KANANAME  PIC X(30).
            05  HCM19-KANANAME2 PIC X(60).
            05  HCM19-NAME  PIC X(26).
            05  HCM19-NAME2 PIC X(52).
            05  HCM19-SEX   PIC X(2).
            05  HCM19-HONKZKKBN PIC X(16).
	    05  HCM19-FTNWARI   PIC X(04).
            05  HCM19-KOFYMD    PIC X(22).
            05  HCM19-KIKYY PIC X(4).
            05  HCM19-KIKMM PIC X(4).
            05  HCM19-KIKDD PIC X(4).
            05  HCM19-HOSPADRS  PIC X(72).
            05  HCM19-HOSPNAME  PIC X(72).
            05  HCM19-HOSPTEL   PIC X(15).
            05  HCM19-DRNAME    PIC X(26).
            05  HCM19-FTNJA2    PIC X(16).
            05  HCM19-FTNJA2N   PIC X(20).
            05  HCM19-JKYSNUM2  PIC X(14).
            05  HCM19-JKYSNUM2N PIC X(20).
            05  HCM19-NAIYO 
	            PIC X(86)   OCCURS  20 TIMES.
            05  HCM19-MENJYO    PIC X(6).
            05  HCM19-BIKOH 
	            PIC X(86)   OCCURS  9 TIMES.
            05  HCM19-HENKOUKA  PIC X(16).
	    05  HCM19-HKNINAME  PIC X(16).
         03 HCM19-TYPE-B.
            05  HCM19-B-PTNUM PIC X(20).
            05  HCM19-B-FTNJA PIC X(16).
            05  HCM19-B-FTNJAN    PIC X(20).
            05  HCM19-B-JKYSNUM   PIC X(14).
            05  HCM19-B-JKYSNUMN  PIC X(20).
            05  HCM19-B-HKNJA PIC X(16).
            05  HCM19-B-KIGONUM   PIC X(32).
            05  HCM19-B-KIGO      PIC X(32).
            05  HCM19-B-NUM       PIC X(32).
            05  HCM19-B-MIDASHI   PIC X(20).
            05  HCM19-B-MIDASHI2   PIC X(88).
            05  HCM19-B-BIRTH PIC X(22).
            05  HCM19-B-KANANAME  PIC X(30).
            05  HCM19-B-KANANAME2 PIC X(60).
            05  HCM19-B-NAME  PIC X(26).
            05  HCM19-B-NAME2 PIC X(52).
            05  HCM19-B-SEX   PIC X(2).
            05  HCM19-B-HONKZKKBN PIC X(16).
	    05  HCM19-B-FTNWARI   PIC X(04).
            05  HCM19-B-KOFYMD    PIC X(22).
            05  HCM19-B-KIKYY PIC X(4).
            05  HCM19-B-KIKMM PIC X(4).
            05  HCM19-B-KIKDD PIC X(4).
            05  HCM19-B-HOSPADRS  PIC X(72).
            05  HCM19-B-HOSPNAME  PIC X(72).
            05  HCM19-B-HOSPTEL   PIC X(15).
            05  HCM19-B-DRNAME    PIC X(26).
            05  HCM19-B-FTNJA2    PIC X(16).
            05  HCM19-B-FTNJA2N   PIC X(20).
            05  HCM19-B-JKYSNUM2  PIC X(14).
            05  HCM19-B-JKYSNUM2N PIC X(20).
            05  HCM19-B-NAIYO 
	            PIC X(86)   OCCURS  20 TIMES.
            05  HCM19-B-MENJYO    PIC X(6).
            05  HCM19-B-BIKOH 
	            PIC X(86)   OCCURS  9 TIMES.
            05  HCM19-B-HENKOUKA  PIC X(16).
	    05  HCM19-B-HKNINAME  PIC X(16).

