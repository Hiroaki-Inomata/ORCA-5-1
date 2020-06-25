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
        01  HCM38.
            02  HCM38-KIKAN PIC X(40).
            02  HCM38-MES   PIC X(50).
            02  HCM38-SAKUSEIBI PIC X(22).
            02  HCM38-PAGE  PIC X(6).
            02  HCM38-CD
                    PIC X(5)    OCCURS  30 TIMES.
            02  HCM38-PTNUM
                    PIC X(10)   OCCURS  30 TIMES.
            02  HCM38-PTNUMUP
                    PIC X(10)   OCCURS  30 TIMES.
            02  HCM38-PTNUMDN
                    PIC X(10)   OCCURS  30 TIMES.
            02  HCM38-NAME
                    PIC X(30)   OCCURS  30 TIMES.
            02  HCM38-NAME2
                    PIC X(80)   OCCURS  30 TIMES.
            02  HCM38-MISYU
                    PIC X(11)   OCCURS  30 TIMES.
            02  HCM38-LASTNYKNYMD
                    PIC X(9)    OCCURS  30 TIMES.
            02  HCM38-LASTYMD
                    PIC X(9)    OCCURS  30 TIMES.
            02  HCM38-SRYKA
                    PIC X(8)    OCCURS  30 TIMES.
            02  HCM38-HOME-TEL1
                    PIC X(15)   OCCURS  30 TIMES.
            02  HCM38-BIKO
                    PIC X(26)   OCCURS  30 TIMES.
            02  HCM38-MARK
                    PIC X(1)    OCCURS  30 TIMES.
            02  HCM38-TITLE2    PIC X(10).
            02  HCM38-GMISYU    PIC X(11).
            02  HCM38-TAIIN PIC X(10).
