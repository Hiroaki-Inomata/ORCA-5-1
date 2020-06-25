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
        02  O05.
              03  O05-B12-STATE   PIC S9(9)   BINARY.
              03  O05-KBT.
                04  O05-VALUE     PIC X(22).
              03  O05-KIKAN.
                04  O05-VALUE     PIC X(20).
              03  O05-LBLMSG-STYLE PIC X(20).
              03  O05-LBLMSG      PIC X(50).
