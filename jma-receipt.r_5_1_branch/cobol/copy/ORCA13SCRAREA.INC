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
      *    orca13（照会）画面
      ******************************************************************
           COPY    "Q01.INC".
           COPY    "Q02.INC".
           COPY    "Q02A.INC".
           COPY    "Q02B.INC".
           COPY    "Q03.INC".
           COPY    "Q04.INC".
           COPY    "Q05.INC".
           COPY    "Q96.INC".
           COPY    "Q97.INC".
           COPY    "Q98.INC".
           COPY    "Q99.INC".
           COPY    "Q100.INC".
           COPY    "QERR.INC".
           COPY    "QID1.INC".
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //QAPI01-//.
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //QAPI02-//.
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //QAPI03-//.
