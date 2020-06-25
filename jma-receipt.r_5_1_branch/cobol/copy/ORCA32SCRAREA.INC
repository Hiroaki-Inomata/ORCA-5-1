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
      *    orca32（入院会計照会）画面
      ******************************************************************
           COPY    "I41.INC".
           COPY    "I42.INC".
           COPY    "I44.INC".
           COPY    "I47.INC".
           COPY    "I48.INC".
           COPY    "I49.INC".
           COPY    "I411.INC".
           COPY    "I4ERR.INC".
           COPY    "I4ID1.INC".
           COPY    "I4ID2.INC".
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //I4API01-//.
