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
      *    orca36（入院患者照会）画面
      ******************************************************************
           COPY    "I20.INC".
           COPY    "I21.INC".
           COPY    "I22.INC".
           COPY    "I23.INC".
           COPY    "I2ERR.INC".
           COPY    "I2ID1.INC".
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //I2API01-//.
