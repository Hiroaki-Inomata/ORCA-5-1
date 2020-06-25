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
      *    orca41（データチェック）画面
      ******************************************************************
           COPY    "D01.INC".
           COPY    "D02.INC".
           COPY    "D03.INC".
           COPY    "D04.INC".
           COPY    "D05.INC".
           COPY    "D99.INC".
           COPY    "DERR.INC".
           COPY    "DID1.INC".
           COPY    "DID2.INC".
           COPY    "CPAPILSTV2.INC"    REPLACING
                                       //APILST-// BY //DAPI01-//.
