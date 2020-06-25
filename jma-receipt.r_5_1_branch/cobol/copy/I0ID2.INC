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
        02  I0ID2.
      *     02  I0ID2-FIXED1.
              03  I0ID2-ID2CODE.
                04  I0ID2-VALUE PIC X(10).
              03  I0ID2-ID2MSG.
                04  I0ID2-VALUE PIC X(70).
              03  I0ID2-IDOYMD.
                04  I0ID2-VALUE PIC X(10).
              03  I0ID2-NYUINKA.
                04  I0ID2-VALUE PIC X(24).
              03  I0ID2-BRM.
                04  I0ID2-VALUE PIC X(6).
              03  I0ID2-BTU.
                04  I0ID2-VALUE PIC X(20).
              03  I0ID2-HKNCOMBI.
                04  I0ID2-VALUE PIC X(46).
              03  I0ID2-TOKNYUIN.
                04  I0ID2-VALUE PIC X(48).
