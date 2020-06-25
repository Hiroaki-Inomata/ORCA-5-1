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
      ************************************************************
      *     API通信用共通エリア         (CPAPIPTAV2.INC)         *
      *                （患者情報取得　xml2)
      ************************************************************
        02  APIPTA-REC.
            03  APIPTA-REQUEST.
              04  APIPTA-ARGUMENT.
                05  APIPTA-PTNUM       PIC X(20).
                05  APIPTA-FORMAT      PIC X(20).
              04  APIPTA-METHOD        PIC X(100).
              04  APIPTA-CONTENT-TYPE  PIC X(100).
              04  APIPTA-BODY          PIC X(36).
              04  APIPTA-HTTPSTATUS    PIC S9(9)   BINARY.
