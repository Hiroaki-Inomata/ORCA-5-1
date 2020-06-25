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
      *----------------------------------------------------------*
      *    入院請求書まとめサブパラメタ    COPY (CPORCHCN03S02.INC)
      *                                         (ORCHCN03S02 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCHCN03S02AREA.
           03  ORCHCN03S02-IDX                PIC 9(5).
           03  ORCHCN03S02-MYMISYU            PIC S9(10).
           03  ORCHCN03S02-FUKUSU-COMBI       PIC 9(01).
           03  ORCHCN03S02-PRTFLG             PIC 9(01).
           03  ORCHCN03S02-GRP-CNT            PIC 9(03).
           03  ORCHCN03S02-GRP-DENPNUM        PIC 9(07)
                                              OCCURS   10.
           03  ORCHCN03S02-CHKPRT             PIC 9(01).
           03  ORCHCN03S02-PTINFREF           PIC X(01).
           03  ORCHCN03S02-SKYPRT             PIC X(01).
           03  ORCHCN03S02-SRYPRT             PIC X(01).
