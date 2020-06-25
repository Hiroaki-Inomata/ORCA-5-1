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
      *****************************************************************
      *
      *     印刷用データ 名称領域      CPCOMMONPRT1.INC
      *            
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/02/17  レイアウト変更
      *****************************************************************
       01  HC011PARA.
           03  FILLER                  PIC  X(09) VALUE "/var/tmp/".
           03  HC011PARA-FORM-ID        PIC  X(05).
           03  FILLER                  PIC  X(01) VALUE ".".
      ***  03  HC011PARA-TERMID        PIC  X(16).
           03  HC011PARA-TERMID        PIC  X(32).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  HC011PARA-SYOHMS        PIC  9(08).
           03  FILLER                  PIC  X(01) VALUE ".".
           03  HC011PARA-CNT           PIC  9(06).
           03  FILLER                  PIC  X(04) VALUE ".dat".
