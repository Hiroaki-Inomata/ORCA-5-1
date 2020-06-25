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
      *     定期請求ファイル 名称領域      CPCOMMONDAT5.INC
      *            
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *****************************************************************
       01  TEIKI01PARA-AREA.
         03  TEIKI01PARA.
           05  TEIKI01PARA-OLD.
               07  FILLER                  PIC  X(05) VALUE "/tmp/".
               07  TEIKI01PARA-BASENAME.
                   09  TEIKI01PARA-HOSPNUM PIC  9(02).
                   09  TEIKI01PARA-FILE-ID PIC  X(08).
                   09  TEIKI01PARA-FILE-NO REDEFINES TEIKI01PARA-FILE-ID
                                           PIC  9(08) .
                   09  TEIKI01PARA-TERMID  PIC  X(32).
               07  FILLER                  PIC  X(977) VALUE SPACE.
           05  TEIKI01PARA-FULLNAME-R REDEFINES TEIKI01PARA-OLD.
               07  TEIKI01PARA-FULLNAME    PIC  X(1024).
