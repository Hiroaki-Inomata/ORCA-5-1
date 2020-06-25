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
      ******************************************************************
      *    ＡＰＩリクエスト患者情報一覧取得（氏名検索）(xml2)
      *                                 COPY (CPPATIENTLST3V2SEQ.INC)
      ******************************************************************
        01  XML-PATIENTLST3REQ.
            03  PATLSTREQ-CONTEXT          PIC S9(9)   BINARY.
            03  PATLSTREQ-OBJECT           PIC X(36).
            03  PATLSTREQ-MODE             PIC S9(9)   BINARY.
            03  PATLSTREQ-RECORDNAME       PIC X(128).
      *     患者情報
            03  PATLSTREQ-PTINF-INFO.
      *         患者氏名
                05  PATLSTREQ-NAME             PIC X(100).
      *         開始生年月日
                05  PATLSTREQ-BIRTHDAY-START   PIC X(10).
      *         終了生年月日
                05  PATLSTREQ-BIRTHDAY-END     PIC X(10).
      *         性別
                05  PATLSTREQ-SEX              PIC X(01).
      *         入外区分（0:入院・外来、1:入院、2：外来）
                05  PATLSTREQ-INOUTKBN         PIC X(01).
      *
