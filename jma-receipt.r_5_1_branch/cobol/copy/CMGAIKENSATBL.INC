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
      *----------------------------------------------------------------*
      *Ｈ２０年４月改正から
      *   外来迅速検体検査算定検査コードテーブル COPY (CMGAIKENSATBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  12/08/03  改正対応追加
      * 04.07.00     NACL-多々納  14/11/21  （尿）削除対応
      *****************************************************************
      *
       01  TBL-KENSA-TAI-AREA.
           03  FILLER          PIC X(09)  VALUE    "160000310".
           03  FILLER          PIC X(09)  VALUE    "160005010".
           03  FILLER          PIC X(09)  VALUE    "160006810".
           03  FILLER          PIC X(09)  VALUE    "160007610".
           03  FILLER          PIC X(09)  VALUE    "160008010".
           03  FILLER          PIC X(09)  VALUE    "160010010".
           03  FILLER          PIC X(09)  VALUE    "160012010".
           03  FILLER          PIC X(09)  VALUE    "160014510".
      *H24.8
           03  FILLER          PIC X(09)  VALUE    "160191810".
           03  FILLER          PIC X(09)  VALUE    "160191910".
      *H24.8
           03  FILLER          PIC X(09)  VALUE    "160114010".
           03  FILLER          PIC X(09)  VALUE    "160017010".
           03  FILLER          PIC X(09)  VALUE    "160017410".
           03  FILLER          PIC X(09)  VALUE    "160018910".
           03  FILLER          PIC X(09)  VALUE    "160019010".
      *****03  FILLER          PIC X(09)  VALUE    "160131950". 
           03  FILLER          PIC X(09)  VALUE    "160019210".
      *****03  FILLER          PIC X(09)  VALUE    "160132150".
           03  FILLER          PIC X(09)  VALUE    "160019310".
      *****03  FILLER          PIC X(09)  VALUE    "160132250".
           03  FILLER          PIC X(09)  VALUE    "160020010".
           03  FILLER          PIC X(09)  VALUE    "160020210".
           03  FILLER          PIC X(09)  VALUE    "160020410".
           03  FILLER          PIC X(09)  VALUE    "160020910".
           03  FILLER          PIC X(09)  VALUE    "160021110".
      *****03  FILLER          PIC X(09)  VALUE    "160132450".
           03  FILLER          PIC X(09)  VALUE    "160021410".
      *****03  FILLER          PIC X(09)  VALUE    "160132650".
           03  FILLER          PIC X(09)  VALUE    "160021510".
      *****03  FILLER          PIC X(09)  VALUE    "160132750".
           03  FILLER          PIC X(09)  VALUE    "160019410".
           03  FILLER          PIC X(09)  VALUE    "160137950".
           03  FILLER          PIC X(09)  VALUE    "160138150".
           03  FILLER          PIC X(09)  VALUE    "160138750".
           03  FILLER          PIC X(09)  VALUE    "160019510".
           03  FILLER          PIC X(09)  VALUE    "160020610".
           03  FILLER          PIC X(09)  VALUE    "160023410".
           03  FILLER          PIC X(09)  VALUE    "160022410".
           03  FILLER          PIC X(09)  VALUE    "160022510".
           03  FILLER          PIC X(09)  VALUE    "160022610".
           03  FILLER          PIC X(09)  VALUE    "160167250".
           03  FILLER          PIC X(09)  VALUE    "160151050".
           03  FILLER          PIC X(09)  VALUE    "160031710".
           03  FILLER          PIC X(09)  VALUE    "160033210".
           03  FILLER          PIC X(09)  VALUE    "160033310".
           03  FILLER          PIC X(09)  VALUE    "160036510".
           03  FILLER          PIC X(09)  VALUE    "160036710".
           03  FILLER          PIC X(09)  VALUE    "160037510".
           03  FILLER          PIC X(09)  VALUE    "160037210".
           03  FILLER          PIC X(09)  VALUE    "160054710".
           03  FILLER          PIC X(09)  VALUE    "160057710".
      *
       01  TBL-KENSA-TAI-AREA-R    REDEFINES  TBL-KENSA-TAI-AREA.
      *    03  TBL-KENSA-TAI-OCC        OCCURS  49  INDEXED  TAI-IDX.
      *****03  TBL-KENSA-TAI-OCC        OCCURS  51  INDEXED  TAI-IDX.
           03  TBL-KENSA-TAI-OCC        OCCURS  45  INDEXED  TAI-IDX.
               05  TBL-TAI-SRYCD        PIC X(09).
       01  KEN-TAI-MAX                  PIC 9(04)   VALUE   45.
      *
