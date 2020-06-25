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
      *    病名システム予約コードテーブル   COPY (CMBYOMEICD.INC)      *
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
      *    病名コードテーブル
       01  TBL-BYOMEICD-AREA.
               05  FILLER          PIC X(02)   VALUE   "（". 
               05  FILLER          PIC X(07)   VALUE   "ZZZ(101".
               05  FILLER          PIC X(02)   VALUE   "［". 
               05  FILLER          PIC X(07)   VALUE   "ZZZ(102".
               05  FILLER          PIC X(02)   VALUE   "【". 
               05  FILLER          PIC X(07)   VALUE   "ZZZ(103".
               05  FILLER          PIC X(02)   VALUE   "｛". 
               05  FILLER          PIC X(07)   VALUE   "ZZZ(104".
               05  FILLER          PIC X(02)   VALUE   "「". 
               05  FILLER          PIC X(07)   VALUE   "ZZZ(105".
               05  FILLER          PIC X(02)   VALUE   "『".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(106".
               05  FILLER          PIC X(02)   VALUE   "）".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(201".
               05  FILLER          PIC X(02)   VALUE   "］".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(202".
               05  FILLER          PIC X(02)   VALUE   "】".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(203".
               05  FILLER          PIC X(02)   VALUE   "｝".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(204".
               05  FILLER          PIC X(02)   VALUE   "」".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(205".
               05  FILLER          PIC X(02)   VALUE   "』".
               05  FILLER          PIC X(07)   VALUE   "ZZZ(206".
               05  FILLER          PIC X(02)   VALUE   "、".
               05  FILLER          PIC X(07)   VALUE   "ZZZC001".
               05  FILLER          PIC X(02)   VALUE   "，".
               05  FILLER          PIC X(07)   VALUE   "ZZZC002".
               05  FILLER          PIC X(02)   VALUE   "・".
               05  FILLER          PIC X(07)   VALUE   "ZZZC003".
      *
       01  TBL-BYOMEICD-AREA-R     REDEFINES   TBL-BYOMEICD-AREA.
           03  TBL-BYOMEICD-OCC    OCCURS      15
                                               INDEXED TBL-BYOMEICD-IDX.
               05  TBL-YYK-BYOMEI  PIC X(02).
               05  TBL-YYK-BYOMEICD
                                   PIC X(07).
      *    病名コードフラグ
       01  FLG-BYOMEICD-OK         PIC 9(01).
      *
       01  TBL-BYOMEICD-MAX        PIC 9(02)   VALUE   15.
      *
      *
