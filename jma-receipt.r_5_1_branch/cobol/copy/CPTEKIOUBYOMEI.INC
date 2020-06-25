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
      *  適応病名マスタ       (CPTEKIOUBYOMEI.INC)
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  TEKIOUBYOMEI-REC.
      *
           03  TEKBYO-KEY.
      *チェック区分
               05  TEKBYO-CHKKBN          PIC X(01).
      *診療行為コード
               05  TEKBYO-SRYCD           PIC X(09).
      *連番号
               05  TEKBYO-RENNUM          PIC 9(04).
      *適応病名
           03  TEKBYO-BYOMEI              PIC X(400).
      *病名コード数
           03  TEKBYO-BYOMEICDSU          PIC 9(02).
      *病名コード
           03  TEKBYO-BYOMEICD-G.
               05  TEKBYO-BYOMEICD-1      PIC X(07).
               05  TEKBYO-BYOMEICD-2      PIC X(07).
               05  TEKBYO-BYOMEICD-3      PIC X(07).
               05  TEKBYO-BYOMEICD-4      PIC X(07).
               05  TEKBYO-BYOMEICD-5      PIC X(07).
               05  TEKBYO-BYOMEICD-6      PIC X(07).
               05  TEKBYO-BYOMEICD-7      PIC X(07).
               05  TEKBYO-BYOMEICD-8      PIC X(07).
               05  TEKBYO-BYOMEICD-9      PIC X(07).
               05  TEKBYO-BYOMEICD-10     PIC X(07).
               05  TEKBYO-BYOMEICD-11     PIC X(07).
               05  TEKBYO-BYOMEICD-12     PIC X(07).
               05  TEKBYO-BYOMEICD-13     PIC X(07).
               05  TEKBYO-BYOMEICD-14     PIC X(07).
               05  TEKBYO-BYOMEICD-15     PIC X(07).
               05  TEKBYO-BYOMEICD-16     PIC X(07).
               05  TEKBYO-BYOMEICD-17     PIC X(07).
               05  TEKBYO-BYOMEICD-18     PIC X(07).
               05  TEKBYO-BYOMEICD-19     PIC X(07).
               05  TEKBYO-BYOMEICD-20     PIC X(07).
               05  TEKBYO-BYOMEICD-21     PIC X(07).
      *
           03  TEKBYO-BYOMEICD-R          REDEFINES   TEKBYO-BYOMEICD-G.
               05  TEKBYO-BYOMEICD        PIC X(07)
                                          OCCURS   21.
      *病名編集フラグ
           03  TEKBYO-BYOMEIHENFLG        PIC X(01).
      *基本病名コード
           03  TEKBYO-KHNBYOMEICD         PIC X(07).
      *基本部位コード１
           03  TEKBYO-KHNBUICD-1          PIC X(07).
      *基本部位コード２
           03  TEKBYO-KHNBUICD-2          PIC X(07).
      *基本部位コード３
           03  TEKBYO-KHNBUICD-3          PIC X(07).
      *端末ＩＤ
           03  TEKBYO-TERMID              PIC X(32).
      *オペーレータＩＤ
           03  TEKBYO-OPID                PIC X(16).
      *作成年月日
           03  TEKBYO-CREYMD              PIC X(08).
      *更新年月日
           03  TEKBYO-UPYMD               PIC X(08).
      *更新時間
           03  TEKBYO-UPHMS               PIC X(06).
      *
      *検索用項目
           03  TEKBYO-SELNAME             PIC  X(200).
           03  TEKBYO-SELYUKOSTYMD        PIC  X(8).
           03  TEKBYO-SELYUKOEDYMD        PIC  X(8).
