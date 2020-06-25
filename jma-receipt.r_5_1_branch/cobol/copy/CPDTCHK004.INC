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
      *    データチェック中間ファイル     COPY (CPDTCHK004.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  DTCHK004-REC.
      *
           03  DTCHK004-KEY.
               05  DTCHK004-ERRSBT         PIC X(01).
               05  DTCHK004-SRTKEY         PIC X(100).
               05  DTCHK004-SRTKEY-R1      REDEFINES   DTCHK004-SRTKEY.
                   07  DTCHK004-R1-PTNUM       PIC X(20).
                   07  DTCHK004-R1-PTID        PIC 9(10).
                   07  DTCHK004-R1-SRTKBN      PIC X(01).
                   07  DTCHK004-R1-ERRCD       PIC X(06).
                   07  DTCHK004-R1-ERRCD-SEQ   PIC 9(05).
                   07  DTCHK004-R1-DUMMY       PIC X(58).
               05  DTCHK004-SRTKEY-R2      REDEFINES   DTCHK004-SRTKEY.
                   07  DTCHK004-R2-KANANAME    PIC X(50).
                   07  DTCHK004-R2-PTNUM       PIC X(20).
                   07  DTCHK004-R2-PTID        PIC 9(10).
                   07  DTCHK004-R2-SRTKBN      PIC X(01).
                   07  DTCHK004-R2-ERRCD       PIC X(06).
                   07  DTCHK004-R2-ERRCD-SEQ   PIC 9(05).
                   07  DTCHK004-R2-DUMMY       PIC X(08).
               05  DTCHK004-SRTKEY-R3      REDEFINES   DTCHK004-SRTKEY.
                   07  DTCHK004-R3-SRTKBN      PIC X(01).
                   07  DTCHK004-R3-PTNUM       PIC X(20).
                   07  DTCHK004-R3-PTID        PIC 9(10).
                   07  DTCHK004-R3-ERRCD       PIC X(06).
                   07  DTCHK004-R3-ERRCD-SEQ   PIC 9(05).
                   07  DTCHK004-R3-DUMMY       PIC X(58).
               05  DTCHK004-SRTKEY-R4      REDEFINES   DTCHK004-SRTKEY.
                   07  DTCHK004-R4-SRTKBN      PIC X(01).
                   07  DTCHK004-R4-KANANAME    PIC X(50).
                   07  DTCHK004-R4-PTNUM       PIC X(20).
                   07  DTCHK004-R4-PTID        PIC 9(10).
                   07  DTCHK004-R4-ERRCD       PIC X(06).
                   07  DTCHK004-R4-ERRCD-SEQ   PIC 9(05).
                   07  DTCHK004-R4-DUMMY       PIC X(08).
      *
      *患者番号
           03  DTCHK004-PTNUM         PIC X(20).
      *
      *患者ＩＤ
           03  DTCHK004-PTID          PIC 9(10).
      *氏名
           03  DTCHK004-NAME          PIC X(100).
      *カナ氏名
           03  DTCHK004-KANANAME      PIC X(100).
      *
      *複数保険区分
           03  DTCHK004-MULHKNKBN     PIC  X(01).
      *
      *保険区分
           03  DTCHK004-HKNKBN        PIC  X(01).
      *
      *他保険エラー区分
           03  DTCHK004-HKNOTHERS     PIC  X(01).
      *
      *生年月日
           03  DTCHK004-BIRTHDAY      PIC  X(08).
      *
      *性別
           03  DTCHK004-SEX           PIC  X(01).
      *
      *エラーコード
           03  DTCHK004-ERRCD         PIC  X(06).
      *
      *同一エラーコード内連番
           03  DTCHK004-ERRCD-SEQ     PIC 9(05).
      *
      *チェック区分
           03  DTCHK004-CHKKBN        PIC X(01).
      *
      *診療コード
           03  DTCHK004-SRYCD         PIC X(09).
      *
      *診療科
           03  DTCHK004-SRYKA         PIC X(02).
      *
      *病名
           03  DTCHK004-BYOMEI        PIC X(160).
      *
      *適用コード
           03  DTCHK004-TEKCD         PIC X(09).
      *
      *適応病名
           03  DTCHK004-TEKBYOMEI     PIC X(160).
      *
      *条件区分
           03  DTCHK004-TEKCOMPKBN    PIC X(1).
      *
      *単位コード
           03  DTCHK004-TANICD        PIC 9(3).
      *
      *単位名称
           03  DTCHK004-TANINAME      PIC X(12).
      *
      *初回番号
           03  DTCHK004-SHONUM        PIC 9(3).
      *
      *診療開始日
           03  DTCHK004-SRYSTYMD      PIC X(8).
      *
      *診療終了日
           03  DTCHK004-SRYEDYMD      PIC X(8).
      *
      *上限回数
           03  DTCHK004-LIMIT         PIC 9(5).
      *
      *月数
           03  DTCHK004-TSUKISU       PIC 9(2).
      *
      *日付
           03  DTCHK004-DAY-G.
               05  DTCHK004-DAY       PIC 9(01)
                                      OCCURS   31.
      *チェック最終日
           03  DTCHK004-LASTYMD       PIC X(08).
      *
      *エラーメッセージ
           03  DTCHK004-ERRMSG        PIC X(500).
