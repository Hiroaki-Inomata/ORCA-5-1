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
      *    ファイル情報診療情報システム用    CPORCSFILEMISSV.INC *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSFILEMISSVAREA.
      *    処理区分                              
           03  FLMISSV-MODE            PIC X(01).
      *    復帰コード     
           03  FLMISSV-RETURN          PIC S9(03).
      *    ＫＥＹ
           03  FLMISSV-TBL-KEY         PIC X(08).
      *
           03  FLMISSV-OCC             OCCURS  100.
      *        連番
               05  FLMISSV-RENNUM      PIC 9(04).
      *        グループ
               05  FLMISSV-TBL-GROUP   PIC X(14).
      *        処理内連番
               05  FLMISSV-SHORI-RENNUM
                                       PIC 9(04).
      *        シェルＩＤ
               05  FLMISSV-SHELLID     PIC X(20).
      *        診療年月
               05  FLMISSV-SRYYM       PIC X(06).
      *        診療年月日
               05  FLMISSV-SRYYMD      PIC X(08).
      *        件数
               05  FLMISSV-CNT         PIC 9(07).
      *        入外区分
               05  FLMISSV-NYUGAIKBN   PIC X(01).
      *        先保存場所
               05  FLMISSV-TO-FOLDER   PIC X(200).
      *        先ファイル名
               05  FLMISSV-TO-DATA     PIC X(200).
      *        コード区分
               05  FLMISSV-CODE-TYPE   PIC X(01).
      *        出力フラグ
               05  FLMISSV-PUTFLG      PIC X(1).
      *        タイトル
               05  FLMISSV-TITLE       PIC X(100).
      *        UUID
               05  FLMISSV-TBL-UUID    PIC X(36).
      *        作成年月日
               05  FLMISSV-CREYMD      PIC X(08).
      *        更新年月日
               05  FLMISSV-UPYMD       PIC X(08).
      *        更新時間
               05  FLMISSV-UPHMS       PIC X(06).
