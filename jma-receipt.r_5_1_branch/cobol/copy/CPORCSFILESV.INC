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
      *    ファイル情報ＤＢ制御サブパラメタ     CPORCSFILESV.INC *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSFILESVAREA.
      *    処理区分                              
           03  SFILESV-MODE            PIC X(01).
      *    復帰コード     
           03  SFILESV-RETURN          PIC S9(03).
      *    ＫＥＹ
           03  SFILESV-TBL-KEY         PIC X(08).
      *
           03  SFILESV-OCC             OCCURS  100.
      *        シェルＩＤ
               05  SFILESV-SHELLID     PIC X(20).
      *        処理内連番
               05  SFILESV-SHORI-RENNUM
                                       PIC 9(03).
      *        連番
               05  SFILESV-RENNUM      PIC 9(03).
      *        診療年月
               05  SFILESV-SRYYM       PIC X(06).
      *        請求年月日
               05  SFILESV-SKYYMD      PIC X(08).
      *        タイトル
               05  SFILESV-TITLE       PIC X(100).
      *        審査支払機関
               05  SFILESV-TEISYUTUSAKI
                                       PIC X(01).
      *        入外区分
               05  SFILESV-NYUGAIKBN   PIC X(01).
      *        件数
               05  SFILESV-CNT-ALL     PIC 9(07).
      *        元格納場所
               05  SFILESV-FOR-FOLDER  PIC X(200).
      *        元ファイル名
               05  SFILESV-FOR-DATA    PIC X(200).
      *        先保存場所
               05  SFILESV-TO-FOLDER   PIC X(200).
      *        先ファイル名
               05  SFILESV-TO-DATA     PIC X(200).
      *        コード区分
               05  SFILESV-CODE-TYPE   PIC X(01).
      *        ＣＳＶ用ＤＢ有無区分
               05  SFILESV-DATA-TYPE   PIC X(01).
      *        エラーコード
               05  SFILESV-ERR-CODE    PIC X(01).
      *        エラーメッセージ
               05  SFILESV-ERR-MSG     PIC X(200).
      *        処理日
               05  SFILESV-SYOYMD      PIC X(08).
      *        作成日
               05  SFILESV-CREYMD      PIC X(08).
      *        作成時間
               05  SFILESV-CREHMS      PIC X(06).
