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
      * 統計ＣＳＶ制御サブパラメタ        CPORCSTOUKEICSV.INC    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSTOUKEICSVAREA.
      *    処理区分                              
           03  STOUKEICSV-MODE         PIC X(03).
      *    復帰コード     
           03  STOUKEICSV-RETURN       PIC S9(03).
      *    連番
           03  STOUKEICSV-RENNUM       PIC 9(04).
      *    ＫＥＹ
           03  STOUKEICSV-TBL-KEY      PIC X(08).
      *    グループ
           03  STOUKEICSV-TBL-GROUP    PIC X(14).
      *    診療年月
           03  STOUKEICSV-SRYYM        PIC X(06).
      *    請求年月日
           03  STOUKEICSV-SRYYMD       PIC X(08).
      *    シェルＩＤ
           03  STOUKEICSV-SHELLID      PIC X(20).
      *    患者ＩＤ
           03  STOUKEICSV-PTID         PIC 9(10).
      *    処理内連番
           03  STOUKEICSV-SHORI-RENNUM PIC 9(04).
      *    印刷済フラグ	
           03  STOUKEICSV-PUTFLG       PIC X(01).
      *    入外区分
           03  STOUKEICSV-NYUGAIKBN    PIC X(01).
      *    先格納場所
           03  STOUKEICSV-TO-FOLDER    PIC X(200).
      *    先ファイル名
           03  STOUKEICSV-TO-DATA      PIC X(200).
      *    コード区分（空白、１：Ｓ−ｊｉｓ ２：ＥＵＣ）
           03  STOUKEICSV-CODE-TYPE    PIC X(01).
      *    帳票タイトル
           03  STOUKEICSV-TITLE        PIC X(100).
      *    帳票データ
           03  STOUKEICSV-CSVDATA      PIC X(20000).
