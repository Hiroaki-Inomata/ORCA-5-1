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
      * お薬手帳データ制御サブパラメタ     CPORCSMNOTE.INC    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSMNOTEAREA.
      *    処理区分                              
           03  SMNOTE-MODE         PIC X(03).
      *    復帰コード     
           03  SMNOTE-RETURN       PIC S9(03).
      *    連番
           03  SMNOTE-RENNUM       PIC 9(04).
      *    ＫＥＹ
           03  SMNOTE-TBL-KEY      PIC X(08).
      *    グループ
           03  SMNOTE-TBL-GROUP    PIC X(14).
      *    診療年月
           03  SMNOTE-SRYYM        PIC X(06).
      *    請求年月日
           03  SMNOTE-SRYYMD       PIC X(08).
      *    シェルＩＤ
           03  SMNOTE-SHELLID      PIC X(20).
      *    患者ＩＤ
           03  SMNOTE-PTID         PIC 9(10).
      *    処理内連番
           03  SMNOTE-SHORI-RENNUM PIC 9(04).
      *    印刷済フラグ	
           03  SMNOTE-PUTFLG       PIC X(01).
      *    入外区分
           03  SMNOTE-NYUGAIKBN    PIC X(01).
      *    先格納場所
           03  SMNOTE-TO-FOLDER    PIC X(200).
      *    先ファイル名
           03  SMNOTE-TO-DATA      PIC X(200).
      *    コード区分（空白、１：Ｓ−ｊｉｓ ２：ＥＵＣ）
           03  SMNOTE-CODE-TYPE    PIC X(01).
      *    帳票タイトル
           03  SMNOTE-TITLE        PIC X(100).
      *    ＵＵＩＤ
           03  SMNOTE-TBL-UUID     PIC X(36).
      *    業務コード
           03  SMNOTE-GYOUMU-CD    PIC 9(03).
      *    出力場所コード
      *    01:診療行為（請求確認）
      *    02:診療行為（中途終了）
      *    03:診療行為（前回処方）
           03  SMNOTE-LOCATION-CD  PIC X(02).
      *    保存用CSVファイル名(PATH付)
           03  SMNOTE-CSVFILE      PIC X(255).
      *    ドクターコード
           03  SMNOTE-DRCD         PIC X(05).
      *    帳票データ
           03  SMNOTE-CSVDATA      PIC X(20000).
      *    データバージョン
           03  SMNOTE-DATA-VERSION PIC X(1).
