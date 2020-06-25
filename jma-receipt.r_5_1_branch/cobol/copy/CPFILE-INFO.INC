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
      *    ファイル情報ＤＢ              (CPFILE-INFO.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  FILE-INFO-REC.
      *
          03  FILE-INFO-KEY.
      *
               05  FILE-INFO-HOSPNUM   PIC 9(2).
      *ＫＥＹ
               05  FILE-INFO-TBL-KEY   PIC X(08).
      *診療年月
               05  FILE-INFO-SRYYM     PIC X(06).
      *シェルＩＤ
               05  FILE-INFO-SHELLID   PIC X(20).
      *連番
               05  FILE-INFO-RENNUM    PIC 9(03).
      *処理内連番
               05  FILE-INFO-SHORI-RENNUM
                                       PIC 9(03).
      *請求年月日
           03  FILE-INFO-SKYYMD        PIC X(08).
      *タイトル
           03  FILE-INFO-TITLE         PIC X(100).
      *審査支払機関
           03  FILE-INFO-TEISYUTUSAKI  PIC X(01).
      *入外区分
           03  FILE-INFO-NYUGAIKBN     PIC X(01).
      *件数
           03  FILE-INFO-CNT-ALL       PIC 9(07).
      *件数（入院）
           03  FILE-INFO-CNT-NYUIN     PIC 9(07).
      *件数（入院外）
           03  FILE-INFO-CNT-GAIRAI    PIC 9(07).
      *元格納場所
           03  FILE-INFO-FOR-FOLDER    PIC X(200).
      *元ファイル名
           03  FILE-INFO-FOR-DATA      PIC X(200).
      *先格納場所
           03  FILE-INFO-TO-FOLDER     PIC X(200).
      *先ファイル名
           03  FILE-INFO-TO-DATA       PIC X(200).
      *コード区分（空白、１：Ｓ−ｊｉｓ ２：ＥＵＣ）
           03  FILE-INFO-CODE-TYPE     PIC X(01).
      *ＣＳＶ用ＤＢ有無区分（１：ＤＢ有）
           03  FILE-INFO-DATA-TYPE     PIC X(01).
      *エラーコード（１：エラー有）
           03  FILE-INFO-ERR-CODE      PIC X(01).
      *エラーメッセージ
           03  FILE-INFO-ERR-MSG       PIC X(200).
      *処理日
           03  FILE-INFO-SYOYMD        PIC X(08).
      *作成日
           03  FILE-INFO-CREYMD        PIC X(08).
      *作成時間
           03  FILE-INFO-CREHMS        PIC X(06).
