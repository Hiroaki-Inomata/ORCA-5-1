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
      *    ファイル情報診療情報システム用  (CPFILE-MIS.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  FILE-MIS-REC.
      *
           03  FILE-MIS-KEY.
      *
               05  FILE-MIS-HOSPNUM    PIC 9(02).
      *ＫＥＹ
               05  FILE-MIS-TBL-KEY    PIC X(08).
      *連番
               05  FILE-MIS-RENNUM     PIC 9(04).
      *グループ
               05  FILE-MIS-TBL-GROUP  PIC X(14).
      *処理内連番
               05  FILE-MIS-SHORI-RENNUM
                                       PIC 9(04).
      *シェルＩＤ
           03  FILE-MIS-SHELLID        PIC X(20).
      *診療年月
           03  FILE-MIS-SRYYM          PIC X(06).
      *診療年月日
           03  FILE-MIS-SRYYMD         PIC X(08).
      *件数
           03  FILE-MIS-CNT            PIC 9(07).
      *入外区分
           03  FILE-MIS-NYUGAIKBN      PIC X(01).
      *先格納場所
           03  FILE-MIS-TO-FOLDER      PIC X(200).
      *先ファイル名
           03  FILE-MIS-TO-DATA        PIC X(200).
      *コード区分（空白、１：ＥＵＣ ２：Ｓ−ｊｉｓ）
           03  FILE-MIS-CODE-TYPE      PIC X(01).
      *出力フラグ
           03  FILE-MIS-PUTFLG         PIC X(1).
      *タイトル
           03  FILE-MIS-TITLE          PIC X(100).
      *UUID
           03  FILE-MIS-TBL-UUID       PIC X(36).
      *作成年月日
           03  FILE-MIS-CREYMD         PIC X(08).
      *更新年月日
           03  FILE-MIS-UPYMD          PIC  X(08).
      *更新時間
           03  FILE-MIS-UPHMS          PIC  X(06).
