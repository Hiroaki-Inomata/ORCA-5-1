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
      *    チェックマスタ        COPY (CPCHK.INC)                     *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02.03.14  主キー更新用項目の追加
      *****************************************************************
      *01  CHK-REC.
           03  CHK-KEY.
               05  CHK-HOSPNUM         PIC 9(2).
      *チェック区分
               05  CHK-CHKKBN          PIC  X(01).
      *診療コード
               05  CHK-SRYCD           PIC  X(09).
      *有効開始年月日
               05  CHK-YUKOSTYMD       PIC  X(08).
      *有効終了年月日
               05  CHK-YUKOEDYMD       PIC  X(08).
      *コード区分
               05  CHK-CDKBN           PIC  X(01).
      *連番号
               05  CHK-RENNUM          PIC  9(02).
      *適用コード個数
           03  CHK-TEKCDNUM            PIC  9(03).
      *コード１から１０
           03  CHK-CD-G.
               05  CHK-CD1             PIC  X(09).
               05  CHK-CD2             PIC  X(09).
               05  CHK-CD3             PIC  X(09).
               05  CHK-CD4             PIC  X(09).
               05  CHK-CD5             PIC  X(09).
               05  CHK-CD6             PIC  X(09).
               05  CHK-CD7             PIC  X(09).
               05  CHK-CD8             PIC  X(09).
               05  CHK-CD9             PIC  X(09).
               05  CHK-CD10            PIC  X(09).
      *
           03  CHK-CD-R                REDEFINES   CHK-CD-G.
               05  CHK-CD              PIC  X(09)
                                       OCCURS   10.
      *端末ＩＤ
           03  CHK-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  CHK-OPID                PIC  X(16).
      *作成年月日
           03  CHK-CREYMD              PIC  X(08).
      *更新年月日
           03  CHK-UPYMD               PIC  X(08).
      *更新時間 
           03  CHK-UPHMS               PIC  X(06).
      *主キー更新用項目
           03  CHKUP-KEY.
               05  CHKUP-HOSPNUM       PIC  9(02).
               05  CHKUP-CHKKBN        PIC  X(01).
               05  CHKUP-SRYCD         PIC  X(09).
               05  CHKUP-YUKOSTYMD     PIC  X(08).
               05  CHKUP-YUKOEDYMD     PIC  X(08).
               05  CHKUP-CDKBN         PIC  X(01).
               05  CHKUP-RENNUM        PIC  9(02).
      *
      *検索用項目
           03  CHK-SELNAME             PIC  X(200).
