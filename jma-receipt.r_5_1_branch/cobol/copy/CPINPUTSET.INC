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
      *    入力セット       COPY (CPINPUTSET.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/10/02  入力区分、換算入力数量追加
      *  05.00.00    NACL-多々納  20/02/XX  コメント、入力値桁数変更
      ************************************************************
      *01 INPUTSET-REC.
           03 ISET-KEY.
      *医療機関ＩＤ
               05 ISET-HOSPNUM          PIC 9(2).
      *セットコード
               05 ISET-SETCD           PIC  X(06).
      *有効開始年月日
               05 ISET-YUKOSTYMD       PIC  X(08).
      *有効終了年月日
               05 ISET-YUKOEDYMD       PIC  X(08).
      *セット内番号
               05 ISET-SETSEQ          PIC  9(03).
      *入力（診療）コード
           03 ISET-INPUTCD             PIC  X(09).
      *数量１
           03 ISET-SURYO1              PIC  9(07)V9(5).
      *数量２
           03 ISET-SURYO2              PIC  9(07)V9(5).
      *回数
           03 ISET-KAISU               PIC  9(03).
      *入力コメント
           03 ISET-COMMENT             PIC  X(140).
      *入力値
           03 ISET-ATAI-AREA.
      *入力値１
               05 ISET-ATAI1           PIC  X(10).
      *入力値２
               05 ISET-ATAI2           PIC  X(10).
      *入力値３
               05 ISET-ATAI3           PIC  X(10).
      *入力値４
               05 ISET-ATAI4           PIC  X(10).
      *入力値５
               05 ISET-ATAI5           PIC  X(10).
           03  ISET-ATAI-AREA-R        REDEFINES    ISET-ATAI-AREA.
               05 ISET-ATAI            PIC  X(10)   OCCURS  5.
      *入力区分（１：関係コメント指示、２：内服種類数指示）
           03 ISET-INPUTKBN            PIC  X(01).
      *換算入力数量
           03 ISET-KANSURYO            PIC  9(05)V9(5).
      *端末ＩＤ
           03 ISET-TERMID              PIC  X(16).
      *オペレータＩＤ
           03 ISET-OPID                PIC  X(16).
      *作成年月日
           03 ISET-CREYMD              PIC  X(08).
      *更新年月日
           03 ISET-UPYMD               PIC  X(08).
      *更新時間
           03 ISET-UPHMS               PIC  X(06).
