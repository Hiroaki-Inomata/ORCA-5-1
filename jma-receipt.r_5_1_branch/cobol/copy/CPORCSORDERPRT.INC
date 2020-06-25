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
      *----------------------------------------------------------*
      *    入院指示せん帳票オーダー作成パラ   COPY (CPORCSORDERPRT.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  11/10/XX  同日再入院対応
      ************************************************************
      *
       01  ORCSORDERPRTAREA.
      *    処理ＰＧ
           03  LNK-ORDERPRT-PG         PIC X(04).
      *    対象ファイル処理
           03  LNK-ORDERPRT-KBN        PIC X(01).
      *    入院処方箋発行
           03  LNK-ORDERPRT-SYOHOU     PIC 9(01).
      *    注射処方箋発行
           03  LNK-ORDERPRT-CHUSYA     PIC 9(01).
      *    指示箋発行
           03  LNK-ORDERPRT-SIJISEN    PIC 9(01).
      *    ドクターコード
           03  LNK-ORDERPRT-DRCD       PIC X(05).
      *    同日再入院区分
           03  LNK-ORDERPRT-DOUJITSUKBN    PIC X(01).
      *
      *    出力
      *    発行日
           03  LNK-ORDERPRT-PRINTYMD   PIC X(08).
      *    発行時間
           03  LNK-ORDERPRT-PRINTHMS   PIC X(06).
      *
      *    復帰コード
           03  LNK-ORDERPRT-RCD        PIC 9(02).
      *
