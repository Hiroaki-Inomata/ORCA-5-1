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
      *    ユーザプログラム起動パラメタ      COPY (CPORCSUSERPG.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/06/XX  伝票番号追加対応
      ************************************************************
      *
       01  ORCSUSERPGAREA.
      *
      *処理ＰＧ
           03  ORCSUSERPG-GMNPG            PIC X(08).
      *
      *処理状況（１：追加、２：更新、３：削除）
      *※入退院登録業務は業務の処理区分（01:入院等）となる。
           03  ORCSUSERPG-SYORI-FLG        PIC 9(02).
      *システム日付西暦
           03  ORCSUSERPG-SYSYMD           PIC X(08).
      *診療日
           03  ORCSUSERPG-SRYYMD           PIC X(08).
      *患者ＩＤ
           03  ORCSUSERPG-PTID             PIC 9(10).
      *患者番号
           03  ORCSUSERPG-PTNUM            PIC X(20).
      *診療科
           03  ORCSUSERPG-SRYKA            PIC X(02).
      *ドクター
           03  ORCSUSERPG-DRCD             PIC X(05).
      *保険組合せ
           03  ORCSUSERPG-HKNCOMBI         PIC X(04).
      *処理・帳票名
           03  ORCSUSERPG-SYORINM          PIC X(30).
      *実行名
           03  ORCSUSERPG-SHELL            PIC X(100).
      *プリンタ名
           03  ORCSUSERPG-PRTNM            PIC X(16).
      *伝票番号(K03)
           03  ORCSUSERPG-DENPNUM          PIC 9(07).
      *リポジトリ名
           03  ORCSUSERPG-REPOS-NAME       PIC X(128).
      *
