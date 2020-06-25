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
      *    ユーザプログラム起動パラメタ      COPY (CPORCSUSERCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/06/XX  伝票番号追加対応
      ************************************************************
      *
       01  ORCSUSERCHKAREA.
      *
      *処理ＰＧ
           03  ORCSUSERCHK-GMNPG            PIC X(08).
      *処理区分(１：チェック、２：実行）
           03  ORCSUSERCHK-SYORI            PIC X(01).
      *実行の時のみ
           03  ORCSUSERCHK-IN-AREA.
      *処理内容(１：新規、２：更新、３：削除）
      *※入退院登録業務は業務の処理区分（01:入院等）となる。
               05  ORCSUSERCHK-SYORI-FLG        PIC 9(02).
      *システム日付西暦
               05  ORCSUSERCHK-SYSYMD           PIC X(08).
      *診療日
               05  ORCSUSERCHK-SRYYMD           PIC X(08).
      *患者ＩＤ
               05  ORCSUSERCHK-PTID             PIC 9(10).
      *患者番号
               05  ORCSUSERCHK-PTNUM            PIC X(20).
      *診療科
               05  ORCSUSERCHK-SRYKA            PIC X(02).
      *ドクター
               05  ORCSUSERCHK-DRCD             PIC X(05).
      *保険組合せ
               05  ORCSUSERCHK-HKNCOMBI         PIC X(04).
      *伝票番号（K03）
               05  ORCSUSERCHK-DENPNUM          PIC 9(07).
      *
      *出力
      *    １：画面表示あり
           03  ORCSUSERCHK-GMNSYORI         PIC X(01).
      *    画面初期表示
           03  ORCSUSERCHK-GYOUMU-FLG       PIC X(01).
      *    1:実行あり
           03  ORCSUSERCHK-GYOUMU-ARI       PIC X(01).
      *
      *
