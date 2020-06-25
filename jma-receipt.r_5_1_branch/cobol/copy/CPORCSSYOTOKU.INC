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
      ************************************************************
      *    患者所得情報編集パラメタ
      *    COPY  (CPORCSSYOTOKU.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSSYOTOKUAREA.
      *入力値
      *    医療機関ＩＤ
           03  LNK-SYOTOKU-HOSPNUM         PIC 9(02).
      *    患者ＩＤ
           03  LNK-SYOTOKU-PTID            PIC 9(10).
      *    処理年月
           03  LNK-SYOTOKU-SRYYM           PIC X(06).
      *    食事区分チェック＋【食事用】返却項目編集
           03  LNK-SYOTOKU-SKJCHKFLG       PIC X(01).
      *    【食事用】食事区分インデックス（レセプト用）取得用の保険組み合わせ
           03  LNK-SYOTOKU-HKNCOMBIINF.
               05  LNK-SYOTOKU-HKNCOMBI    PIC 9(04)   OCCURS  10.
      *返却項目
           03  LNK-SYOTOKU-HENKYAKU-AREA.
               05  LNK-SYOTOKU-HENKYAKU-TABLE     OCCURS  31.
      *主保険・全国公費区分
      *  （空白：一般　１：低１　２：低２　４：低１（老齢））
                   07  LNK-SYOTOKU-SYUKBN         PIC  X(01).
      *地方公費区分
      *  （空白：一般　１：低１　２：低２）
                   07  LNK-SYOTOKU-CHIKBN         PIC  X(01).
      *食事区分
      *  （空白：一般　１：低１　２：低２　３：低２（９０日超）　４：低１（老齢））
                   07  LNK-SYOTOKU-SKJKBN         PIC  X(01).
      *
      *【食事用】境界層区分
      *  （空白：非該当　１：該当）
                   07  LNK-SYOTOKU-KKSKBN         PIC  X(01).
      *【食事用】年齢区分
      *  （食事算定保険組み合わせ（同日再入院分も含む）が
      *    0000・特療費・労災・自賠責・公害・自費・治験の場合は空白）
      *  （空白：対象外　１：高齢者　２：７０歳未満）
                   07  LNK-SYOTOKU-AGEKBN         PIC  X(01).
      *
      *【食事用】食事区分インデックス（レセプト用）
               05  LNK-SYOTOKU-SKJIDX             PIC  9(02).
      *
      *復帰コード（１：処理年月エラー　２：患者ＩＤエラー）
      *          （３：低１エラー　　　４：低２エラー　　）
           03  LNK-SYOTOKU-RC                     PIC  S9(9) BINARY.
      *
