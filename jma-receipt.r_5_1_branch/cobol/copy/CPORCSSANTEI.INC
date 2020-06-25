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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *---------------------------------------------------*
      *    算定履歴変更パラメタ                          *
      *                         COPY  (CPORCSSANTEI.INC) *
      *
      *---------------------------------------------------*
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/12/05  初診料算定日追加
      *  01.00.02    NACL-多々納  05/03/03  当月点数・点数区分追加
      *  01.00.03    NACL-多々納  06/03/15  数量追加
      *  03.04.00    NACL-多々納  06/12/01  自賠責健保準拠追加
      *  04.01.00    NACL-多々納  07/10/19  公務災害追加
      *  04.07.00    NACL-多々納  11/08/XX  算定履歴付加削除追加
      *****************************************************************
       01  ORCSSANTEIAREA.
      *
      *処理区分（１：追加、更新、３：削除）
           03  SSANTEI-SYORI             PIC  X(01).
      *処理区分２（１：算定履歴付加削除）
           03  SSANTEI-SYORI2            PIC  X(01).
      *処理ＰＧ
           03  SSANTEI-PG                PIC  X(03).
      *対象コード
           03  SSANTEI-SRYCD             PIC  X(09).
      *診療年月日
           03  SSANTEI-SRYYMD            PIC  X(08).
      *初回年月日（必要な時のみ）
           03  SSANTEI-SYOKAIYMD         PIC  X(08).
      *患者ＩＤ
           03  SSANTEI-PTID              PIC  9(10).
      *算定回数
           03  SSANTEI-KAISU             PIC  9(08).
      *算定数量（リハビリ他）
           03  SSANTEI-SRYSURYO          PIC  9(05).
      *初診料算定日（新規でＫ０３の時のみ）
           03  SSANTEI-SYOYMD            PIC  X(08).
      *診療科
           03  SSANTEI-SRYKA             PIC  X(02).
      *保険組合せ
           03  SSANTEI-HKNCOMBINUM       PIC  X(04).
      *保険区分
           03  SSANTEI-HKNKBN            PIC  X(01).
      *労災・自賠責区分
           03  SSANTEI-RSI-HKNKBN        PIC  X(01).
      *労災・自賠責 健保準拠
           03  SSANTEI-RSI-JUNKYO        PIC  X(01).
      *内分泌負荷試験用
      *当月算定点数
           03  SSANTEI-MSANTEITEN        PIC  9(08).
      *当月算定点数区分
           03  SSANTEI-MSANTEITENKBN     PIC  9(01).
      *
      *    一括更新の時（Ｊ０２）
           03  SSANTEI-IKKATU.
      *入外区分
               05  SSANTEI-NYUGAIKBN       PIC  X(01).
      *当月初回算定日
               05  SSANTEI-MSANTEID        PIC  X(02).
      *当月算定回数
               05  SSANTEI-MSANTEICNT      PIC  9(03).
      *変更診療回数テーブル
               05  SSANTEI-DAY-AREA.
                   07  SSANTEI-DAY         PIC  9(03)
                                           OCCURS   31.
      *変更前診療回数テーブル
               05  SSANTEI-MAE-DAY-AREA.
                   07  SSANTEI-MAE-DAY     PIC  9(03)
                                           OCCURS   31.
      *
      *内分泌負荷試験削除フラグ
           03  SSANTEI-MSANTEIDEL        PIC  9(01).
      *算定履歴付加情報あり（削除）
           03  SSANTEI-SANTEIPLUS        PIC  9(01).
      *
