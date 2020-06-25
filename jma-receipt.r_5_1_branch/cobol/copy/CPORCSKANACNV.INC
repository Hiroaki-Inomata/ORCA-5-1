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
      *    カナ変換      COPY (CPORCSKANACNV.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     02.05.29  新規作成
      *  04.08.01    NACL-森脇    15/12/21  かな変換対応
      ************************************************************
       01  KANACNV-LNK.
      *    処理フラグ（空白：カナ変換、１：かな変換）
           03  KANACNV-SYORI           PIC  X(01).
           03  KANACNV-IN-STR          PIC  X(200).
      *    1:ひらがなのみ／2:カタカナのみ／3:不明
           03  KANACNV-TYPE            PIC  9(01).
      *    1:ひらがなのみの場合…カタカナに変換して設定
      *    2:カタカナのみの場合…カタカナに変換して設定
      *    3:不明の場合…空白を設定
           03  KANACNV-OT-STR          PIC  X(200).
      *    リターンコード
      *    0:正常
      *    1:半角混入
           03  KANACNV-RC              PIC  9(01).
