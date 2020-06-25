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
      *    一時チェックマスタ２編集サブ　COPY (CPORCSS013.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS013-AREA.
           03  SS013-FUNC                   PIC X(20).
      *
      *    作成区分
      *    空白:チェックマスタ＋適応病名マスタ
      *    1:適応病名マスタのみ
      *    2:チェックマスタのみ
      *    9:データ登録なし
           03  SS013-CREATEKBN              PIC X(01).
           03  SS013-CHKKBN                 PIC X(01).
           03  SS013-SRYCD                  PIC X(09).
           03  SS013-KJNYMD                 PIC X(08).
      *    適応病名マスタ区分
      *    0:適応病名マスタをインポートしない
      *    1:適応病名マスタをインポートする
      *    2:チェックマスタの設定に従って適応病名マスタをインポートする
           03  SS013-TEKBYOKBN              PIC X(01).
           03  SS013-TEMP-NAME              PIC X(10).
           03  SS013-TEMP-KEY               PIC X(16).
