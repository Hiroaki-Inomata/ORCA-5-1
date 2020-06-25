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
      *         チェックマスタ４    COPY   (CPCHK004.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *KANRICD
      *1102 チェック機能情報３
      *1103 データチェック時の個々のチェックについてのする／しない設定
      *1200 前回エラー情報
      *1201 前回エラー情報の詳細
      *2000 個別指示基本情報
      *2001 個別選択患者
      *2002 前回確認項目
       01  CHK004-REC.
      *キー
           03  CHK004-KEY.
               05  CHK004-HOSPNUM          PIC 9(2).
      *管理コード
               05  CHK004-KANRICD          PIC  X(04).
      *区分コード
               05  CHK004-KBNCD            PIC  X(08).
      *開始有効年月日
               05  CHK004-STYUKYMD         PIC  X(08).
      *終了有効年月日
               05  CHK004-EDYUKYMD         PIC  X(08).
      *患者ＩＤ
           03  CHK004-PTID                 PIC  9(10).
      *管理テーブル
           03  CHK004-KANRITBL             PIC  X(500).
      *端末ＩＤ
           03  CHK004-TERMID               PIC  X(16).
      *オペーレータＩＤ
           03  CHK004-OPID                 PIC  X(16).
      *作成年月日
           03  CHK004-CREYMD               PIC  X(08).
      *更新年月日
           03  CHK004-UPYMD                PIC  X(08).
      *更新時間
           03  CHK004-UPHMS                PIC  X(06).
      *　　主キー更新用項目
           03  CHK004-UPDKEY.
               05  CHK004-UPHOSPNUM        PIC 9(2).
               05  CHK004-UPKANRICD        PIC X(04).
               05  CHK004-UPKBNCD          PIC X(08).
               05  CHK004-UPSTYUKYMD       PIC X(08).
               05  CHK004-UPEDYUKYMD       PIC X(08).
           03  CHK004-SELLIMIT             PIC 9(08).
           03  CHK004-SELOFFSET            PIC 9(08).
