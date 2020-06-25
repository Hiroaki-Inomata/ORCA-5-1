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
      *    入院会計作成用パラメタ　　　COPY (CPORCSNYUACCT.INC)
      *                                使用プログラム (ORCGI01)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYUACCT-AREA.
      *
      *入院会計作成区分（１：入院登録　２：退院登録　３：変更
      *                  ４：異動取消時の入院会計削除
      *                  ５：入院取消（入院会計のみ）
      *                  ６：入院取消（入院会計と診療会計）
      *                　７：退院取消　８：異動　９：次月作成
      *                　Ａ：退院登録（診療保存））
           03  LNK-SCNYUACCT-SYORI-KBN               PIC  X(01).
      *病棟番号
           03  LNK-SCNYUACCT-BTU-NUM                 PIC  X(02).
      *特定入院料区分（０：一般入院料　１：特定入院料）
           03  LNK-SCNYUACCT-TOKUTEINYUIN-KBN        PIC  X(01).
      *入院会計作成期間（開始）
           03  LNK-SCNYUACCT-STSRYYMD                PIC  X(08).
      *入院会計作成期間（終了）
           03  LNK-SCNYUACCT-EDSRYYMD                PIC  X(08).
      *入院基本料エリア
           03  LNK-SCNYUACCT-NYUKHN.
               05  LNK-SCNYUACCT-NYUKHN-SRYCD        PIC  X(09)
                                                     OCCURS  6.
           03  LNK-SCNYUACCT-KAGENCD.
               05  LNK-SCNYUACCT-KAGENCD-TBL         OCCURS  10.
                   07  LNK-SCNYUACCT-KAGEN-SRYCD     PIC  X(09).
                   07  LNK-SCNYUACCT-KAGEN-STYUKYMD  PIC  X(08).
                   07  LNK-SCNYUACCT-KAGEN-EDYUKYMD  PIC  X(08).
      *通算算定済日数（入院料加算算定用）
           03  LNK-SCNYUACCT-NYUKHN-NISUU            PIC  9(05).
      *入院料加算領域
           03  LNK-SCNYUACCT-NYUKHNKSN-AREA          OCCURS  20.
      *入院料加算区分
             05  LNK-SCNYUACCT-NYUKHNKSN-KBN         PIC  X(01).
      *入院料加算診療コード
             05  LNK-SCNYUACCT-NYUKHNKSN-SRYCD       PIC  X(09).
      *外泊診療コード
           03  LNK-SCNYUACCT-GAIHAKU-SRYCD           PIC  X(09).
      *室料差額診療コード
           03  LNK-SCNYUACCT-SAGAKU-SRYCD            PIC  X(09).
      *室料差額区分
           03  LNK-SCNYUACCT-SAGAKU-KBN              PIC  9(02).
      *食事診療コード
           03  LNK-SCNYUACCT-SHOKUJI-SRYCD           PIC  X(09).
      *食事区分
           03  LNK-SCNYUACCT-SHOKUJIKBN              PIC  9(01).
      *食事セット区分（空白：SHOKUJIKBNを使用　１：朝昼夜の食事区分を使用）
           03  LNK-SCNYUACCT-SHOKUJI-SETKBN          PIC  X(01).
      *食事区分（朝）
           03  LNK-SCNYUACCT-SHOKUJIKBN-ASA          PIC  9(01).
      *食事区分（昼）
           03  LNK-SCNYUACCT-SHOKUJIKBN-HIRU         PIC  9(01).
      *食事区分（夜）
           03  LNK-SCNYUACCT-SHOKUJIKBN-YORU         PIC  9(01).
      *食堂加算
           03  LNK-SCNYUACCT-SHOKUDO-SRYCD           PIC  X(09).
      *食堂加算区分
           03  LNK-SCNYUACCT-SHOKUDO-KBN             PIC  9(01).
      *保険組合コード
           03  LNK-SCNYUACCT-HKNCOMBINUM-SRYCD       PIC  X(09).
      *保険組合せ
           03  LNK-SCNYUACCT-HKNCOMBINUM             PIC  9(04).
      *転科時元入院科
           03  LNK-SCNYUACCT-MOTO-NYUINKA            PIC  X(02).
      *選定療養費診療行為コード
           03  LNK-SCNYUACCT-SENTEI-SRYCD            PIC  X(09).
      *選定療養費開始日
           03  LNK-SCNYUACCT-SENTEI-STYMD            PIC  X(08).
      *入院カレンダー設定区分
           03  LNK-SCNYUACCT-NYUCAL-KBN              PIC  9(01).
      *同日再入院の入院取消時の診療会計削除区分
      *（空白：同日再入院以外、１：同日再入院）
           03  LNK-SCNYUACCT-DOUJITSUKBN             PIC  X(01).
      *診療会計削除開始日
           03  LNK-SCNYUACCT-SRYDELYMD               PIC  X(08).
      *異動取消時の入院会計作成区分
           03  LNK-SCNYUACCT-IDOSAKUSEI-KBN          PIC  X(01).
      *復帰コード
           03  LNK-SCNYUACCT-RC                      PIC S9(9) BINARY.
      *
