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
      *         チェックマスタ４     (CPCK1102.INC)              *
      *         （１１０２）                                     *
      *         データチェック機能情報３                         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  CK4-1102-REC.
           03  CK4-1102-KEY.
               05  CK4-1102-HOSPNUM        PIC 9(2).
      *管理コード
               05  CK4-1102-KANRICD        PIC  X(04).
      *区分コード
               05  CK4-1102-KBNCD          PIC  X(08).
      *有効開始年月日
               05  CK4-1102-STYUKYMD       PIC  X(08).
      *有効終了年月日
               05  CK4-1102-EDYUKYMD       PIC  X(08).
      *患者ＩＤ
           03  CK4-1102-PTID               PIC  9(10).
      *管理テーブル
           03  CK4-1102-TBL.
      *保険者番号
               05  CK4-1102-HKNJANUM       PIC  X(08).
      *保険者番号文字列長
               05  CK4-1102-HKNJANUMLEN    PIC  9(03).
      *条件
               05  CK4-1102-JYKNKBN        PIC  X(01).
      *保険の種類
               05  CK4-1102-HKNNUM         PIC  X(03).
      *記号チェック
               05  CK4-1102-KIGOCHK        PIC  X(01).
      *記号チェックパターン
               05  CK4-1102-KGCHKPTN       PIC  X(01).
      *記号数字チェック
               05  CK4-1102-KGSUJI         PIC  X(01).
      *記号漢字チェック
               05  CK4-1102-KGKANJI        PIC  X(01).
      *記号英字チェック
               05  CK4-1102-KGABT          PIC  X(01).
      *記号ハイフン、長音チェック
               05  CK4-1102-KGHYPN         PIC  X(01).
      *記号括弧チェック
               05  CK4-1102-KGKAKKO        PIC  X(01).
      *記号その他チェック
               05  CK4-1102-KGETC          PIC  X(01).
      *記号漢字未入力チェック
               05  CK4-1102-KGKANJINASHI   PIC  X(01).
      *番号数字無しチェック
               05  CK4-1102-NMSUJINASHI    PIC  X(01).
      *番号１文字目チェックパターン
               05  CK4-1102-NM1CHKPTN      PIC  X(01).
      *番号１文字目数字チェック
               05  CK4-1102-NM1SUJI        PIC  X(01).
      *番号１文字目漢字チェック
               05  CK4-1102-NM1KANJI       PIC  X(01).
      *番号１文字目英字チェック
               05  CK4-1102-NM1ABT         PIC  X(01).
      *番号１文字目ハイフン、長音チェック
               05  CK4-1102-NM1HYPN        PIC  X(01).
      *番号１文字目括弧チェック
               05  CK4-1102-NM1KAKKO       PIC  X(01).
      *番号１文字目その他チェック
               05  CK4-1102-NM1ETC         PIC  X(01).
      *番号２文字目以降チェックパターン
               05  CK4-1102-NM2CHKPTN      PIC  X(01).
      *番号２文字目以降数字チェック
               05  CK4-1102-NM2SUJI        PIC  X(01).
      *番号２文字目以降漢字チェック
               05  CK4-1102-NM2KANJI       PIC  X(01).
      *番号２文字目以降英字チェック
               05  CK4-1102-NM2ABT         PIC  X(01).
      *番号２文字目以降ハイフン、長音チェック
               05  CK4-1102-NM2HYPN        PIC  X(01).
      *番号２文字目以降括弧チェック
               05  CK4-1102-NM2KAKKO       PIC  X(01).
      *番号２文字目以降その他チェック
               05  CK4-1102-NM2ETC         PIC  X(01).
      *更新チェック用ダミー
               05  CK4-1102-UPDDUMMY       PIC  X(01).
      *番号桁数下限
               05  CK4-1102-NMKETAMIN      PIC  X(02).
      *番号桁数上限
               05  CK4-1102-NMKETAMAX      PIC  X(02).
      *記号桁数下限
               05  CK4-1102-KGKETAMIN      PIC  X(02).
      *記号桁数上限
               05  CK4-1102-KGKETAMAX      PIC  X(02).
      *
               05  CK4-1102-YOBI           PIC  X(452).
      *端末ＩＤ
           03  CK4-1102-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  CK4-1102-OPID               PIC  X(16).
      *作成年月日
           03  CK4-1102-CREYMD             PIC  X(08).
      *更新年月日
           03  CK4-1102-UPYMD              PIC  X(08).
      *更新時間
           03  CK4-1102-UPHMS              PIC  X(06).
