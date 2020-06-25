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
      *    保険算定用年齢・割合計算パラメタ                      *
      *                         COPY  (CPORCSAGECHK.INC)          *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  03/11/13  老人フラグ追加
      * 01.00.02     NACL-多々納  04/04/21  老人区分２、保険区分追加
      *                                     実年齢追加
      * 01.00.03     NACL-多々納  04/05/13  入外区分追加
      * 01.00.04     NACL-多々納  05/09/15  公費単独区分追加
      * 03.00.01     NACL-多々納  06/08/25  平成１８年１０月改正対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.02.00     NACL-多々納  08/03/XX  就学前区分追加
      ************************************************************
       01  ORCSAGECHKAREA.
      *入力値
      *    医療機関ＩＤ（必須）
           03  AGECHK-HOSPNUM          PIC 9(02).
      *    患者ＩＤ（必須）
           03  AGECHK-PTID             PIC 9(10).
      *    診療日（必須）
           03  AGECHK-SRYYMD           PIC X(08).
      *    患者生年月日
           03  AGECHK-BIRTHDAY         PIC X(08).
      *    保険組合番号
           03  AGECHK-HKNCOMBINUM      PIC X(04).
      *    入外区分（１：入院、以外は外来）
           03  AGECHK-NYUGAIKBN        PIC X(01).
      *出力値
           03  AGECHK-OUT-AREA.
      *        年齢（保険）
               05  AGECHK-NENREI         PIC 9(03).
      *        以下、保険組合番号入力時
      *        負担割合（３歳未満と70歳以上の８，９割給付のみ）
               05  AGECHK-FTNRATE        PIC 9(03).
      *        負担割合名称
               05  AGECHK-FTNRATEMEI     PIC X(10).
      *        老人エラー（１：老人保険なし、２：７５歳以上老人公費なし
      *                    ３：２割）
               05  AGECHK-RJNCHK         PIC 9(01).
      *        保険エラー（１：保険なし）
               05  AGECHK-HKNCHK         PIC 9(01).
      *        老人区分（０：一般、１：老人）
               05  AGECHK-ROUJIN         PIC 9(01).
      *        老人区分２（０：一般、１：老人）
      *        （当日に老人公費がある時）
               05  AGECHK-ROUJIN2        PIC 9(01).
      *        保険区分（１：労災、２：自賠責、３：自費、０：以外
      *                            ４：自賠責（健保準拠））
      *                            ５：公務災害（健保準拠））
               05  AGECHK-HKNKBN         PIC 9(01).
      *        実年齢（診療日の年齢）
               05  AGECHK-NENREI2.
                   07  AGECHK-NENREI2-YY     PIC 9(03).
                   07  AGECHK-NENREI2-MM     PIC 9(02).
                   07  AGECHK-NENREI2-DD     PIC 9(02).
      *        地方公費単独（１：単独）
               05  AGECHK-KOHCHK         PIC 9(01).
      *        国保前期高齢者判定有無
               05  AGECHK-KAKUNINCHK     PIC 9(01).
      *        就学前区分（１：就学前）
               05  AGECHK-SYUGAKUKBN     PIC 9(01).
      *    復帰コード
           03  AGECHK-RC              PIC 9(02).
      *
      *
