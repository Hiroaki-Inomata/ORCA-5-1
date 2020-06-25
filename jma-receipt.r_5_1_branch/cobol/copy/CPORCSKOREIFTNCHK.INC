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
      *    高齢者の一部負担金額記載に関するチェックパラメタ      *
      *                         COPY  (CPORCSKOREIFTNCHK.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-門脇    06/08/10  高齢者経過措置フラグ追加
      *                                     主保険単独の患者負担額追加
      *                                     公費を含む場合の患者負担額追加
      *  03.03.01    NACL-門脇    06/11/02  減額・免除・支払猶予対応
      *  03.04.01    NACL-門脇    06/11/20  高額４回目以降対応
      *  03.04.02    NACL-門脇    07/03/16  全国公費有り無しフラグ追加
      *                                     公費を含む場合の主保険の一部負
      *                                     担額２追加
      *  03.04.03    NACL-門脇    07/04/01  高齢者・若人区分追加
      *                                     年齢追加
      *                                     若人区分追加
      *                                     返却値に請求金額追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/12/03  県番号追加
      *  04.02.01    NACL-門脇    08/04/01  就学前区分追加
      *                                     軽減特例措置対象者区分追加
      *                                     マル長公費ＩＤ追加
      *  04.03.01    NACL-門脇    09/01/05  ７５歳到達月の特例区分追加
      *                                     マル長のみ区分追加
      *  04.07.01    NACL-門脇    13/11/11  減額（円）の対応
      ************************************************************
       01  ORCSKOREIFTNCHKAREA.
      *
      *入力値
      *    医療機関ＩＤ
           03  LNK-KOREIFTNCHK-HOSPNUM      PIC 9(2).
      *    患者ＩＤ
           03  LNK-KOREIFTNCHK-PTID         PIC 9(10).
      *    入外区分
           03  LNK-KOREIFTNCHK-NYUGAIKBN    PIC X(01).
      *    診療年月
           03  LNK-KOREIFTNCHK-SRYYM        PIC X(06).
      *    主保険−保険番号
           03  LNK-KOREIFTNCHK-HKNNUM       PIC X(03).
      *    主保険−保険ＩＤ
           03  LNK-KOREIFTNCHK-HKNID        PIC 9(10).
      *    低所得フラグ
           03  LNK-KOREIFTNCHK-TEISYOTOKU   PIC X(01).
      *    レセプト科
           03  LNK-KOREIFTNCHK-RECEKA       PIC X(02).
      *    高齢者経過措置フラグ
           03  LNK-KOREIFTNCHK-KEIKASOCHI   PIC X(01).
      *    減額区分
           03  LNK-KOREIFTNCHK-GENGAKUKBN   PIC 9(01).
      *    減額（割合）
           03  LNK-KOREIFTNCHK-GENGAKU-RITU PIC 9(03).
      *    減額（金額）
           03  LNK-KOREIFTNCHK-GENGAKU-KIN  PIC 9(07).
      *    減額計算フラグ
           03  LNK-KOREIFTNCHK-GENGAKUFLG   PIC 9(01).
      *    高額４回目以降
           03  LNK-KOREIFTNCHK-KOHYONKAIME  PIC 9(01).
      *    高齢者・若人区分
           03  LNK-KOREIFTNCHK-NENREIKBN    PIC X(01).
      *    年齢
           03  LNK-KOREIFTNCHK-AGE          PIC 9(03).
      *    県番号
           03  LNK-KOREIFTNCHK-PREFNUM      PIC X(02).
      *    就学前区分
           03  LNK-KOREIFTNCHK-SYUGAKUKBN   PIC 9(01).
      *    マル長公費ＩＤ
           03  LNK-KOREIFTNCHK-KOHID-CYOKI  PIC 9(10).
      *    ７５歳到達月の特例区分
           03  LNK-KOREIFTNCHK-TOKU75       PIC 9(01).
      *    マル長のみ区分
           03  LNK-KOREIFTNCHK-CYOKINOMI    PIC 9(01).
      *    所得区分
           03  LNK-KOREIFTNCHK-SYOTOKUKBN   PIC 9(01).
      *
      *返却値
           03  LNK-KOREIFTNCHK-OUT-AREA.
      *主保険単独の患者負担額
             05 LNK-KOREIFTNCHK-FTNGAKU     PIC 9(07).
      *公費を含む場合の患者負担額
             05 LNK-KOREIFTNCHK-KOHFTNGAKU  PIC 9(07).
      *公費を含む場合の主保険の一部負担額
             05 LNK-KOREIFTNCHK-ICHIBUGAKU  PIC 9(07).
      *公費を含む場合の主保険の一部負担額（１円単位）（判定用）
             05 LNK-KOREIFTNCHK-ICHIBUGAKU2 PIC 9(07).
      *入　：高齢者＋地方公費の請求金額
             05 LNK-KOREIFTNCHK-CHIHOUGAKU  PIC 9(07).
      *
      *復帰コード
           03  LNK-KOREIFTNCHK-RC           PIC 9(02).
