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
      *         システム管理マスタ     (CPSK5000.INC)   *
      *         （５０００）                            *
      *         医療機関情報−入院基本情報              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.00    NACL-多々納  08/08/28  退院時院外処方せん区分追加
      *
      *  04.08.01    NACL-藤原    17/11/21  次回作成時の食事区分追加
      ************************************************************
       01  SYS-5000-REC.
           03  SYS-5000-KEY.
               05  SYS-5000-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-5000-KANRICD        PIC X(04).
      *区分コード
               05  SYS-5000-KBNCD          PIC X(08).
      *有効開始年月日
               05  SYS-5000-STYUKYMD       PIC X(08).
      *有効終了年月日
               05  SYS-5000-EDYUKYMD       PIC X(08).
      *管理テーブル
           03  SYS-5000-TBL.
      *入院時食事療養
               05  SYS-5000-SHOKUJIRYOYOCD
                                           PIC X(09).
      *食事情報
               05  SYS-5000-SHOKU-INF      OCCURS  5.
                   07  SYS-5000-SHOKUCD    PIC  X(09).
                   07  SYS-5000-SHOKUKBN   PIC  X(01).
      *
      *医療機関減算情報
               05  SYS-5000-GEN-INF        OCCURS   5.
                   07  SYS-5000-GENCD      PIC  X(09).
                   07  SYS-5000-GENKBN     PIC  X(01).
      *入院料加算情報
               05  SYS-5000-KSN-INF        OCCURS  20.
                   07  SYS-5000-KSNCD      PIC  X(09).
                   07  SYS-5000-KSNKBN     PIC  X(01).
      *地域加算情報
               05  SYS-5000-CHIIKIKSN      PIC  X(09).
      *入院会計テーブル表示情報
               05  SYS-5000-GENSAN-INF     OCCURS  10.
                   07  SYS-5000-DSPCD      PIC  X(09).
      *有床診療所一般病床初期加算区分
               05  SYS-5000-IPPANKSNKBN    PIC  X(01).
      *栄養管理経過措置区分
               05  SYS-5000-EIYOUKSNKBN    PIC  X(01).
      *救急・在宅等支援療養病床初期加算区分
               05  SYS-5000-KYUKYUKSNKBN   PIC  X(01).
      *次月作成時の食事区分
               05  SYS-5000-SKJ-CREATEKBN  PIC  X(01).
      *予備（１）
      ******   05                          PIC  X(60).
      ******   05                          PIC  X(56).
      ******   05  SYS-5000-YOBI           PIC  X(55).
      ******   05  SYS-5000-YOBI           PIC  X(54).
      ******   05  SYS-5000-YOBI           PIC  X(53).
               05  SYS-5000-YOBI           PIC  X(52).
      *入院時の食事
               05  SYS-5000-SKJKBN         PIC  X(01).
      *入院時の選定入院
               05  SYS-5000-SENTEIKBN
                                           PIC  X(01).
      *請求書発行方法
               05  SYS-5000-SKYSUMKBN
                                           PIC  X(01).
      *退院時院外処方せん発行
               05  SYS-5000-SHOHOUKBN
                                           PIC  X(01).
      *入院歴表示区分
               05  SYS-5000-REKI-DISPKBN
                                           PIC  X(01).
      *請求書発行区分
               05  SYS-5000-SKYPRTKBN
                                           PIC  X(01).
      *退院証明書発行区分
               05  SYS-5000-TAIPRTKBN
                                           PIC  X(01).
      *診療費明細書発行区分
               05  SYS-5000-SRYPRTKBN
                                           PIC  X(01).
      *退院証明書記載区分
               05  SYS-5000-TAIKSIKBN
                                           PIC  X(01).
      *再計算区分
               05  SYS-5000-SAIKEISANKBN   PIC  X(01).
      *請求書未収額印刷区分
               05  SYS-5000-SKYMISYUPRTKBN
                                           PIC  X(01).
      *選定入院負担区分
               05  SYS-5000-SENTEI-FTNKBN
                                           PIC  X(01).
      *選定入院消費税区分
               05  SYS-5000-SENTEI-TAXKBN
                                           PIC  X(01).
      *選定入院点数単価
               05  SYS-5000-SENTEI-TANKA-X PIC  X(02).
               05  SYS-5000-SENTEI-TANKA-R REDEFINES
                   SYS-5000-SENTEI-TANKA-X.
                   07  SYS-5000-SENTEI-TANKA
                                           PIC  9(02).
      *選定入院特別料金（共通）
               05  SYS-5000-SENTEI-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-KINGAKU-X.
                   07  SYS-5000-SENTEI-KINGAKU
                                           PIC  9(04).
      *室料差額消費税区分
               05  SYS-5000-BRMSGK-TAXKBN
                                           PIC  X(01).
      *入院会計室料差額区分
               05  SYS-5000-NACCT-BRMSGKKBN
                                           PIC  X(01).
      *選定入院特別料金（一般）
               05  SYS-5000-SENTEI-IPN-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-IPN-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-IPN-KINGAKU-X.
                   07  SYS-5000-SENTEI-IPN-KINGAKU
                                           PIC  9(04).
      *選定入院特別料金（老人）
               05  SYS-5000-SENTEI-RJN-KINGAKU-X
                                           PIC  X(04).
               05  SYS-5000-SENTEI-RJN-KINGAKU-R
                                           REDEFINES
                   SYS-5000-SENTEI-RJN-KINGAKU-X.
                   07  SYS-5000-SENTEI-RJN-KINGAKU
                                           PIC  9(04).
      *入院会計外泊区分
               05  SYS-5000-NACCT-GAIHAKUKBN
                                           PIC  X(01).
      *標欠減額区分
               05  SYS-5000-HYOUKETUKBN    PIC  X(01).
      *定数超過区分
               05  SYS-5000-TEISUKBN       PIC  X(01).
      *ＡＤＬ入力区分
               05  SYS-5000-ADLINPUTKBN    PIC  X(01).
      *定期請求差額初期表示区分
               05  SYS-5000-TEIKISAGAKUACTKBN
                                           PIC  X(01).
      *入金の取扱い
               05  SYS-5000-NYKNKNRKBN     PIC  X(01).
      *療養病棟（病床）会計表示区分
               05  SYS-5000-RYOYODSPKBN    PIC  X(01).
      *端末ＩＤ
           03  SYS-5000-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  SYS-5000-OPID               PIC  X(16).
      *作成年月日
           03  SYS-5000-CREYMD             PIC  X(08).
      *更新年月日
           03  SYS-5000-UPYMD              PIC  X(08).
      *更新時間
           03  SYS-5000-UPHMS              PIC  X(06).
      *主キー更新用項目
           03  SYS-5000-UPD-KEY.
               05  SYS-5000-UPD-HOSPNUM     PIC 9(2).
               05  SYS-5000-UPD-KANRICD     PIC X(04).
               05  SYS-5000-UPD-KBNCD       PIC X(08).
               05  SYS-5000-UPD-STYUKYMD    PIC X(08).
               05  SYS-5000-UPD-EDYUKYMD    PIC X(08).
