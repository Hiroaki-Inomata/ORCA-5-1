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
      ******************************************************************
      *    ＡＰＩレスポンスＣＬＡＩＭ請求送信取得(xml2)
      *                                      COPY (CPCLAIMACCOUNTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-CLAIM-ACCOUNTV2RES.
           02  CLACRES-CONTEXT           PIC S9(9)   BINARY.
           02  CLACRES-OBJECT            PIC X(36).
           02  CLACRES-MODE              PIC S9(9)   BINARY.
           02  CLACRES-RECORDNAME        PIC X(128).
           02  CLACRES-CLAIM-FRONT.
      *        実施日
               03  CLACRES-INFORMATION-DATE    PIC X(10).
      *        実施時間
               03  CLACRES-INFORMATION-TIME    PIC X(8).
      *        返却コード
               03  CLACRES-API-RESULT          PIC X(2).
      *        返却メッセージ
               03  CLACRES-API-RESULT-MSG      PIC X(100).
      *        患者番号
               03  CLACRES-PATIENTID           PIC X(20).
      *
      * 実施日(CCYY-MM-DDTHH:MM:SS形式)
               03  CLACRES-INFORMATION-HEN     PIC X(19).
      * 患者主ID
               03  CLACRES-PTID1               PIC X(20).
      * 患者主IDのテーブル名
               03  CLACRES-IDTYPE1        PIC X(08).
      * 作成者 個人ID [必須]
               03  CLACRES-CRE001-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               03  CLACRES-CRE001-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               03  CLACRES-CRE001-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               03  CLACRES-CRE001-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               03  CLACRES-CRE001-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               03  CLACRES-CRE001-PREFIX       PIC X(50).
      * 作成者 学位
               03  CLACRES-CRE001-DEGREE       PIC X(50).
      * 作成者 施設情報名
               03  CLACRES-CRE001-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               03  CLACRES-CRE001-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               03  CLACRES-CRE001-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               03  CLACRES-CRE001-DPERTID      PIC X(20).
      * 作成者 住所
               03  CLACRES-CRE001-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               03  CLACRES-CRE001-ADRCLASS     PIC X(10).
      * 作成者 都道府県
               03  CLACRES-CRE001-ADRPREF      PIC X(20).
      * 作成者 市、区、郡
               03  CLACRES-CRE001-ADRCITY      PIC X(20).
      * 作成者 町、村
               03  CLACRES-CRE001-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等
               03  CLACRES-CRE001-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               03  CLACRES-CRE001-ZIP          PIC X(8).
      * 作成者 国コード
               03  CLACRES-CRE001-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               03  CLACRES-CRE001-EMAIL        PIC X(100).
      * 作成者 市外局番
               03  CLACRES-CRE001-PHONEAREA    PIC X(10).
      * 作成者 局番
               03  CLACRES-CRE001-PHONECITY    PIC X(10).
      * 作成者 加入番号
               03  CLACRES-CRE001-PHONENUM     PIC X(10).
      * 作成者 内線番号
               03  CLACRES-CRE001-PHONEEXT     PIC X(10).
      * 作成者 国番号
               03  CLACRES-CRE001-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               03  CLACRES-CRE001-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               03  CLACRES-CRE001-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
               03  CLACRES-CRE001-LICENSE      PIC X(20).
      *## コンスタント値 (<accessRight permit="all" />) const
               03  CLACRES-M-RI401-ACC1             PIC X(50).
      *## コンスタント値 (Data of Amount) const
               03  CLACRES-M-RI401-TITLE            PIC X(20).
      *
      * [点数金額モジュール]
      * 文書ユニークID
               03  CLACRES-UID1                PIC X(36).
      * カルテ電子保存の確定日
               03  CLACRES-CONFIRMDATE1        PIC X(10).
      * 作成者 個人ID [必須]
               03  CLACRES-CRE401-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               03  CLACRES-CRE401-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               03  CLACRES-CRE401-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               03  CLACRES-CRE401-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               03  CLACRES-CRE401-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               03  CLACRES-CRE401-PREFIX       PIC X(50).
      * 作成者 学位
               03  CLACRES-CRE401-DEGREE       PIC X(50).
      * 作成者 施設情報名
               03  CLACRES-CRE401-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               03  CLACRES-CRE401-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               03  CLACRES-CRE401-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               03  CLACRES-CRE401-DPERTID      PIC X(20).
      * 作成者 住所
               03  CLACRES-CRE401-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               03  CLACRES-CRE401-ADRCLASS     PIC X(10).
      * 作成者 都道府県
               03  CLACRES-CRE401-ADRPREF      PIC X(20).
      * 作成者 市、区、郡
               03  CLACRES-CRE401-ADRCITY      PIC X(20).
      * 作成者 町、村
               03  CLACRES-CRE401-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等
               03  CLACRES-CRE401-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               03  CLACRES-CRE401-ZIP          PIC X(8).
      * 作成者 国コード
               03  CLACRES-CRE401-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               03  CLACRES-CRE401-EMAIL        PIC X(100).
      * 作成者 市外局番
               03  CLACRES-CRE401-PHONEAREA    PIC X(10).
      * 作成者 局番
               03  CLACRES-CRE401-PHONECITY    PIC X(10).
      * 作成者 加入番号
               03  CLACRES-CRE401-PHONENUM     PIC X(10).
      * 作成者 内線番号
               03  CLACRES-CRE401-PHONEEXT     PIC X(10).
      * 作成者 国番号
               03  CLACRES-CRE401-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               03  CLACRES-CRE401-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               03  CLACRES-CRE401-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
               03  CLACRES-CRE401-LICENSE      PIC X(20).
      *### コンスタント値 (<extRefs />)                const
               03  CLACRES-M-RX401-I1               PIC X(20).
      *
      * 点数金額モジュールの状態(claimA:status) (accountを設定)
               03  CLACRES-STATUS              PIC X(07).
      *オーダー発行日時
               03  CLACRES-ORDER-TIME          PIC X(19).
      *実施予定日時
               03  CLACRES-APPOINT-TIME        PIC X(19).
      *受付日時
               03  CLACRES-REGIST-TIME         PIC X(19).
      *実施日時
               03  CLACRES-PERFORM-TIME        PIC X(19).
      *会計終了日時
               03  CLACRES-ACCOUNT-TIME        PIC X(19).
      *入外区分 (true OR false)
               03  CLACRES-NYUGAIKBN           PIC X(05).
      *時間外区分
               03  CLACRES-JIKANNGAIKBN        PIC X(01).
      *使用保険ＵＩＤ
               03  CLACRES-HOKEN-UID           PIC X(36).
      *共通医事請求コード体系名
               03  CLACRES-DEFAULT-TABLED      PIC X(100).
      *診療科名称
               03  CLACRES-SRYKA-NAME          PIC X(12).
      * 診療科名称の表記法 (mmlDp:repCode)
               03  CLACRES-SRYKA-REPCODE       PIC X(01).
      * 診療科ID
               03  CLACRES-SRYKA-ID            PIC X(02).
      * 診療科IDの種類 (mmlCm:type)
               03  CLACRES-SRYKA-IDCODE        PIC X(07).
      * 診療病棟名称
               03  CLACRES-SNRBT-NAME          PIC X(12).
      * 診療病棟名称の表記法 (mmlDp:repCode)
               03  CLACRES-SNRBT-REPCODE       PIC X(01).
      * 診療病棟ID
               03  CLACRES-SNRBT-ID            PIC X(02).
      * 診療病棟IDの種類 (mmlCm:type)
               03  CLACRES-SNRBT-IDCODE        PIC X(07).
      * 健康保険種別
               03  CLACRES-HKNSBT              PIC X(20).
      * 健康保険種別コード
               03  CLACRES-HKNSBTCODE          PIC X(2).
      * 健康保険種別コードテーブル名
               03  CLACRES-HKNSBTTBL           PIC X(7).
      *
               03  CLACRES-SRYTBL             OCCURS  30.
      *診療行為区分コード (claimA:classCode)
                   05  CLACRES-SRYCD          PIC  X(03).
      * 診療行為区分コードのテーブル名 (claimA:classCodeId)
                   05  CLACRES-SRYCDTBL       PIC  X(08).
      *診療行為区分名
                   05  CLACRES-SRYNAME        PIC  X(40).
      *合計請求点数(Z編集)
                   05  CLACRES-TENNSUALL      PIC  X(16).
      *合計請求金額(Z編集)
                   05  CLACRES-GINGAKUALL     PIC  X(16).
      *用法
                   05  CLACRES-ADMINISTRATION
                                       PIC  X(50).
      *用法コード
                   05  CLACRES-ADMINCODE      PIC  X(09).
      * 用法コードのテーブル名 (claimA:adminCodeId)
                   05  CLACRES-ADMINCODETBL   PIC  X(08).
      *用法コメント
                   05  CLACRES-ADMMEMO        PIC  X(100).
      *回数又は日数(Z編集)
                   05  CLACRES-KAISU          PIC  X(16).
      *手技料合計点数(Z編集)
                   05  CLACRES-SYUGI          PIC  X(16).
      *材料料合計点数(Z編集)
                   05  CLACRES-ZAIRYOU        PIC  X(16).
      *薬剤料合計点数(Z編集)
                   05  CLACRES-YAKUZAI        PIC  X(16).
      *包括区分包括フラグ(true OR false)
                   05  CLACRES-PPSFLG         PIC  X(05).
      *明細テーブル
                   05  CLACRES-SRYTBL       OCCURS  30.
      *診療種別区分コード(0:手技、1:材料,2:薬剤)
                       07  CLACRES-SRYSYUBETSU
                                       PIC  X(01).
      * 診療種別区分コードのテーブル名 (claimA:subclassCodeId)
                       07  CLACRES-SRYSYUBETSUTBL
                                       PIC  X(08).
      *医事請求コード(S + 診療行為コード)
                       07  CLACRES-IJICD      PIC  X(10).
      *医事請求コード体系名
                       07  CLACRES-TBLID      PIC  X(20).
      *医事請求コード
                       07  CLACRES-ALSCODE    PIC  X(10).
      *医事請求コード体系名
                       07  CLACRES-ALSTBLID   PIC  X(20).
      *医事請求コード名称
                       07  CLACRES-ALSNAME    PIC  X(80).
      *数量(Z編集)
                       07  CLACRES-SURYO      PIC  X(12).
      *数量コード
                       07  CLACRES-SURYOCD    PIC  X(02).
      * 数量コードのテーブル名 (claimA:numberCodeId)
                       07  CLACRES-SURYOCDTBL PIC  X(08).
      *単位
                       07  CLACRES-TANI       PIC  X(20).
      *請求点数(Z編集)
                       07  CLACRES-SEIKYUTEN  PIC  X(16).
      *明細単価(Z編集)
                       07  CLACRES-MEISAITANKA
                                       PIC  X(16).
      *所要時間(PTnHnM,5時間23分→PT5H23M)
                       07  CLACRES-DURATION   PIC  X(19).
      *部位
                       07  CLACRES-LOCATION   PIC  X(50).
      *フィルム
      *フィルムサイズ
                       07  CLACRES-FILMSIZE   PIC  X(50).
      *フィルムサイズコード
                       07  CLACRES-FILMSIZECD
                                              PIC  X(02).
      * フィルムサイズコードのテーブル名 (claimA:numberCodeId)
                       07  CLACRES-FILMSIZECDTBL
                                              PIC  X(08).
      *フィルム分割
                       07  CLACRES-FILMDEV    PIC  X(02).
      *フィルム枚数(Z編集)
                       07  CLACRES-FILMNUM    PIC  X(06).
      *関連手技
                       07  CLACRES-EVENT      PIC  X(50).
      *開始日
                       07  CLACRES-EVENTSTART PIC  X(10).
      *終了日
                       07  CLACRES-EVENTEND   PIC  X(10).
      *メモ
                       07  CLACRES-SRYMEMO    PIC  X(100).
      *括りメモ
                   05  CLACRES-MEMO           PIC  X(1000).

