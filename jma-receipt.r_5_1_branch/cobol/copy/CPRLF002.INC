      ************************************************************
      *    ＣＬＡＩＭ００２           COPY (CPRLF002.INC)        *
      *    点数金額モジュール用                                  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/05/28  診療、明細テーブル 20 → 30
      *                                     医事請求コード名称 X(80)
      ************************************************************
      *01  LF02-REC.
      * [MMLヘッダ]
      * 患者主ID
           03  LF02-PTID1               PIC X(20).
      * 患者主IDのテーブル名
           03  LF02-IDTYPE1             PIC X(08).
      * 作成者 個人ID [必須]
           03  LF02-CRE001-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF02-CRE001-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF02-CRE001-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF02-CRE001-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF02-CRE001-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF02-CRE001-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF02-CRE001-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF02-CRE001-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF02-CRE001-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF02-CRE001-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF02-CRE001-DPERTID      PIC X(20).
      * 作成者 住所
           03  LF02-CRE001-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF02-CRE001-ADRCLASS     PIC X(10).
      * 作成者 都道府県
           03  LF02-CRE001-ADRPREF      PIC X(20).
      * 作成者 市、区、郡
           03  LF02-CRE001-ADRCITY      PIC X(20).
      * 作成者 町、村
           03  LF02-CRE001-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等
           03  LF02-CRE001-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF02-CRE001-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF02-CRE001-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF02-CRE001-EMAIL        PIC X(100).
      * 作成者 市外局番
           03  LF02-CRE001-PHONEAREA    PIC X(10).
      * 作成者 局番
           03  LF02-CRE001-PHONECITY    PIC X(10).
      * 作成者 加入番号
           03  LF02-CRE001-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF02-CRE001-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF02-CRE001-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF02-CRE001-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF02-CRE001-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
           03  LF02-CRE001-LICENSE      PIC X(20).
      * [点数金額モジュール]
      * 文書ユニークID
           03  LF02-UID1                PIC X(36).
      * カルテ電子保存の確定日
           03  LF02-CONFIRMDATE1        PIC X(10).
      * 作成者 個人ID [必須]
           03  LF02-CRE401-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF02-CRE401-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF02-CRE401-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF02-CRE401-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF02-CRE401-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF02-CRE401-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF02-CRE401-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF02-CRE401-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF02-CRE401-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF02-CRE401-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF02-CRE401-DPERTID      PIC X(20).
      * 作成者 住所
           03  LF02-CRE401-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF02-CRE401-ADRCLASS     PIC X(10).
      * 作成者 都道府県
           03  LF02-CRE401-ADRPREF      PIC X(20).
      * 作成者 市、区、郡
           03  LF02-CRE401-ADRCITY      PIC X(20).
      * 作成者 町、村
           03  LF02-CRE401-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等
           03  LF02-CRE401-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF02-CRE401-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF02-CRE401-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF02-CRE401-EMAIL        PIC X(100).
      * 作成者 市外局番
           03  LF02-CRE401-PHONEAREA    PIC X(10).
      * 作成者 局番
           03  LF02-CRE401-PHONECITY    PIC X(10).
      * 作成者 加入番号
           03  LF02-CRE401-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF02-CRE401-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF02-CRE401-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF02-CRE401-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF02-CRE401-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
           03  LF02-CRE401-LICENSE      PIC X(20).
      * 点数金額モジュールの状態(claimA:status) (accountを設定)
           03  LF02-STATUS              PIC X(07).
      *オーダー発行日時
           03  LF02-ORDER-TIME          PIC X(19).
      *実施予定日時
           03  LF02-APPOINT-TIME        PIC X(19).
      *受付日時
           03  LF02-REGIST-TIME         PIC X(19).
      *実施日時
           03  LF02-PERFORM-TIME        PIC X(19).
      *会計終了日時
           03  LF02-ACCOUNT-TIME        PIC X(19).
      *入外区分 (true OR false)
           03  LF02-NYUGAIKBN           PIC X(05).
      *時間外区分
           03  LF02-JIKANNGAIKBN        PIC X(01).
      *使用保険ＵＩＤ
           03  LF02-HOKEN-UID           PIC X(36).
      *共通医事請求コード体系名
           03  LF02-DEFAULT-TABLED      PIC X(100).
      *診療科名称
           03  LF02-SRYKA-NAME          PIC X(12).
      * 診療科名称の表記法 (mmlDp:repCode)
           03  LF02-SRYKA-REPCODE       PIC X(01).
      * 診療科ID
           03  LF02-SRYKA-ID            PIC X(02).
      * 診療科IDの種類 (mmlCm:type)
           03  LF02-SRYKA-IDCODE        PIC X(07).
      * 診療病棟名称
           03  LF02-SNRBT-NAME          PIC X(12).
      * 診療病棟名称の表記法 (mmlDp:repCode)
           03  LF02-SNRBT-REPCODE       PIC X(01).
      * 診療病棟ID
           03  LF02-SNRBT-ID            PIC X(02).
      * 診療病棟IDの種類 (mmlCm:type)
           03  LF02-SNRBT-IDCODE        PIC X(07).
      * 健康保険種別
           03  LF02-HKNSBT              PIC X(20).
      * 健康保険種別コード
           03  LF02-HKNSBTCODE          PIC X(2).
      * 健康保険種別コードテーブル名
           03  LF02-HKNSBTTBL           PIC X(7).
      *
           03  LF02-SRYTBL             OCCURS  30.
      *診療行為区分コード (claimA:classCode)
               05  LF02-SRYCD          PIC  X(03).
      * 診療行為区分コードのテーブル名 (claimA:classCodeId)
               05  LF02-SRYCDTBL       PIC  X(08).
      *診療行為区分名
               05  LF02-SRYNAME        PIC  X(40).
      *合計請求点数(Z編集)
               05  LF02-TENNSUALL      PIC  X(16).
      *合計請求金額(Z編集)
               05  LF02-GINGAKUALL     PIC  X(16).
      *用法
               05  LF02-ADMINISTRATION
                                       PIC  X(50).
      *用法コード
               05  LF02-ADMINCODE      PIC  X(09).
      * 用法コードのテーブル名 (claimA:adminCodeId)
               05  LF02-ADMINCODETBL   PIC  X(08).
      *用法コメント
               05  LF02-ADMMEMO        PIC  X(100).
      *回数又は日数(Z編集)
               05  LF02-KAISU          PIC  X(16).
      *手技料合計点数(Z編集)
               05  LF02-SYUGI          PIC  X(16).
      *材料料合計点数(Z編集)
               05  LF02-ZAIRYOU        PIC  X(16).
      *薬剤料合計点数(Z編集)
               05  LF02-YAKUZAI        PIC  X(16).
      *包括区分包括フラグ(true OR false)
               05  LF02-PPSFLG         PIC  X(05).
      *明細テーブル
               05  LF02-SRYTBL       OCCURS  30.
      *診療種別区分コード(0:手技、1:材料,2:薬剤)
                   07  LF02-SRYSYUBETSU
                                       PIC  X(01).
      * 診療種別区分コードのテーブル名 (claimA:subclassCodeId)
                   07  LF02-SRYSYUBETSUTBL
                                       PIC  X(08).
      *医事請求コード(S + 診療行為コード)
                   07  LF02-IJICD      PIC  X(10).
      *医事請求コード体系名
                   07  LF02-TBLID      PIC  X(20).
      *医事請求コード
                   07  LF02-ALSCODE    PIC  X(10).
      *医事請求コード体系名
                   07  LF02-ALSTBLID   PIC  X(20).
      *医事請求コード名称
      *@@@@@       07  LF02-ALSNAME    PIC  X(40).
                   07  LF02-ALSNAME    PIC  X(80).
      *数量(Z編集)
                   07  LF02-SURYO      PIC  X(12).
      *数量コード
                   07  LF02-SURYOCD    PIC  X(02).
      * 数量コードのテーブル名 (claimA:numberCodeId)
                   07  LF02-SURYOCDTBL PIC  X(08).
      *単位
                   07  LF02-TANI       PIC  X(20).
      *請求点数(Z編集)
                   07  LF02-SEIKYUTEN  PIC  X(16).
      *明細単価(Z編集)
                   07  LF02-MEISAITANKA
                                       PIC  X(16).
      *所要時間(PTnHnM,5時間23分→PT5H23M)
                   07  LF02-DURATION   PIC  X(19).
      *部位
                   07  LF02-LOCATION   PIC  X(50).
      *フィルム
                   07  LF02-FILM       OCCURS  1.
      *フィルムサイズ
                     09  LF02-FILMSIZE PIC  X(50).
      *フィルムサイズコード
                     09  LF02-FILMSIZECD
                                       PIC  X(02).
      * フィルムサイズコードのテーブル名 (claimA:numberCodeId)
                     09  LF02-FILMSIZECDTBL
                                       PIC  X(08).
      *フィルム分割
                     09  LF02-FILMDEV  PIC  X(02).
      *フィルム枚数(Z編集)
                     09  LF02-FILMNUM  PIC  X(06).
      *関連手技
                   07  LF02-EVENT      PIC  X(50).
      *開始日
                   07  LF02-EVENTSTART PIC  X(10).
      *終了日
                   07  LF02-EVENTEND   PIC  X(10).
      *メモ
                   07  LF02-SRYMEMO    PIC  X(100).
      *括りメモ
               05  LF02-MEMO           PIC  X(1000).
      *    03  LF02-MEMO               PIC  X(100).
