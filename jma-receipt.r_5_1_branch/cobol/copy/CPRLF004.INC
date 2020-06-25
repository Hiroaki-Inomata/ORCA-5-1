      ************************************************************
      *    ＣＬＡＩＭ００４           COPY (CPRLF004.INC)        *
      *    処方モジュール用                                      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    03/09/30  診断履歴モジュールの変更
      *  02.05.00    NACL-竹田    04/10/20  ドクターコード追加
      *  04.07.00    NACL-多々納  11/10/XX  UID追加
      *  04.07.00    NACL-多々納  11/11/02  同日再入院区分追加
      *  04.07.00    NACL-多々納  13/08/XX  API病名補足コメント追加
      *  04.08.00    NACL-多々納  17/03/XX  API内服１種類区分追加
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      ************************************************************
      *01  LF04-REC.
      *
      *     03  LF04-SRYCD               PIC X(02).
      *
      *
      * [MMLヘッダ]
      * 患者主ID
           03  LF04-PTID1               PIC X(20).
      * 患者主IDのテーブル名
           03  LF04-IDTYPE1             PIC X(08).
      * 作成者 個人ID [必須]
           03  LF04-CRE001-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF04-CRE001-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF04-CRE001-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF04-CRE001-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF04-CRE001-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF04-CRE001-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF04-CRE001-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF04-CRE001-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF04-CRE001-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF04-CRE001-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF04-CRE001-DPERTID      PIC X(20).
      * 作成者 住所
           03  LF04-CRE001-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF04-CRE001-ADRCLASS     PIC X(10).
      * 作成者 都道府県
           03  LF04-CRE001-ADRPREF      PIC X(20).
      * 作成者 市、区、郡
           03  LF04-CRE001-ADRCITY      PIC X(20).
      * 作成者 町、村
           03  LF04-CRE001-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等
           03  LF04-CRE001-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF04-CRE001-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF04-CRE001-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF04-CRE001-EMAIL        PIC X(100).
      * 作成者 市外局番
           03  LF04-CRE001-PHONEAREA    PIC X(10).
      * 作成者 局番
           03  LF04-CRE001-PHONECITY    PIC X(10).
      * 作成者 加入番号
           03  LF04-CRE001-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF04-CRE001-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF04-CRE001-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF04-CRE001-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF04-CRE001-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
           03  LF04-CRE001-LICENSE      PIC X(20).
      *
      *
      * [患者情報]
      * 文書ユニークID
           03  LF04-UID1                PIC X(36).
      * カルテ電子保存の確定日
           03  LF04-CONFIRMDATE1        PIC X(10).
      * <ID情報>
      * (患者主ID)
      * 患者主ID
           03  LF04-PTID2               PIC X(20).
      * IDの種類コード (mmlCm:type)
           03  LF04-PTID2-TYPE          PIC X(8).
      * IDの種類コードのテーブル名 (mmlCm:tableId) (MML0024)
           03  LF04-PTID2-TYPETBL       PIC X(7).
      * チェックディジット値 (mmlCm:checkDigit)
           03  LF04-PTID2-CD            PIC X(2).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
           03  LF04-PTID2-CDSCHEMA      PIC X(3).
      * (その他のID)
      * IDの種類コード (mmlPi:type)
           03  LF04-OTHERID-TYPE        PIC X(20).
      * その他のID
           03  LF04-OTHERID1            PIC X(20).
      * IDの種類コード (mmlCm:type)
           03  LF04-OTHERID1-TYPE       PIC X(8).
      * IDの種類コードのテーブル名 (mmlCm:tableId)
           03  LF04-OTHERID1-TYPETBL    PIC X(7).
      * チェックディジット値 (mmlCm:checkDigit)
           03  LF04-OTHERID1-CD         PIC X(2).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
           03  LF04-OTHERID1-CDSHEMA    PIC X(3).
      * <氏名情報>
      * 表記法 (mmlNm:repCode)
           03  LF04-PATNAME-REPCODE     PIC X(1).
      * 表記法のテーブル名 (mmlNm:tableId) (MML0025)
           03  LF04-PATNAME-TBL         PIC X(7).
      * 姓
           03  LF04-PATNAME-FAMILY      PIC X(50).
      * 名
           03  LF04-PATNAME-GIVEN       PIC X(50).
      * ミドルネーム
           03  LF04-PATNAME-MIDDLE      PIC X(50).
      * フルネーム
           03  LF04-PATNAME-FULLNAME    PIC X(100).
      * 肩書きなど
           03  LF04-PATNAME-PREFIX      PIC X(50).
      * 学位
           03  LF04-PATNAME-DEGREE      PIC X(50).
      * <その他患者情報 1>
      * 生年月日
           03  LF04-PATNAME-BIRTHDAY    PIC X(10).
      * 性別
           03  LF04-PATNAME-SEX         PIC X(7).
      * 国籍コード
           03  LF04-PATNAME-NATIONALITY PIC X(10).
      * 第２国籍コード (mmlPi:subtype)
           03  LF04-PATNAME-SUBNATIONAL PIC X(10).
      * 婚姻状態
           03  LF04-PATNAME-MARITAL     PIC X(10).
      * <住所情報>
           03  LF04-PATADR-TBL          OCCURS  4.
      * 表記法 (mmlAd:repCode)
               05  LF04-PATADR-REPCODE  PIC  X(1).
      * 表記法のテーブル名 (mmlAd:tableId)
               05  LF04-PATADR-TABLEID  PIC  X(7).
      * 住所の種類コード (mmlAd:addressClass)
               05  LF04-PATADR-ADRCLASS PIC  X(10).
      * 都道府県
               05  LF04-PATADR-PREF     PIC  X(20).
      * 市、区、郡
               05  LF04-PATADR-CITY     PIC  X(20).
      * 町、村
               05  LF04-PATADR-TOWN     PIC  X(20).
      * 番地、丁目、その他
               05  LF04-PATADR-HOMENUM  PIC  X(200).
      * 一連住所
               05  LF04-PATADR-FULLNAME PIC  X(400).
      * 郵便番号
               05  LF04-PATADR-ZIP      PIC  X(8).
      * 国コード
               05  LF04-PATADR-COUNTRY  PIC  X(10).
      * <電子メールアドレス>
      * 電子メールアドレス
           03  LF04-PAT-EMAIL           PIC X(100).
      * <電話番号>
      * 装置の種類コード (mmlPh:telEquipType)
           03  LF04-PAT-PHONEEQUIP      PIC X(10).
      * 市外局番
           03  LF04-PAT-PHONEAREA       PIC X(10).
      * 局番
           03  LF04-PAT-PHONECITY       PIC X(10).
      * 加入番号
           03  LF04-PAT-PHONENUM        PIC X(10).
      * 内線番号
           03  LF04-PAT-PHONEEXT        PIC X(10).
      * 国番号
           03  LF04-PAT-PHONECOUNTRY    PIC X(10).
      * 使用時間帯などのメモ
           03  LF04-PAT-PHONEMEMO       PIC X(100).
      * <その他患者情報 2>
      * 会計番号
           03  LF04-PAT-ACCOUNTNUM      PIC X(50).
      * 社会番号
           03  LF04-PAT-SOCIALNUM       PIC X(50).
      * 死亡フラグ
           03  LF04-PAT-DEATHFLG        PIC X(5).
      * 死亡日／日時 (mmlPi:date)
           03  LF04-PAT-DEATHDATE       PIC X(16).
      *
      *
      * [健康保険モジュール]
           03  LF04-HKNTBL              OCCURS  2.
      * 文書ユニークID
               05  LF04-UID2                PIC X(36).
      * カルテ電子保存の確定日
               05  LF04-CONFIRMDATE2        PIC X(10).
      * 国タイプ (mmlHi:countryType)
               05  LF04-HKN-COUNTRYTYPE     PIC X(10).
      * 健康保険種別
               05  LF04-HKN-HKNSBT          PIC X(20).
      * 保険種別コード (mmlHi:ClassCode)
               05  LF04-HKN-HKNSBTCODE      PIC X(2).
      * 保険種別コードのテーブル名 (mmlHi:tableId)
               05  LF04-HKN-HKNSBTTBL       PIC X(7).
      * 健康保険番号
               05  LF04-HKN-HKNJANUM        PIC  X(08).
      * 被保険者記号
               05  LF04-HKN-KIGO            PIC  X(80).
      * 被保険者番号
               05  LF04-HKN-NUM             PIC  X(80).
      * 本人家族区分
               05  LF04-HKN-HONKZKKBN       PIC  X(05).
      * <被保険者情報>
      * 表記法 (mmlNm:repCode)
               05  LF04-HIHKNNAME-REPCODE   PIC X(1).
      * 表記法のテーブル名 (mmlNm:tableId) (MML0025)
               05  LF04-HIHKNNAME-TBL       PIC X(7).
      * 姓
               05  LF04-HIHKNNAME-FAMILY    PIC X(50).
      * 名
               05  LF04-HIHKNNAME-GIVEN     PIC X(50).
      * ミドルネーム
               05  LF04-HIHKNNAME-MIDDLE    PIC X(50).
      * フルネーム
               05  LF04-HIHKNNAME-FULLNAME  PIC X(100).
      * 肩書きなど
               05  LF04-HIHKNNAME-PREFIX    PIC X(50).
      * 学位
               05  LF04-HIHKNNAME-DEGREE    PIC X(50).
      * <被保険者住所>
      * 表記法 (mmlAd:repCode)
               05  LF04-HIHKNADR-REPCODE    PIC  X(1).
      * 表記法のテーブル名 (mmlAd:tableId)
               05  LF04-HIHKNADR-TABLEID    PIC  X(7).
      * 住所の種類コード (mmlAd:addressClass)
               05  LF04-HIHKNADR-ADRCLASS   PIC  X(10).
      * 都道府県
               05  LF04-HIHKNADR-PREF       PIC  X(20).
      * 市、区、郡
               05  LF04-HIHKNADR-CITY       PIC  X(20).
      * 町、村
               05  LF04-HIHKNADR-TOWN       PIC  X(20).
      * 番地、丁目、その他
               05  LF04-HIHKNADR-HOMENUM    PIC  X(200).
      * 一連住所
               05  LF04-HIHKNADR-FULLNAME   PIC  X(400).
      * 郵便番号
               05  LF04-HIHKNADR-ZIP        PIC  X(8).
      * 国コード
               05  LF04-HIHKNADR-COUNTRY    PIC  X(10).
      * <被保険者電話番号>
      * 装置の種類コード (mmltelEquipType)
               05  LF04-HIHKNPHONE-EQUIP    PIC X(10).
      * 市外局番
               05  LF04-HIHKNPHONE-AREA     PIC X(10).
      * 局番
               05  LF04-HIHKNPHONE-CITY     PIC X(10).
      * 加入番号
               05  LF04-HIHKNPHONE-NUM      PIC X(10).
      * 内線番号
               05  LF04-HIHKNPHONE-EXT      PIC X(10).
      * 国番号
               05  LF04-HIHKNPHONE-COUNTRY  PIC X(10).
      * 使用時間帯などのメモ
               05  LF04-HIHKNPHONE-MEMO     PIC X(100).
      * <その他健康保険情報>
      * その他継続適応疾患名
               05  LF04-HKN-OTHERKEIZOKU    PIC  X(50).
      * 適用開始年月日・開始日 (交付年月日)
               05  LF04-HKN-TEKSTYMD        PIC  X(10).
      * 適用終了年月日・有効期限
               05  LF04-HKN-TEKEDYMD        PIC  X(10).
      * 入院時の負担率
               05  LF04-HKN-NYUINFUTAN      PIC  X(10).
      * 外来時の負担率
               05  LF04-HKN-GAIRAIFUTAN     PIC  X(10).
      * <保険者施設情報>
      * 表記法 (mmlFc:repCode)
               05  LF04-HKNSYASIS-REPCODE   PIC X(01).
      * 表記法のテーブル名 (mmlFc:tableId)
               05  LF04-HKNSYASIS-REPCODETBL
                                            PIC X(07).
      * 施設名称
               05  LF04-HKNSYASIS-NAME      PIC X(50).
      * 施設ID
               05  LF04-HKNSYASIS-ID        PIC X(20).
      * IDの種類コード(mmlCm:type)
               05  LF04-HKNSYASIS-IDCODE    PIC X(10).
      * IDの種類コードのテーブル名 (mmlCm:tableId) (MML0027)
               05  LF04-HKNSYASIS-IDCODETBL PIC X(07).
      * チェックディジット値 (mmlCm:checkDigit)
               05  LF04-HKNSYASIS-CD        PIC X(02).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
               05  LF04-HKNSYASIS-CDSCHEMA  PIC X(03).
      * <保険者住所>
      * 表記法 (mmlAd:repCode)
               05  LF04-HKNSYAADR-REPCODE   PIC  X(1).
      * 表記法のテーブル名 (mmlAd:tableId)
               05  LF04-HKNSYAADR-TABLEID   PIC  X(7).
      * 住所の種類コード (mmlAd:addressClass)
               05  LF04-HKNSYAADR-ADRCLASS  PIC  X(10).
      * 都道府県
               05  LF04-HKNSYAADR-PREF      PIC  X(20).
      * 市、区、郡
               05  LF04-HKNSYAADR-CITY      PIC  X(20).
      * 町、村
               05  LF04-HKNSYAADR-TOWN      PIC  X(20).
      * 番地、丁目、その他
               05  LF04-HKNSYAADR-HOMENUM   PIC  X(200).
      * 一連住所
               05  LF04-HKNSYAADR-FULLNAME  PIC  X(400).
      * 郵便番号
               05  LF04-HKNSYAADR-ZIP       PIC  X(8).
      * 国コード
               05  LF04-HKNSYAADR-COUNTRY   PIC  X(10).
      * <保険者電話番号>
      * 装置の種類コード (mmltelEquipType)
               05  LF04-HKNSYAPHONE-EQUIP   PIC X(10).
      * 市外局番
               05  LF04-HKNSYAPHONE-AREA    PIC X(10).
      * 局番
               05  LF04-HKNSYAPHONE-CITY    PIC X(10).
      * 加入番号
               05  LF04-HKNSYAPHONE-NUM     PIC X(10).
      * 内線番号
               05  LF04-HKNSYAPHONE-EXT     PIC X(10).
      * 国番号
               05  LF04-HKNSYAPHONE-COUNTRY PIC X(10).
      * 使用時間帯などのメモ
               05  LF04-HKNSYAPHONE-MEMO    PIC X(100).
      * <事業所施設情報>
      * 表記法 (mmlFc:repCode)
               05  LF04-JGYSYASIS-REPCODE   PIC X(01).
      * 表記法のテーブル名 (mmlFc:tableId)
               05  LF04-JGYSYASIS-REPCODETBL
                                            PIC X(07).
      * 施設名称
               05  LF04-JGYSYASIS-NAME      PIC X(50).
      * 施設ID
               05  LF04-JGYSYASIS-ID        PIC X(20).
      * IDの種類コード(mmlCm:type)
               05  LF04-JGYSYASIS-IDCODE    PIC X(10).
      * IDの種類コードのテーブル名 (mmlCm:tableId) (MML0027)
               05  LF04-JGYSYASIS-IDCODETBL PIC X(07).
      * チェックディジット値 (mmlCm:checkDigit)
               05  LF04-JGYSYASIS-CD        PIC X(02).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
               05  LF04-JGYSYASIS-CDSCHEMA  PIC X(03).
      * <事業所住所>
      * 表記法 (mmlAd:repCode)
               05  LF04-JGYSYAADR-REPCODE   PIC  X(1).
      * 表記法のテーブル名 (mmlAd:tableId)
               05  LF04-JGYSYAADR-TABLEID   PIC  X(7).
      * 住所の種類コード (mmlAd:addressClass)
               05  LF04-JGYSYAADR-ADRCLASS  PIC  X(10).
      * 都道府県
               05  LF04-JGYSYAADR-PREF      PIC  X(20).
      * 市、区、郡
               05  LF04-JGYSYAADR-CITY      PIC  X(20).
      * 町、村
               05  LF04-JGYSYAADR-TOWN      PIC  X(20).
      * 番地、丁目、その他
               05  LF04-JGYSYAADR-HOMENUM   PIC  X(200).
      * 一連住所
               05  LF04-JGYSYAADR-FULLNAME  PIC  X(400).
      * 郵便番号
               05  LF04-JGYSYAADR-ZIP       PIC  X(8).
      * 国コード
               05  LF04-JGYSYAADR-COUNTRY   PIC  X(10).
      * <事業所電話番号>
      * 装置の種類コード (mmltelEquipType)
               05  LF04-JGYSYAPHONE-EQUIP   PIC X(10).
      * 市外局番
               05  LF04-JGYSYAPHONE-AREA    PIC X(10).
      * 局番
               05  LF04-JGYSYAPHONE-CITY    PIC X(10).
      * 加入番号
               05  LF04-JGYSYAPHONE-NUM     PIC X(10).
      * 内線番号
               05  LF04-JGYSYAPHONE-EXT     PIC X(10).
      * 国番号
               05  LF04-JGYSYAPHONE-COUNTRY PIC X(10).
      * 使用時間帯などのメモ
               05  LF04-JGYSYAPHONE-MEMO    PIC X(100).
      * <公費負担医療情報>
               05  LF04-KOHINF       OCCURS  6.
      * 複数公費の優先順位 (mmlHi:priority)
                   07  LF04-HKNKOH-PRIORITY     PIC  X(01).
      * 公費負担名称
                   07  LF04-HKNKOH-PROVIDER     PIC  X(20).
      * 負担者番号
                   07  LF04-HKNKOH-FTNJANUM     PIC  X(08).
      * 受給者番号
                   07  LF04-HKNKOH-JKYSNUM      PIC  X(20).
      * 適用開始年月日・開始日
                   07  LF04-HKNKOH-TTEKSTYMD    PIC  X(10).
      * 適用終了年月日・有効期限
                   07  LF04-HKNKOH-TTEKEDYMD    PIC  X(10).
      * 負担率または負担金 (mmlHi:ratioType)
                   07  LF04-HKNKOH-RATIOTYPE    PIC  X(05).
      * 負担率または負担金
                   07  LF04-HKNKOH-RATIO        PIC  X(10).
      *
      *
      * [診断履歴モジュール]
           03  LF04-SNDRRKTBL           OCCURS  50.
      * 文書ユニークID
               05  LF04-UID3                PIC X(36).
      * カルテ電子保存の確定日
               05  LF04-CONFIRMDATE3        PIC X(10).
      * 疾患コード (mmlRd:code)
      *********05  LF04-SND-SPICOD1         PIC X(10).
               05  LF04-SND-SPICOD1         PIC X(50).
      * 疾患コード体系名 (mmlRd:system)
               05  LF04-SND-SPICODTBL1      PIC X(15).
      * 疾患名
      *********05  LF04-SND-SPINAME1        PIC X(50).
               05  LF04-SND-SPINAME1        PIC X(80).
      * <疾患要素>
      *********05  LF04-SNDSPITBL           OCCURS  3.
               05  LF04-SNDSPITBL           OCCURS  6.
      * 疾患コード (mmlRd:code)
                   07  LF04-SNDSPITBL-SPICOD2     PIC X(10).
      * 疾患コード体系名 (mmlRd:system)
                   07  LF04-SNDSPITBL-SPICODTBL2  PIC X(15).
      * 疾患名
                   07  LF04-SNDSPITBL-SPINAME2    PIC X(20).
      * <分類>
               05  LF04-SNDBNRTBL           OCCURS  4.
      * 分類の種類コード (mmlRd:tableId)
                   07  LF04-SNDBNRTBL-BNRTBL1         PIC X(07).
      * 分類名
                   07  LF04-SNDBNRTBL-BNRNAME1        PIC X(30).
      * <その他診断履歴情報>
      * 疾患開始日
               05  LF04-SND-SKNSTARTDATE    PIC X(10).
      * 疾患終了日
               05  LF04-SND-SKNENDDATE      PIC X(10).
      * 転帰
               05  LF04-SND-TENKI           PIC X(20).
      * 疾患の初診日
               05  LF04-SND-SKNSHOSHINDATE  PIC X(10).
      * 疾患な関する保険の健康保険モジュールのUID (単独タグ) (mmlRd:uid)
               05  LF04-SND-SKNHLTUID       PIC X(36).
      * 入外区分 (mmlRd:InOutPatient)
               05  LF04-SND-NYUGAIKBN       PIC X(20).
      *
      *
      * [予約請求(Claim)モジュール]
      * 文書ユニークID
           03  LF04-UID8                PIC X(36).
      * カルテ電子保存の確定日
           03  LF04-CONFIRMDATE8        PIC X(10).
      * ドクターコード
           03  LF04-DOCCD               PIC X(10).
      * 予約請求モジュールの状態 (claim:status)
           03  LF04-STATUS              PIC X(07).
      * オーダー発行日時 (claim:orderTime)
           03  LF04-ORDER-TIME         PIC  X(19).
      *実施予定日時 (claim:appointTime)
           03  LF04-APPOINT-TIME        PIC X(19).
      * 受付日時 (claim:registTime)
           03  LF04-REGIST-TIME         PIC X(19).
      *実施日時 (claim:performTime)
           03  LF04-PERFORM-TIME        PIC X(19).
      * 入院外来区分 (true OR false) (claim:admitFlag)
           03  LF04-NYUGAIKBN           PIC X(05).
      *時間外区分 (claim:timeClass)
           03  LF04-JIKANNGAIKBN        PIC X(01).
      * 使用保険ＵＩＤ (claim:insuranceUid)
           03  LF04-HOKEN-UID           PIC X(36).
      * 共通医事請求コード体系
           03  LF04-DEFAULT-TABLED      PIC X(100).
      * <予約情報>
      * 予約
           03  LF04-YYK                PIC  X(20).
      * 予約コード (claim:appCode)
           03  LF04-YYKCOD             PIC  X(14).
      * 予約コードのテーブル名 (claim:appCodeId)
           03  LF04-YYKCODTBL          PIC  X(8).
      * 予約メモ
           03  LF04-YYKMEMO            PIC  X(100).
      * <健康保険種別>
      * 健康保険種別
           03  LF04-HKNSBT              PIC X(20).
      * 健康保険種別コード (mmlHi:ClassCode)
           03  LF04-HKNSBTCODE          PIC X(2).
      * 健康保険種別コードテーブル名 (mmlHi:tableId) (MML0031)
           03  LF04-HKNSBTTBL           PIC X(7).
      * <診療科実施情報>
      * 診療科名称
           03  LF04-SRYKA-NAME          PIC X(12).
      * 表記法 (mmlDp:repCode)
           03  LF04-SRYKA-REPCODE       PIC X(01).
      * 表記法のテーブル名 (mmlDp:tableId) (MML0025)
           03  LF04-SRYKA-REPCODETBL    PIC X(07).
      * 診療科ID
           03  LF04-SRYKA-ID            PIC X(02).
      * IDの種類コード (mmlCm:type)
           03  LF04-SRYKA-IDCODE        PIC X(07).
      * IDの種類コードのテーブル名 (mmlCm:tableId) (MML0029)
           03  LF04-SRYKA-IDCODETBL     PIC X(07).
      * チェックディジット値 (mmlCm:checkDigit)
           03  LF04-SRYKA-CD            PIC X(02).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
           03  LF04-SRYKA-CDSCHEMA      PIC X(03).
      * <診療実施病棟情報>
      * 診療実施病棟名称
           03  LF04-SNRBT-NAME          PIC X(12).
      * 表記法 (mmlDp:repCode)
           03  LF04-SNRBT-REPCODE       PIC X(01).
      * 表記法のテーブル名 (mmlDp:tableId) (MML0025)
           03  LF04-SNRBT-REPCODETBL    PIC X(07).
      * 診療病棟ID
           03  LF04-SNRBT-ID            PIC X(02).
      * IDの種類コード (mmlCm:type)
           03  LF04-SNRBT-IDCODE        PIC X(07).
      * IDの種類コードのテーブル名 (mmlCm:tableId) (MML0029)
           03  LF04-SNRBT-IDCODETBL     PIC X(07).
      * チェックディジット値 (mmlCm:checkDigit)
           03  LF04-SNRBT-CD            PIC X(02).
      * チェックディジット方式 (mmlCm:checkDigitSchema)
           03  LF04-SNRBT-CDSCHEMA      PIC X(03).
      * <診療括り 1>
           03  LF04-SRYTBL-G.
             04  LF04-SRYTBL             OCCURS  40.
      * 診療行為区分コード (claim:classCode)
               05  LF04-SRYCD          PIC  X(03).
      * 診療行為区分コードのテーブル名 (claim:classCodeId) (Claim002)
               05  LF04-SRYCDTBL       PIC  X(08).
      * 診療行為区分名
               05  LF04-SRYNAME        PIC  X(40).
      * 用法
               05  LF04-ADMINISTRATION
                                       PIC  X(50).
      * 用法コード (claim:adminCode)
               05  LF04-ADMINCODE      PIC  X(09).
      * 用法コードのテーブル名 (claim:adminCodeId) (Claim006)
               05  LF04-ADMINCODETBL   PIC  X(08).
      * 用法コメント
               05  LF04-ADMMEMO        PIC  X(100).
      * 回数もしくは日数(Z編集)
               05  LF04-KAISU          PIC  X(50).
      * <明細テーブル(診療項目 1)>
               05  LF04-SRYTBL1       OCCURS  40.
      * 診療種別区分コード(0:手技、1:材料,2:薬剤) (claim:subclassCode)
                   07  LF04-SRYSYUBETSU
                                       PIC  X(01).
      * 診療種別区分コードのテーブル名 (claim:subclassCodeId) (Claim003)
                   07  LF04-SRYSYUBETSUTBL
                                       PIC  X(08).
      * 医事請求コード(S + 診療行為コード) (claim:code)
                   07  LF04-IJICD      PIC  X(10).
      * 医事請求コードのテーブル名 (claim:tableId)
                   07  LF04-TBLID      PIC  X(20).
      * 第２医事請求コード (claim:aliasCode)
                   07  LF04-ALSCODE    PIC  X(10).
      * 第２医事請求コードのテーブル名 (claim:aliasTableId)
                   07  LF04-ALSTBLID   PIC  X(20).
      * 医事請求名称
      *R02.4       07  LF04-ALSNAME    PIC  X(80).
                   07  LF04-ALSNAME    PIC  X(140).
      * 数量(Z編集)
      *****        07  LF04-SURYO      PIC  X(12).
                   07  LF04-SURYO      PIC  X(40).
      * 数量コード (claim:numberCode)
                   07  LF04-SURYOCD    PIC  X(02).
      * 数量コードのテーブル名 (claim:numberCodeId) (Claim004)
                   07  LF04-SURYOCDTBL PIC  X(08).
      * 単位 (claim:unit)
                   07  LF04-TANI       PIC  X(20).
      * 所要時間(PTnHnM,5時間23分→PT5H23M)
                   07  LF04-DURATION   PIC  X(19).
      * 部位
                   07  LF04-LOCATION   PIC  X(50).
      * <フィルム>
                   07  LF04-FILM       OCCURS  6.
      * フィルムサイズ
                     09  LF04-FILMSIZE PIC  X(50).
      * フィルムサイズコード (claim:sizeCode)
                     09  LF04-FILMSIZECD
                                       PIC  X(02).
      * フィルムサイズコードのテーブル名 (claim:sizeCodeId)
                     09  LF04-FILMSIZECDTBL
                                       PIC  X(08).
      * フィルム分割 (claim:filmDivision)
                     09  LF04-FILMDEV  PIC  X(02).
      * フィルム枚数(Z編集)
                     09  LF04-FILMNUM  PIC  X(06).
      * <診療項目 2>
      * 関連手技
                   07  LF04-EVENT      PIC  X(50).
      * 関連手技 開始日 (claim:eventStart)
                   07  LF04-EVENTSTART PIC  X(10).
      * 関連手技 終了日 (claim:eventEnd)
                   07  LF04-EVENTEND   PIC  X(10).
      * 診療項目メモ
                   07  LF04-SRYMEMO    PIC  X(100).
      * <診療括り 2>
      * 診療括りメモ
               05  LF04-MEMO           PIC  X(100).
      *AIP用
      *UID
           03  LF04-MEDICAL-UID       PIC X(36).
      *同日再入院区分
           03  LF04-DOUJITSUKBN       PIC X(01).
      *ver.4.7
      *病名補足コメント
           03  LF04-BYOMEI-TBL        OCCURS   50.
      *        補足コメントコード
               05  LF04-HOSOKUCD-G.
                   07  LF04-HOSOKUCD      PIC X(10)
                                          OCCURS   3.
      *        補足コメント
               05  LF04-HOSOKU-COMMENT    PIC X(40).
      *ver.4.8
      *API 診療行為
           03  LF04-SRYACCT-TBL1       OCCURS   40.
               05  LF04-SRYACCT-TBL2       OCCURS   40.
      *            継続コメント指示区分
                   07  LF04-CONTKBN        PIC X(01).
      *            内服１種類区分
                   07  LF04-NAIFKBN        PIC X(01).
      *
