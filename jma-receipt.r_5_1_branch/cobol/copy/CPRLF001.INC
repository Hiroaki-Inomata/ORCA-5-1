      ************************************************************
      *    ＣＬＡＩＭ００１。ＴＸＴ   COPY (CPRLF001.INC)        *
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  LF01-REC.
      * [MMLヘッダ]
      * 患者主ID
           03  LF01-PTID1               PIC X(20).
      * IDの種類コード
           03  LF01-IDTYPE1             PIC X(08).
      * 作成者 個人ID [必須]
           03  LF01-CRE001-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF01-CRE001-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF01-CRE001-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF01-CRE001-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF01-CRE001-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF01-CRE001-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF01-CRE001-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF01-CRE001-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF01-CRE001-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF01-CRE001-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF01-CRE001-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
           03  LF01-CRE001-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF01-CRE001-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
           03  LF01-CRE001-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
           03  LF01-CRE001-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
           03  LF01-CRE001-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
           03  LF01-CRE001-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF01-CRE001-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF01-CRE001-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF01-CRE001-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
           03  LF01-CRE001-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
           03  LF01-CRE001-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
           03  LF01-CRE001-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF01-CRE001-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF01-CRE001-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF01-CRE001-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF01-CRE001-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
           03  LF01-CRE001-LICENSE      PIC X(20).
      * [患者情報]
      * 文書ユニークID
           03  LF01-UID1                PIC X(36).
      * カルテ電子保存確定日
           03  LF01-CONFIRMDATE1        PIC X(10).
      * 作成者 個人ID [必須]
           03  LF01-CRE101-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF01-CRE101-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF01-CRE101-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF01-CRE101-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF01-CRE101-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF01-CRE101-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF01-CRE101-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF01-CRE101-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF01-CRE101-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF01-CRE101-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF01-CRE101-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
           03  LF01-CRE101-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF01-CRE101-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
           03  LF01-CRE101-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
           03  LF01-CRE101-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
           03  LF01-CRE101-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
           03  LF01-CRE101-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF01-CRE101-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF01-CRE101-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF01-CRE101-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
           03  LF01-CRE101-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
           03  LF01-CRE101-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
           03  LF01-CRE101-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF01-CRE101-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF01-CRE101-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF01-CRE101-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF01-CRE101-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格
           03  LF01-CRE101-LICENSE      PIC X(20).
      * 患者主ID
           03  LF01-PTID2               PIC X(20).
      * 患者主IDのテーブル名
           03  LF01-IDTYPE2             PIC X(08).
      * 氏名(カナ)の表記法
           03  LF01-KANANAME-REPCODE    PIC X(01).
      * 氏名(カナ)のフルネーム
           03  LF01-KANANAME            PIC X(100).
      * 氏名(漢字)の表記法
           03  LF01-NAME-REPCODE        PIC X(01).
      * 氏名(漢字)のフルネーム
           03  LF01-NAME                PIC X(100).
      * 生年月日
           03  LF01-BIRTHDAY            PIC X(10).
      * 性別
           03  LF01-SEX                 PIC X(07).
      * 住所(自宅)
           03  LF01-HOME-ADRS           PIC X(400).
      * 住所(自宅) 種類コード
           03  LF01-HOME-KREPCODE       PIC X(09).
      * 住所(自宅) 郵便番号
           03  LF01-HOME-POST           PIC X(08).
      * 住所(連絡先)
           03  LF01-RENRAKU-ADRS        PIC X(400).
      * 住所(連絡先) 種類コード
           03  LF01-RENRAKU-KREPCODE    PIC X(09).
      * 住所(連絡先) 郵便番号
           03  LF01-RENRAKU-POST        PIC X(08).
      * 住所(勤務先)
           03  LF01-OFFICE-ADRS         PIC X(400).
      * 住所(勤務先) 種類コード
           03  LF01-OFFICE-KREPCODE     PIC X(09).
      * 住所(勤務先) 郵便番号
           03  LF01-OFFICE-POST         PIC X(08).
      * 住所(帰省先)
           03  LF01-KISEI-ADRS          PIC X(400).
      * 住所(帰省先) 種類コード
           03  LF01-KISEI-KREPCODE      PIC X(09).
      * 住所(帰省先) 郵便番号
           03  LF01-KISEI-POST          PIC X(08).
      * 市外局番
           03  LF01-PHONE-AREA          PIC X(10).
      * 局番
           03  LF01-PHONE-CITY          PIC X(10).
      * 加入番号
           03  LF01-PHONE-NUM           PIC X(10).
      * 内線番号
           03  LF01-PHONE-EXT           PIC X(10).
      * 国番号
           03  LF01-PHONE-COUNTRY       PIC X(10).
      * 使用時間帯などを記載
           03  LF01-PHONE-MEMO          PIC X(100).
      * 装置の種類コード [省略可能・属性] (PH)
           03  LF01-PHONE-EQUIP         PIC X(10).
      * 死亡フラグ
           03  LF01-DEATHKBN            PIC X(05).
      * [健康保険情報]
           03  LF01-TBL                OCCURS  20.
      * 文書ユニークID
               05  LF01-UID2           PIC X(36).
      * カルテ電子保存の確定日
               05  LF01-CONFIRMDATE2   PIC X(10).
      * 作成者 個人ID [必須]
               05  LF01-CRE201-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               05  LF01-CRE201-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               05  LF01-CRE201-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               05  LF01-CRE201-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               05  LF01-CRE201-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               05  LF01-CRE201-PREFIX       PIC X(50).
      * 作成者 学位
               05  LF01-CRE201-DEGREE       PIC X(50).
      * 作成者 施設情報名
               05  LF01-CRE201-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               05  LF01-CRE201-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               05  LF01-CRE201-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               05  LF01-CRE201-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
               05  LF01-CRE201-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               05  LF01-CRE201-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
               05  LF01-CRE201-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
               05  LF01-CRE201-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
               05  LF01-CRE201-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
               05  LF01-CRE201-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               05  LF01-CRE201-ZIP          PIC X(8).
      * 作成者 国コード
               05  LF01-CRE201-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               05  LF01-CRE201-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
               05  LF01-CRE201-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
               05  LF01-CRE201-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
               05  LF01-CRE201-PHONENUM     PIC X(10).
      * 作成者 内線番号
               05  LF01-CRE201-PHONEEXT     PIC X(10).
      * 作成者 国番号
               05  LF01-CRE201-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               05  LF01-CRE201-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               05  LF01-CRE201-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
               05  LF01-CRE201-LICENSE      PIC X(20).
      * 健康保険種別名称
               05  LF01-HKNSBT              PIC X(20).
      * 健康保険種別コード
               05  LF01-HKNSBTCODE          PIC X(2).
      * 健康保険種別コードテーブル名
               05  LF01-HKNSBTTBL           PIC X(7).
      *保険者番号
               05  LF01-HKNJANUM       PIC  X(08).
      *記号
               05  LF01-KIGO           PIC  X(80).
      *番号
               05  LF01-NUM            PIC  X(80).
      *本人家族区分
               05  LF01-HONKZKKBN      PIC  X(05).
      *適用開始年月日
               05  LF01-TEKSTYMD       PIC  X(10).
      *適用終了年月日
               05  LF01-TEKEDYMD       PIC  X(10).
      * 入院時の負担率
               05  LF01-INRATIO        PIC X(5).
      * 外来時の負担率
               05  LF01-OUTRATIO       PIC X(5).
      *公費情報
               05  LF01-KOHINF       OCCURS  6.
      *優先順位
                   07  LF01-PRIORITY   PIC  X(01).
      *公費名称
                   07  LF01-KOHINAME   PIC  X(50).
      *負担者番号
                   07  LF01-FTNJANUM   PIC  X(08).
      *受給者番号
                   07  LF01-JKYSNUM    PIC  X(20).
      *適用開始年月日
                   07  LF01-TTEKSTYMD  PIC  X(10).
      *適用終了年月日
                   07  LF01-TTEKEDYMD  PIC  X(10).
      * 負担金または負担率
                   07  LF01-RATIO-CODE PIC X(10).
                   07  LF01-RATIO      PIC X(10).
      *[CLAIM]
      * 文書ユニークID
           03  LF01-UID3                PIC X(36).
      * カルテ電子保存の確定日
           03  LF01-CONFIRMDATE3        PIC X(10).
      * 作成者 個人ID [必須]
           03  LF01-CRE301-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
           03  LF01-CRE301-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
           03  LF01-CRE301-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
           03  LF01-CRE301-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
           03  LF01-CRE301-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
           03  LF01-CRE301-PREFIX       PIC X(50).
      * 作成者 学位
           03  LF01-CRE301-DEGREE       PIC X(50).
      * 作成者 施設情報名
           03  LF01-CRE301-FACILITYNAME PIC X(50).
      * 作成者 施設ID
           03  LF01-CRE301-FACILITYID   PIC X(20).
      * 作成者 診療科名称
           03  LF01-CRE301-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
           03  LF01-CRE301-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
           03  LF01-CRE301-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
           03  LF01-CRE301-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
           03  LF01-CRE301-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
           03  LF01-CRE301-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
           03  LF01-CRE301-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
           03  LF01-CRE301-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
           03  LF01-CRE301-ZIP          PIC X(8).
      * 作成者 国コード
           03  LF01-CRE301-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
           03  LF01-CRE301-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
           03  LF01-CRE301-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
           03  LF01-CRE301-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
           03  LF01-CRE301-PHONENUM     PIC X(10).
      * 作成者 内線番号
           03  LF01-CRE301-PHONEEXT     PIC X(10).
      * 作成者 国番号
           03  LF01-CRE301-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
           03  LF01-CRE301-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
           03  LF01-CRE301-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
           03  LF01-CRE301-LICENSE      PIC X(20).
      * 予約請求モジュールの状態 (claim:status)
           03  LF01-UKEYMD-KREPCODE    PIC  X(07).
      * オーダー発行日時 (claim:oderTime)
           03  LF01-CONFIRMDATE4       PIC  X(19).
      * 受付日時 (claim:registTime)
           03  LF01-UKEYMD             PIC  X(19).
      * 入院外来区分 (claim:admitFlag)
           03  LF01-NYUGAIKBN          PIC  X(05).
      * 使用保険 (claim:insuranceUid)
           03  LF01-FIL1               PIC  X(36).
      * 共通医事請求コード体系
           03  LF01-IJISKYCOD          PIC  X(100).
      * 予約
           03  LF01-YYK                PIC  X(20).
      * 予約コード (claim:appCode)
           03  LF01-YYKCOD             PIC  X(14).
      * 予約メモ
           03  LF01-YYKMEMO            PIC  X(100).
      *診療科名称
           03  LF01-SRYNAME            PIC  X(12).
      *診療科表記
           03  LF01-SRYREP             PIC  X(1).
      * 診療科ID
           03  LF01-SRYID              PIC  X(2).
      * 診療科IDのテーブル名
           03  LF01-SRYIDTBL           PIC  X(7).
      * 病棟診療科名称
           03  LF01-SNRNAME            PIC  X(12).
      * 病棟診療科 表記法
           03  LF01-SNRREP             PIC  X(1).
      * 病棟診療科名称
           03  LF01-SNRID              PIC  X(2).
      * 病棟診療科 IDの種類コード
           03  LF01-SNRIDTBL           PIC  X(7).
      * 診療種別コード (claim:classCode)
           03  LF01-BUNDLE-CLASSCODE   PIC  X(3).
      *医事請求コード  
           03  LF01-IJICD             PIC  X(10).
      * 医事請求コード体系名
           03  LF01-ID                PIC  X(20).
      * 医事請求名称
           03  LF01-HOSPSBT           PIC  X(40).
      * 受付メモ
           03  LF01-UKE-MEMO          PIC  X(1000).
