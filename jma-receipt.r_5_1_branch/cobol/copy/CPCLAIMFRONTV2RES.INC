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
      *    ＡＰＩレスポンスＣＬＡＩＭ受付送信取得(xml2)
      *                                      COPY (CPCLAIMFRONTV2RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-CLAIM-FRONTV2RES.
           02  CLFRRES-CONTEXT           PIC S9(9)   BINARY.
           02  CLFRRES-OBJECT            PIC X(36).
           02  CLFRRES-MODE              PIC S9(9)   BINARY.
           02  CLFRRES-RECORDNAME        PIC X(128).
           02  CLFRRES-CLAIM-FRONT.
      *        実施日
               03  CLFRRES-INFORMATION-DATE    PIC X(10).
      *        実施時間
               03  CLFRRES-INFORMATION-TIME    PIC X(8).
      *        返却コード
               03  CLFRRES-API-RESULT          PIC X(2).
      *        返却メッセージ
               03  CLFRRES-API-RESULT-MSG      PIC X(100).
      *        患者番号
               03  CLFRRES-PATIENTID           PIC X(20).
      *
      * 実施日(CCYY-MM-DDTHH:MM:SS形式)
               03  CLFRRES-INFORMATION-HEN     PIC X(19).
      * 患者主ID
               03  CLFRRES-PTID1               PIC X(20).
      * IDの種類コード
               03  CLFRRES-IDTYPE1             PIC X(08).
      * 作成者 個人ID [必須]
               03  CLFRRES-CRE001-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               03  CLFRRES-CRE001-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               03  CLFRRES-CRE001-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               03  CLFRRES-CRE001-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               03  CLFRRES-CRE001-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               03  CLFRRES-CRE001-PREFIX       PIC X(50).
      * 作成者 学位
               03  CLFRRES-CRE001-DEGREE       PIC X(50).
      * 作成者 施設情報名
               03  CLFRRES-CRE001-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               03  CLFRRES-CRE001-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               03  CLFRRES-CRE001-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               03  CLFRRES-CRE001-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
               03  CLFRRES-CRE001-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               03  CLFRRES-CRE001-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
               03  CLFRRES-CRE001-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
               03  CLFRRES-CRE001-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
               03  CLFRRES-CRE001-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
               03  CLFRRES-CRE001-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               03  CLFRRES-CRE001-ZIP          PIC X(8).
      * 作成者 国コード
               03  CLFRRES-CRE001-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               03  CLFRRES-CRE001-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
               03  CLFRRES-CRE001-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
               03  CLFRRES-CRE001-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
               03  CLFRRES-CRE001-PHONENUM     PIC X(10).
      * 作成者 内線番号
               03  CLFRRES-CRE001-PHONEEXT     PIC X(10).
      * 作成者 国番号
               03  CLFRRES-CRE001-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               03  CLFRRES-CRE001-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               03  CLFRRES-CRE001-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
               03  CLFRRES-CRE001-LICENSE      PIC X(20).
      *##
      * コンスタント値 (<extRefs />)
               03  CLFRRES-M-RX001-I1          PIC X(20).
      * コンスタント値  (<accessRight permit="all" />) 
               03  CLFRRES-M-RI101-ACC1        PIC X(50).
      * コンスタント値 (Data of Regist) 
               03  CLFRRES-M-RI101-TITLE       PIC X(20).
      *##
      *
      * [患者情報]
      * 文書ユニークID
               03  CLFRRES-UID1                PIC X(36).
      * カルテ電子保存確定日
               03  CLFRRES-CONFIRMDATE1        PIC X(10).
      * 作成者 個人ID [必須]
               03  CLFRRES-CRE101-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               03  CLFRRES-CRE101-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               03  CLFRRES-CRE101-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               03  CLFRRES-CRE101-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               03  CLFRRES-CRE101-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               03  CLFRRES-CRE101-PREFIX       PIC X(50).
      * 作成者 学位
               03  CLFRRES-CRE101-DEGREE       PIC X(50).
      * 作成者 施設情報名
               03  CLFRRES-CRE101-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               03  CLFRRES-CRE101-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               03  CLFRRES-CRE101-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               03  CLFRRES-CRE101-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
               03  CLFRRES-CRE101-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               03  CLFRRES-CRE101-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
               03  CLFRRES-CRE101-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
               03  CLFRRES-CRE101-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
               03  CLFRRES-CRE101-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
               03  CLFRRES-CRE101-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               03  CLFRRES-CRE101-ZIP          PIC X(8).
      * 作成者 国コード
               03  CLFRRES-CRE101-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               03  CLFRRES-CRE101-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
               03  CLFRRES-CRE101-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
               03  CLFRRES-CRE101-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
               03  CLFRRES-CRE101-PHONENUM     PIC X(10).
      * 作成者 内線番号
               03  CLFRRES-CRE101-PHONEEXT     PIC X(10).
      * 作成者 国番号
               03  CLFRRES-CRE101-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               03  CLFRRES-CRE101-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               03  CLFRRES-CRE101-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格
               03  CLFRRES-CRE101-LICENSE      PIC X(20).
      *##
      * コンスタント値 (<extRefs />) 
               03  CLFRRES-M-RX101-I1          PIC X(20).
      *##
      * 患者主ID
               03  CLFRRES-PTID2               PIC X(20).
      * 患者主IDのテーブル名
               03  CLFRRES-IDTYPE2             PIC X(08).
      * 氏名(カナ)の表記法
               03  CLFRRES-KANANAME-REPCODE    PIC X(01).
      * 氏名(カナ)のフルネーム
               03  CLFRRES-KANANAME            PIC X(100).
      * 氏名(漢字)の表記法
               03  CLFRRES-NAME-REPCODE        PIC X(01).
      * 氏名(漢字)のフルネーム
               03  CLFRRES-NAME                PIC X(100).
      * 生年月日
               03  CLFRRES-BIRTHDAY            PIC X(10).
      * 性別
               03  CLFRRES-SEX                 PIC X(07).
      * 住所(自宅)
               03  CLFRRES-HOME-ADRS           PIC X(400).
      * 住所(自宅) 種類コード
               03  CLFRRES-HOME-KREPCODE       PIC X(09).
      * 住所(自宅) 郵便番号
               03  CLFRRES-HOME-POST           PIC X(08).
      * 住所(連絡先)
               03  CLFRRES-RENRAKU-ADRS        PIC X(400).
      * 住所(連絡先) 種類コード
               03  CLFRRES-RENRAKU-KREPCODE    PIC X(09).
      * 住所(連絡先) 郵便番号
               03  CLFRRES-RENRAKU-POST        PIC X(08).
      * 住所(勤務先)
               03  CLFRRES-OFFICE-ADRS         PIC X(400).
      * 住所(勤務先) 種類コード
               03  CLFRRES-OFFICE-KREPCODE     PIC X(09).
      * 住所(勤務先) 郵便番号
               03  CLFRRES-OFFICE-POST         PIC X(08).
      * 住所(帰省先)
               03  CLFRRES-KISEI-ADRS          PIC X(400).
      * 住所(帰省先) 種類コード
               03  CLFRRES-KISEI-KREPCODE      PIC X(09).
      * 住所(帰省先) 郵便番号
               03  CLFRRES-KISEI-POST          PIC X(08).
      * 市外局番
               03  CLFRRES-PHONE-AREA          PIC X(10).
      * 局番
               03  CLFRRES-PHONE-CITY          PIC X(10).
      * 加入番号
               03  CLFRRES-PHONE-NUM           PIC X(10).
      * 内線番号
               03  CLFRRES-PHONE-EXT           PIC X(10).
      * 国番号
               03  CLFRRES-PHONE-COUNTRY       PIC X(10).
      * 使用時間帯などを記載
               03  CLFRRES-PHONE-MEMO          PIC X(100).
      * 装置の種類コード [省略可能・属性] (PH)
               03  CLFRRES-PHONE-EQUIP         PIC X(10).
      * 死亡フラグ
               03  CLFRRES-DEATHKBN            PIC X(05).
      * [健康保険情報]
               03  CLFRRES-TBL                OCCURS  20.
      *##
      * コンスタント値 (<accessRight permit="all" />)
                   05  CLFRRES-M-RI2II-ACC1     PIC X(30).
      * コンスタント値 (Data of Regist)
                   05  CLFRRES-M-RI2II-TITLE    PIC X(30).
      *##
      * 文書ユニークID
                   05  CLFRRES-UID2           PIC X(36).
      * カルテ電子保存の確定日
                   05  CLFRRES-CONFIRMDATE2   PIC X(10).
      * 作成者 個人ID [必須]
                   05  CLFRRES-CRE201-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
                   05  CLFRRES-CRE201-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
                   05  CLFRRES-CRE201-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
                   05  CLFRRES-CRE201-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
                   05  CLFRRES-CRE201-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
                   05  CLFRRES-CRE201-PREFIX       PIC X(50).
      * 作成者 学位
                   05  CLFRRES-CRE201-DEGREE       PIC X(50).
      * 作成者 施設情報名
                   05  CLFRRES-CRE201-FACILITYNAME PIC X(50).
      * 作成者 施設ID
                   05  CLFRRES-CRE201-FACILITYID   PIC X(20).
      * 作成者 診療科名称
                   05  CLFRRES-CRE201-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
                   05  CLFRRES-CRE201-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
                   05  CLFRRES-CRE201-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
                   05  CLFRRES-CRE201-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
                   05  CLFRRES-CRE201-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
                   05  CLFRRES-CRE201-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
                   05  CLFRRES-CRE201-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
                   05  CLFRRES-CRE201-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
                   05  CLFRRES-CRE201-ZIP          PIC X(8).
      * 作成者 国コード
                   05  CLFRRES-CRE201-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
                   05  CLFRRES-CRE201-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
                   05  CLFRRES-CRE201-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
                   05  CLFRRES-CRE201-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
                   05  CLFRRES-CRE201-PHONENUM     PIC X(10).
      * 作成者 内線番号
                   05  CLFRRES-CRE201-PHONEEXT     PIC X(10).
      * 作成者 国番号
                   05  CLFRRES-CRE201-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
                   05  CLFRRES-CRE201-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
                   05  CLFRRES-CRE201-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
                   05  CLFRRES-CRE201-LICENSE      PIC X(20).
      *##
      * コンスタント値  (<extRefs />)
                   05  CLFRRES-M-RX2II-I1     PIC X(20).
      *##
      * 健康保険種別名称
                   05  CLFRRES-HKNSBT              PIC X(20).
      * 健康保険種別コード
                   05  CLFRRES-HKNSBTCODE          PIC X(2).
      * 健康保険種別コードテーブル名
                   05  CLFRRES-HKNSBTTBL           PIC X(7).
      *保険者番号
                   05  CLFRRES-HKNJANUM       PIC  X(08).
      *記号
                   05  CLFRRES-KIGO           PIC  X(80).
      *番号
                   05  CLFRRES-NUM            PIC  X(80).
      *本人家族区分
                   05  CLFRRES-HONKZKKBN      PIC  X(05).
      *適用開始年月日
                   05  CLFRRES-TEKSTYMD       PIC  X(10).
      *適用終了年月日
                   05  CLFRRES-TEKEDYMD       PIC  X(10).
      * 入院時の負担率
                   05  CLFRRES-INRATIO        PIC X(5).
      * 外来時の負担率
                   05  CLFRRES-OUTRATIO       PIC X(5).
      *公費情報
                   05  CLFRRES-KOHINF       OCCURS  6.
      *優先順位
                       07  CLFRRES-PRIORITY   PIC  X(01).
      *公費名称
                       07  CLFRRES-KOHINAME   PIC  X(50).
      *負担者番号
                       07  CLFRRES-FTNJANUM   PIC  X(08).
      *受給者番号
                       07  CLFRRES-JKYSNUM    PIC  X(20).
      *適用開始年月日
                       07  CLFRRES-TTEKSTYMD  PIC  X(10).
      *適用終了年月日
                       07  CLFRRES-TTEKEDYMD  PIC  X(10).
      * 負担金または負担率
                       07  CLFRRES-RATIO-CODE PIC X(10).
                       07  CLFRRES-RATIO      PIC X(10).
      *##
      * コンスタント値 (<accessRight permit="all" />)
               03  CLFRRES-M-RI301-ACC1     PIC X(50).
      * コンスタント値 (Data of Regist) 
               03  CLFRRES-M-RI301-TITLE     PIC X(20).
      *##
      *[CLAIM]
      * 文書ユニークID
               03  CLFRRES-UID3                PIC X(36).
      * カルテ電子保存の確定日
               03  CLFRRES-CONFIRMDATE3        PIC X(10).
      * 作成者 個人ID [必須]
               03  CLFRRES-CRE301-PERSONALID   PIC X(20).
      * 作成者 個人フルネーム [条件付き必須]
               03  CLFRRES-CRE301-FULLNAME     PIC X(100).
      * 作成者 個人姓 [条件付き必須]
               03  CLFRRES-CRE301-FAMILYNAME   PIC X(50).
      * 作成者 個人名 [条件付き必須]
               03  CLFRRES-CRE301-GIVENNAME    PIC X(50).
      * 作成者 ミドルネーム
               03  CLFRRES-CRE301-MIDDLENAME   PIC X(50).
      * 作成者 肩書きなど
               03  CLFRRES-CRE301-PREFIX       PIC X(50).
      * 作成者 学位
               03  CLFRRES-CRE301-DEGREE       PIC X(50).
      * 作成者 施設情報名
               03  CLFRRES-CRE301-FACILITYNAME PIC X(50).
      * 作成者 施設ID
               03  CLFRRES-CRE301-FACILITYID   PIC X(20).
      * 作成者 診療科名称
               03  CLFRRES-CRE301-DPERTNAME    PIC X(50).
      * 作成者 診療科ID
               03  CLFRRES-CRE301-DPERTID      PIC X(20).
      * 作成者 住所 [条件付き必須]
               03  CLFRRES-CRE301-ADRFULLNAME  PIC X(400).
      * 作成者 住所の種類
               03  CLFRRES-CRE301-ADRCLASS     PIC X(10).
      * 作成者 都道府県 [条件付き必須]
               03  CLFRRES-CRE301-ADRPREF      PIC X(20).
      * 作成者 市、区、郡 [条件付き必須]
               03  CLFRRES-CRE301-ADRCITY      PIC X(20).
      * 作成者 町、村 [条件付き必須]
               03  CLFRRES-CRE301-ADRTOWN      PIC X(20).
      * 作成者 番地、丁目等 [条件付き必須]
               03  CLFRRES-CRE301-ADRHOMENUM   PIC X(200).
      * 作成者 郵便番号
               03  CLFRRES-CRE301-ZIP          PIC X(8).
      * 作成者 国コード
               03  CLFRRES-CRE301-COUNTRY      PIC X(10).
      * 作成者 メールアドレス
               03  CLFRRES-CRE301-EMAIL        PIC X(100).
      * 作成者 市外局番 [必須]
               03  CLFRRES-CRE301-PHONEAREA    PIC X(10).
      * 作成者 局番 [必須]
               03  CLFRRES-CRE301-PHONECITY    PIC X(10).
      * 作成者 加入番号 [必須]
               03  CLFRRES-CRE301-PHONENUM     PIC X(10).
      * 作成者 内線番号
               03  CLFRRES-CRE301-PHONEEXT     PIC X(10).
      * 作成者 国番号
               03  CLFRRES-CRE301-PHONECOUNTRY PIC X(10).
      * 作成者 使用時間帯などを記載
               03  CLFRRES-CRE301-PHONEMEMO    PIC X(100).
      * 作成者 装置の種類コード
               03  CLFRRES-CRE301-PHONEEQUIP   PIC X(10).
      * 作成者 生成者の資格 [必須]
               03  CLFRRES-CRE301-LICENSE      PIC X(20).
      *##
      * コンスタント値 (<extRefs />)                  const
               03  CLFRRES-M-RX301-I1          PIC X(20).
      * 予約請求モジュールの状態 (claim:status)
               03  CLFRRES-UKEYMD-KREPCODE    PIC  X(07).
      * オーダー発行日時 (claim:oderTime)
               03  CLFRRES-CONFIRMDATE4       PIC  X(19).
      * 受付日時 (claim:registTime)
               03  CLFRRES-UKEYMD             PIC  X(19).
      * 入院外来区分 (claim:admitFlag)
               03  CLFRRES-NYUGAIKBN          PIC  X(05).
      * 使用保険 (claim:insuranceUid)
               03  CLFRRES-FIL1               PIC  X(36).
      * 共通医事請求コード体系
               03  CLFRRES-IJISKYCOD          PIC  X(100).
      * 予約
               03  CLFRRES-YYK                PIC  X(20).
      * 予約コード (claim:appCode)
               03  CLFRRES-YYKCOD             PIC  X(14).
      *##
      * コンスタント値 (claim:appCodeId="Claim009"")
               03  CLFRRES-C-B3                PIC X(30).
      * 予約メモ
               03  CLFRRES-YYKMEMO            PIC  X(100).
      *診療科名称
               03  CLFRRES-SRYNAME            PIC  X(12).
      *診療科表記
               03  CLFRRES-SRYREP             PIC  X(1).
      * 診療科ID
               03  CLFRRES-SRYID              PIC  X(2).
      * 診療科IDのテーブル名
               03  CLFRRES-SRYIDTBL           PIC  X(7).
      * 病棟診療科名称
               03  CLFRRES-SNRNAME            PIC  X(12).
      * 病棟診療科 表記法
               03  CLFRRES-SNRREP             PIC  X(1).
      * 病棟診療科名称
               03  CLFRRES-SNRID              PIC  X(2).
      * 病棟診療科 IDの種類コード
               03  CLFRRES-SNRIDTBL           PIC  X(7).
      * 診療種別コード (claim:classCode)
               03  CLFRRES-BUNDLE-CLASSCODE   PIC  X(3).
      *医事請求コード  
               03  CLFRRES-IJICD             PIC  X(10).
      * 医事請求コード体系名
               03  CLFRRES-ID                PIC  X(20).
      * 医事請求名称
               03  CLFRRES-HOSPSBT           PIC  X(40).
      * 受付メモ
               03  CLFRRES-UKE-MEMO          PIC  X(1000).
      *
