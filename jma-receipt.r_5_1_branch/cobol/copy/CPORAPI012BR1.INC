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
      ******************************************************************
      *    ＡＰＩ患者情報一括取得 (xml2)
      *                                    COPY (CPORAPI012BR1.INC)
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTINFLST-REC.
      *    患者情報
           03  PTINFLST-PATIENT-INFOMATION.
      *        患者番号
               05  PTINFLST-PATIENTID          PIC X(20).
      *        漢字氏名
               05  PTINFLST-NAME               PIC X(100).
      *        カナ氏名
               05  PTINFLST-KANANAME           PIC X(100).
      *        生年月日
               05  PTINFLST-BIRTHDAY           PIC X(10).
      *        性別
               05  PTINFLST-SEX                PIC X(1).
      *        世帯主名
               05  PTINFLST-SETAI-NAME         PIC X(100).
      *        続柄
               05  PTINFLST-ZOKUGARA           PIC X(60).
      *        患者自宅情報
               05  PTINFLST-ADDRESS-INFO.
      *            郵便番号
                   07  PTINFLST-HOME-ZIP-CODE      PIC X(7).
      *            住所
                   07  PTINFLST-HOME-ADDRESSES1    PIC X(200).
      *            番地番号
                   07  PTINFLST-HOME-ADDRESSES2    PIC X(200).
      *            自宅電話番号（昼）
                   07  PTINFLST-HOME-HPONE1        PIC X(15).
      *            自宅電話番号（昼）
                   07  PTINFLST-HOME-HPONE2        PIC X(15).
      *
      *        勤務先情報
               05  PTINFLST-OFFICE-INFO.
      *            勤務先名
                   07  PTINFLST-OFFICE-NAME        PIC X(50).
      *            郵便番号
                   07  PTINFLST-OFFICE-ZIP-CODE    PIC X(7).
      *            住所
                   07  PTINFLST-OFFICE-ADDRESSES1  PIC X(100).
      *            番地番号
                   07  PTINFLST-OFFICE-ADDRESSES2  PIC X(100).
      *            電話番号
                   07  PTINFLST-OFFICE-HPONE       PIC X(15).
      *        連絡先情報
               05  PTINFLST-RENRAKU-INFO.
      *            連絡先名
                   07  PTINFLST-RENRAKU-NAME        PIC X(50).
      *            連絡先続柄
                   07  PTINFLST-RENRAKU-ZOKUGARA    PIC X(30).
      *            郵便番号
                   07  PTINFLST-RENRAKU-ZIP-CODE    PIC X(7).
      *            住所
                   07  PTINFLST-RENRAKU-ADDRESSES1  PIC X(100).
      *            番地番号
                   07  PTINFLST-RENRAKU-ADDRESSES2  PIC X(100).
      *            電話番号　昼
                   07  PTINFLST-RENRAKU-HPONE1      PIC X(15).
      *            電話番号　夜
                   07  PTINFLST-RENRAKU-HPONE2      PIC X(15).
      *        帰省先情報
               05  PTINFLST-KISEI-INFO.
      *            帰省先名
                   07  PTINFLST-KISEI-NAME          PIC X(50).
      *            郵便番号
                   07  PTINFLST-KISEI-ZIP-CODE      PIC X(7).
      *            住所
                   07  PTINFLST-KISEI-ADDRESSES1    PIC X(100).
      *            番地番号
                   07  PTINFLST-KISEI-ADDRESSES2    PIC X(100).
      *            電話番号
                   07  PTINFLST-KISEI-HPONE         PIC X(15).
      *
      *        禁忌１
               05  PTINFLST-TABOO-1            PIC  X(100).
      *        禁忌２
               05  PTINFLST-TABOO-2            PIC  X(100).
      *        アレルギー１
               05  PTINFLST-ALLERGY-1          PIC  X(100).
      *        アレルギー２
               05  PTINFLST-ALLERGY-2          PIC  X(100).
      *        感染症１
               05  PTINFLST-KANSENSYO-1        PIC  X(100).
      *        感染症２
               05  PTINFLST-KANSENSYO-2        PIC  X(100).
      *        コメント１
               05  PTINFLST-COMMENT-1          PIC  X(100).
      *        コメント２
               05  PTINFLST-COMMENT-2          PIC  X(100).
      *        テスト患者区分
               05  PTMODRES-TEST-PATIENT       PIC  X(01).
      *        死亡区分
               05  PTMODRES-DEATHKBN           PIC  X(01).
      *        職業
               05  PTINFLST-JOB                PIC X(20).
      *        通称
               05  PTINFLST-NICKNAME           PIC  X(50).
      *        携帯電話番号
               05  PTINFLST-KEITAI-TEL         PIC  X(15).
      *        ＦＡＸ番号
               05  PTINFLST-FAX                PIC  X(15).
      *        電子メールアドレス
               05  PTINFLST-EMAIL              PIC  X(50).
      *        減免事由
               05  PTINFLST-DISCOUNTKBN        PIC  X(02).
               05  PTINFLST-DISCOUNTKBN-NAME   PIC  X(20).
      *        割引率
               05  PTINFLST-DISCOUNT           PIC  X(02).
               05  PTINFLST-DISCOUNT-NAME      PIC  X(14).
      *        状態１から３
               05  PTINFLST-CONDITION-KBN-G.
                   07  PTINFLST-CONDITION-OCC    OCCURS   3.
                       09  PTINFLST-CONDITION-KBN     PIC X(02).
                       09  PTINFLST-CONDITION-NAME    PIC X(20).
      *        入金方法区分
               05  PTINFLST-IC-CODE            PIC  X(02).
               05  PTINFLST-IC-CODE-NAME       PIC  X(20).
      *
      *        地域連携ID
               05  PTINFLST-COMMUNITY-CID        PIC  X(20).
      *        同意フラグ
               05  PTINFLST-COMMUNITY-CID-AGREE  PIC  X(05).
      *
      *        初回来院日
               05  PTINFLST-FIRST-DATE           PIC  X(10).
      *        最終来院日（退院日）
               05  PTINFLST-LAST-DATE            PIC  X(10).
      *        入院中区分（１：入院中）
               05  PTINFLST-OUTPATIENT-CLASS     PIC  X(01).
      *        入院日（入院中のみ）
               05  PTINFLST-ADMISSION-DATE       PIC  X(10).
      *        退院日（当日退院時のみ）
               05  PTINFLST-DISCHARGE-DATE       PIC  X(10).
      *
      *    保険情報
           03  PTINFLST-INSURANCE-INFO     OCCURS  40 TIMES.
      *        保険組合せ番号
               05  PTINFLST-COMB-NUMBER            PIC X(04).
      *        保険組合せ　開始日
               05  PTINFLST-COMB-START-DATE        PIC X(10).
      *        保険組合せ　終了日
               05  PTINFLST-COMB-END-DATE          PIC X(10).
      *        保険組合せ　負担割合（入院）
               05  PTINFLST-COMB-RATE-ADM          PIC X(04).
      *        保険組合せ　負担割合（外来）
               05  PTINFLST-COMB-RATE-OUT          PIC X(04).
      *        非表示区分
               05  PTINFLST-COMB-HYOJI             PIC X(01).
      *
      *        保険情報
               05  PTINFLST-MAIN-INSURANCE-INFO.
      *            保険の種類
                   07  PTINFLST-MAIN-CLASS             PIC X(03).
      *            保険者番号
                   07  PTINFLST-INSURANCE-NUMBER       PIC X(08).
      *            保険の種類名称
                   07  PTINFLST-MAIN-NAME              PIC X(20).
      *            記号
                   07  PTINFLST-MAIN-MARK              PIC X(80).
      *            番号
                   07  PTINFLST-MAIN-NUMBER            PIC X(80).
      *            継続区分
                   07  PTINFLST-MAIN-CONTINUATION      PIC X(01).
      *            補助区分
                   07  PTINFLST-MAIN-ASSISTANCE        PIC X(01).
      *            補助区分名称
                   07  PTINFLST-MAIN-ASSISTANCE-NAME   PIC X(20).
      *            本人家族区分
                   07  PTINFLST-MAIN-FAMILY            PIC X(01).
      *            被保険者名
                   07  PTINFLST-MAIN-POLICYHOLDER      PIC X(100).
      *            有効開始日
                   07  PTINFLST-MAIN-START-DATE        PIC X(10).
      *            有効終了日
                   07  PTINFLST-MAIN-END-DATE          PIC X(10).
      *            資格取得日
                   07  PTINFLST-MAIN-GET-DATE          PIC X(10).
      *            最終確認日
                   07  PTINFLST-MAIN-CHECK-DATE        PIC X(10).
      *
      *            公費情報
                   07  PTINFLST-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *                公費の種類
                       09  PTINFLST-PUBLIC-CLASS           PIC X(03).
      *                公費の種類名称
                       09  PTINFLST-PUBLIC-NAME            PIC X(20).
      *                負担者番号
                       09  PTINFLST-PROVIDER-NUMBER        PIC X(08).
      *                受給者番号
                       09  PTINFLST-RECIPIENT-NUMBER       PIC X(20).
      *                入院−負担率（割）
                       09  PTINFLST-RATE-ADMISSION         PIC X(04).
      *                入院−固定額
                       09  PTINFLST-MONEY-ADMISSION        PIC X(06).
      *                外来−負担率（割）
                       09  PTINFLST-RATE-OUTPATIENT        PIC X(04).
      *                外来−固定額
                       09  PTINFLST-MONEY-OUTPATIENT       PIC X(06).
      *                有効開始日
                       09  PTINFLST-PUBLIC-START-DATE      PIC X(10).
      *                有効終了日
                       09  PTINFLST-PUBLIC-END-DATE        PIC X(10).
      *R01.6
      *                最終確認日
                       09  PTINFLST-PUBLIC-CHECK-DATE      PIC X(10).
      *労災情報
               05  PTINFLST-ACCIDENT-INF-G.
      *            労災保険名称（記号と同じ内容）
                   07  PTINFLST-ACCIDENT-NAME           PIC X(50).
      *            傷病の部位（番号と同じ内容）
                   07  PTINFLST-DISEASE-LOCATION        PIC X(100).
      *            傷病年月日
                   07  PTINFLST-DISEASE-DATE            PIC X(10).
      *労災
      *            労働保険番号
                   07  PTINFLST-ACCIDENT-NUMBER         PIC X(14).
      *            年金証書番号
                   07  PTINFLST-PENSIONCERTIFICATE      PIC X(09).
      *            災害区分（名称）
                   07  PTINFLST-ACCIDENT-CLASS          PIC X(20).
      *            労働基準監督署コード
                   07  PTINFLST-LABOR-ST-CODE           PIC X(05).
      *            労働基準監督署コード
                   07  PTINFLST-LABOR-ST-NAME           PIC X(50).
      *            事業所名称
                   07  PTINFLST-RSI-JIGYOUNAME          PIC X(80).
      *            所在地都道府県名
                   07  PTINFLST-RSI-PREFNAME            PIC X(20).
      *            所在地都道府県区分
                   07  PTINFLST-RSI-PREFKBN             PIC X(01).
      *            所在地都道府県区分名称
                   07  PTINFLST-RSI-PREFKBNNAME         PIC X(02).
      *            所在地郡市区名
                   07  PTINFLST-RSI-CITYNAME            PIC X(80).
      *            所在地郡市区区分
                   07  PTINFLST-RSI-CITYKBN             PIC X(01).
      *            所在地郡市区名称
                   07  PTINFLST-RSI-CITYKBNNAME         PIC X(02).
      *自賠責
      *            自賠責保険会社名
                   07  PTINFLST-RSI-HOKENNAME           PIC X(80).
      *アフターケア
      *            健康管理手帳番号
                   07  PTINFLST-RSI-KANRITCHO-NUMBER    PIC X(13).
      *            損傷区分
                   07  PTINFLST-DAMAGE-CLASS            PIC X(03).
      *            損傷区分名称
                   07  PTINFLST-DAMAGE-CLASS-NAME       PIC X(100).
      *
      *    介護情報
           03  PTINFLST-CARE-INF-G.
      *        介護保険情報
               05  PTINFLST-CARE-INSURANCE-G    OCCURS   10.
      *            保険者番号
                   07  PTINFLST-CARE-INSURANCE-NUMBER  PIC X(06).
      *            被保険者番号
                   07  PTINFLST-CARE-POLICYHOLDER      PIC X(10).
      *            有効開始日
                   07  PTINFLST-CARE-START-DATE        PIC X(10).
      *            有効終了日
                   07  PTINFLST-CARE-END-DATE          PIC X(10).
      *            要介護認定情報
               05  PTINFLST-CERTIFICATION-G    OCCURS   50.
      *            要介護状態区分
                   07  PTINFLST-NEED-CARE-STATE-CODE   PIC X(02).
      *            要介護状態名称
                   07  PTINFLST-NEED-CARE-STATE        PIC X(20).
      *            認定日付
                   07  PTINFLST-NEED-DATE              PIC X(10).
      *            有効開始日
                   07  PTINFLST-NEED-START-DATE        PIC X(10).
      *            有効終了日
                   07  PTINFLST-NEED-END-DATE          PIC X(10).
      *
      *    地域包括診療対象疾病
           03  PTINFLST-COMMUNITY-DISEASE.
               05  PTINFLST-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *
      *    患者個人番号情報
           03  PTINFLST-PTMYNUMBER-G.
               05  PTINFLST-PTMYNUMBER-TBL     OCCURS   20.
      *            ＩＤ識別
                   07  PTINFLST-ID-KEY           PIC X(10).
      *            ＩＤ番号
                   07  PTINFLST-MY-NUMBER        PIC X(20).
      *            備考
                   07  PTINFLST-DESCRIPTION      PIC X(100).
      *
      *H30.10
      *    患者個別情報
           03  PTINFLST-PTCONF-INFO.
      *        妊婦設定
               05  PTINFLST-NINPUKBN             PIC X(05).
      *        認知症地域包括診療
               05  PTINFLST-NINTIHOKATU          PIC X(05).
      *        小児かかりつけ診療
               05  PTINFLST-SYOUNIKAKARI         PIC X(05).
      *
      *    管理料等自動算定
           03  PTINFLST-AUTO-SRYCD-G.
               05  PTINFLST-AUTO-SRYCD-TBL     OCCURS   3.
                   07  PTINFLST-AUTO-SRYCD       PIC X(09).
                   07  PTINFLST-AUTO-NAME        PIC X(80).
                   07  PTINFLST-AUTO-ENDYMD      PIC X(10).
      *
      *    患者禁忌薬剤
           03  PTINFLST-KINKI-YKZAI-G.
               05  PTINFLST-KINKI-YKZAI-TBL    OCCURS   100.
      *            禁忌薬剤コード
                   07  PTINFLST-DRUGCD-CODE       PIC X(09).
                   07  PTINFLST-DRUGCD-NAME       PIC X(80).
      *            有効終了日（マスタ終了日）
                   07  PTINFLST-DRUGCD-END-DATE   PIC X(10).
      *            禁忌開始日
                   07  PTINFLST-START-DATE        PIC X(10).
      *
