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
      *    ＡＰＩレスポンス患者情報取得 (xml2)
      *                                    COPY (CPPATIENTXMLV2RES.INC)
      ******************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.07.00     NACL-多々納  14/05/12  介護保険情報等追加
      * 04.07.00     NACL-多々納  14/11/XX  初回・最終来院日他追加
      * 04.07.00     NACL-多々納  15/01/XX  労災自賠責編集追加
      * 04.08.00     NACL-多々納  16/10/XX  勤務先から帰省先追加
      * 04.08.00     NACL-多々納  17/04/XX  職業から入金方法追加等
      *                                     保険組合せ３０件へ
      * 04.08.00     NACL-多々納  17/05/XX  テスト患者・死亡区分追加
      * 05.00.00     NACL-多々納  18/10/XX  患者個別情報から禁忌薬剤追加
      * 05.00.00     NACL-多々納  19/06/XX  保険資格取得日など追加
      ************************************************************
        01  XML-PATIENTINFORES.
            02  PTRES-CONTEXT          PIC S9(9)   BINARY.
            02  PTRES-OBJECT           PIC X(36).
            02  PTRES-MODE             PIC S9(9)   BINARY.
            02  PTRES-RECORDNAME       PIC X(128).
            02  PTRES-PATIENTINFORES.
      *       実施日
              03  PTRES-INFORMATION-DATE   PIC X(10).
      *       実施時間
              03  PTRES-INFORMATION-TIME   PIC X(8).
      *       返却コード
              03  PTRES-API-RESULT         PIC X(2).
      *       返却メッセージ
              03  PTRES-API-RESULT-MSG     PIC X(100).
              03  PTRES-RESKEY             PIC X(100).
      *       患者情報
              03  PTRES-PATIENT-INFOMATION.
      *           患者番号
                  04  PTRES-PATIENTID          PIC X(20).
      *           漢字氏名
                  04  PTRES-NAME               PIC X(100).
      *           カナ氏名
                  04  PTRES-KANANAME           PIC X(100).
      *           生年月日
                  04  PTRES-BIRTHDAY           PIC X(10).
      *           性別
                  04  PTRES-SEX                PIC X(1).
      *           世帯主名
                  04  PTRES-SETAI-NAME         PIC X(100).
      *           続柄
                  04  PTRES-ZOKUGARA           PIC X(60).
      *           患者自宅情報
                  04  PTRES-ADDRESS-INFO.
      *               郵便番号
                      06  PTRES-HOME-ZIP-CODE      PIC X(7).
      *               住所
                      06  PTRES-HOME-ADDRESSES1    PIC X(200).
      *               番地番号
                      06  PTRES-HOME-ADDRESSES2    PIC X(200).
      *               自宅電話番号（昼）
                      06  PTRES-HOME-HPONE1        PIC X(15).
      *               自宅電話番号（昼）
                      06  PTRES-HOME-HPONE2        PIC X(15).
      *H28.10
      *           勤務先情報
                  04  PTRES-OFFICE-INFO.
      *               勤務先名
                      06  PTRES-OFFICE-NAME        PIC X(50).
      *               郵便番号
                      06  PTRES-OFFICE-ZIP-CODE    PIC X(7).
      *               住所
                      06  PTRES-OFFICE-ADDRESSES1  PIC X(100).
      *               番地番号
                      06  PTRES-OFFICE-ADDRESSES2  PIC X(100).
      *               電話番号
                      06  PTRES-OFFICE-HPONE        PIC X(15).
      *           連絡先情報
                  04  PTRES-RENRAKU-INFO.
      *               連絡先名
                      06  PTRES-RENRAKU-NAME        PIC X(50).
      *               連絡先続柄
                      06  PTRES-RENRAKU-ZOKUGARA    PIC X(30).
      *               郵便番号
                      06  PTRES-RENRAKU-ZIP-CODE    PIC X(7).
      *               住所
                      06  PTRES-RENRAKU-ADDRESSES1  PIC X(100).
      *               番地番号
                      06  PTRES-RENRAKU-ADDRESSES2  PIC X(100).
      *               電話番号　昼
                      06  PTRES-RENRAKU-HPONE1      PIC X(15).
      *               電話番号　夜
                      06  PTRES-RENRAKU-HPONE2      PIC X(15).
      *           帰省先情報
                  04  PTRES-KISEI-INFO.
      *               帰省先名
                      06  PTRES-KISEI-NAME          PIC X(50).
      *               郵便番号
                      06  PTRES-KISEI-ZIP-CODE      PIC X(7).
      *               住所
                      06  PTRES-KISEI-ADDRESSES1    PIC X(100).
      *               番地番号
                      06  PTRES-KISEI-ADDRESSES2    PIC X(100).
      *               電話番号
                      06  PTRES-KISEI-HPONE         PIC X(15).
      *
      *           禁忌１
                  04  PTRES-TABOO-1              PIC  X(200).
      *           禁忌２
                  04  PTRES-TABOO-2              PIC  X(200).
      *           アレルギー１
                  04  PTRES-ALLERGY-1            PIC  X(200).
      *           アレルギー２
                  04  PTRES-ALLERGY-2            PIC  X(200).
      *           感染症１
                  04  PTRES-KANSENSYO-1          PIC  X(200).
      *           感染症２
                  04  PTRES-KANSENSYO-2          PIC  X(200).
      *           コメント１
                  04  PTRES-COMMENT-1            PIC  X(200).
      *           コメント２
                  04  PTRES-COMMENT-2            PIC  X(200).
      *H29.5
      *           テスト患者区分
                  04  PTMODRES-TEST-PATIENT      PIC  X(01).
      *           死亡区分
                  04  PTMODRES-DEATHKBN          PIC  X(01).
      *H29.4
      *           職業
                  04  PTRES-JOB                PIC X(20).
      *           通称
                  04  PTRES-NICKNAME           PIC  X(50).
      *           携帯電話番号
                  04  PTRES-KEITAI-TEL         PIC  X(15).
      *           ＦＡＸ番号
                  04  PTRES-FAX                PIC  X(15).
      *           電子メールアドレス
                  04  PTRES-EMAIL              PIC  X(050).
      *           減免事由
                  04  PTRES-DISCOUNTKBN        PIC  X(02).
                  04  PTRES-DISCOUNTKBN-NAME   PIC  X(20).
      *           割引率
                  04  PTRES-DISCOUNT           PIC  X(02).
                  04  PTRES-DISCOUNT-NAME      PIC  X(14).
      *           状態１から３
                  04  PTRES-CONDITION-KBN-G.
                      05  PTRES-CONDITION-OCC    OCCURS   3.
                          07  PTRES-CONDITION-KBN     PIC X(02).
                          07  PTRES-CONDITION-NAME    PIC X(20).
      *           入金方法区分
                  04  PTRES-IC-CODE           PIC  X(02).
                  04  PTRES-IC-CODE-NAME      PIC  X(20).
      *H25.12
      *           地域連携ID
                  04  PTRES-COMMUNITY-CID        PIC  X(20).
      *           同意フラグ
                  04  PTRES-COMMUNITY-CID-AGREE  PIC  X(05).
      *H26.11
      *           初回来院日
                  04  PTRES-FIRST-DATE           PIC  X(10).
      *           最終来院日（退院日）
                  04  PTRES-LAST-DATE            PIC  X(10).
      *           入院中区分（１：入院中）
                  04  PTRES-OUTPATIENT-CLASS     PIC  X(01).
      *           入院日（入院中のみ）
                  04  PTRES-ADMISSION-DATE       PIC  X(10).
      *           退院日（当日退院時のみ）
                  04  PTRES-DISCHARGE-DATE       PIC  X(10).
      *
      *           保険情報
                  04  PTRES-INSURANCE-INFO     OCCURS  30 TIMES.
      *               保険組合せ番号
                      06  PTRES-COMBINATION-NUMBER     PIC X(04).
      *H26.11
      *               保険組合せ　負担割合（入院）
                      06  PTRES-COMBINATION-RATE-ADM   PIC X(04).
      *               保険組合せ　負担割合（外来）
                      06  PTRES-COMBINATION-RATE-OUT   PIC X(04).
      *H29.04
      *               非表示区分
                      06  PTRES-COMBINATION-HYOJI      PIC X(01).
      *
      *               保険の種類
                      06  PTRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *               保険者番号
                      06  PTRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *               保険の種類名称
                      06  PTRES-MAIN-INSURANCE-NAME    PIC X(20).
      *               記号
                      06  PTRES-MAIN-MARK              PIC X(80).
      *               番号
                      06  PTRES-MAIN-NUMBER            PIC X(80).
      *               枝番
                      06  PTRES-MAIN-EDABAN            PIC X(02).
      *               継続区分
                      06  PTRES-MAIN-CONTINUATION      PIC X(01).
      *               補助区分
                      06  PTRES-MAIN-ASSISTANCE        PIC X(01).
      *               補助区分名称
                      06  PTRES-MAIN-ASSISTANCE-NAME   PIC X(20).
      *               本人家族区分
                      06  PTRES-MAIN-FAMILY            PIC X(01).
      *               被保険者名
                      06  PTRES-MAIN-POLICYHOLDER      PIC X(100).
      *               有効開始日
                      06  PTRES-MAIN-START-DATE        PIC X(10).
      *               有効終了日
                      06  PTRES-MAIN-END-DATE          PIC X(10).
      *R01.06
      *               資格取得日
                      06  PTRES-MAIN-GET-DATE          PIC X(10).
      *H26.11
      *               最終確認日
                      06  PTRES-MAIN-CHECK-DATE        PIC X(10).
      *
      *               公費情報
                      06  PTRES-PUBLIC-INSURANCE-INFO           
                                              OCCURS  4 TIMES.
      *                 公費の種類
                        07  PTRES-INSURANCE-CLASS         PIC X(03).
      *                 公費の種類名称
                        07  PTRES-INSURANCE-NAME          PIC X(20).
      *                 負担者番号
                        07  PTRES-PROVIDER-NUMBER         PIC X(08).
      *                 受給者番号
                        07  PTRES-RECIPIENT-NUMBER        PIC X(20).
      *                 入院−負担率（割）
                        07  PTRES-RATE-ADMISSION          PIC X(04).
      *                 入院−固定額
                        07  PTRES-MONEY-ADMISSION         PIC X(06).
      *                 外来−負担率（割）
                        07  PTRES-RATE-OUTPATIENT         PIC X(04).
      *                 外来−固定額
                        07  PTRES-MONEY-OUTPATIENT        PIC X(06).
      *                 有効開始日
                        07  PTRES-INSURANCE-START-DATE    PIC X(10).
      *                 有効終了日
                        07  PTRES-INSURANCE-END-DATE      PIC X(10).
      *R01.06
      *                 最終確認日
                        07  PTRES-INSURANCE-CHECK-DATE    PIC X(10).
      *H27.1
      *労災情報
                     06  PTRES-ACCIDENT-INF-G.
      *                  労災保険名称（記号と同じ内容）
                         07  PTRES-ACCIDENT-NAME           PIC X(50).
      *                  傷病の部位（番号と同じ内容）
                         07  PTRES-DISEASE-LOCATION        PIC X(100).
      *                  傷病年月日
                         07  PTRES-DISEASE-DATE            PIC X(10).
      *労災
      *                  労働保険番号
                         07  PTRES-ACCIDENT-NUMBER         PIC X(14).
      *                  年金証書番号
                         07  PTRES-PENSIONCERTIFICATE      PIC X(09).
      *                  災害区分（名称）
                         07  PTRES-ACCIDENT-CLASS          PIC X(20).
      *                  労働基準監督署コード
                         07  PTRES-LABOR-ST-CODE           PIC X(05).
      *                  労働基準監督署コード
                         07  PTRES-LABOR-ST-NAME           PIC X(50).
      *                  事業所名称
                         07  PTRES-RSI-JIGYOUNAME          PIC X(80).
      *                  所在地都道府県名
                         07  PTRES-RSI-PREFNAME            PIC X(20).
      *                  所在地都道府県区分
                         07  PTRES-RSI-PREFKBN             PIC X(01).
      *                  所在地都道府県区分名称
                         07  PTRES-RSI-PREFKBNNAME         PIC X(02).
      *                  所在地郡市区名
                         07  PTRES-RSI-CITYNAME            PIC X(80).
      *                  所在地郡市区区分
                         07  PTRES-RSI-CITYKBN             PIC X(01).
      *                  所在地郡市区名称
                         07  PTRES-RSI-CITYKBNNAME         PIC X(02).
      *自賠責
      *                  自賠責保険会社名
                         07  PTRES-RSI-HOKENNAME           PIC X(80).
      *アフターケア
      *                  健康管理手帳番号
                         07  PTRES-RSI-KANRITCHO-NUMBER    PIC X(13).
      *                  損傷区分
                         07  PTRES-DAMAGE-CLASS            PIC X(03).
      *                  損傷区分名称
                         07  PTRES-DAMAGE-CLASS-NAME       PIC X(100).
      *
      *H26.5
      *           介護情報
                  04  PTRES-CARE-INF-G.
      *               介護保険情報
                      05  PTRES-CARE-INSURANCE-G    OCCURS   10.
      *                   保険者番号
                          07  PTRES-CARE-INSURANCE-NUMBER  PIC X(06).
      *                   被保険者番号
                          07  PTRES-CARE-POLICYHOLDER      PIC X(10).
      *                   有効開始日
                          07  PTRES-CARE-START-DATE        PIC X(10).
      *                   有効終了日
                          07  PTRES-CARE-END-DATE          PIC X(10).
      *               要介護認定情報
                      05  PTRES-CERTIFICATION-G    OCCURS   50.
      *                   要介護状態区分
                          07  PTRES-NEED-CARE-STATE-CODE   PIC X(02).
      *                   要介護状態名称
                          07  PTRES-NEED-CARE-STATE        PIC X(20).
      *                   認定日付
                          07  PTRES-NEED-DATE              PIC X(10).
      *                   有効開始日
                          07  PTRES-NEED-START-DATE        PIC X(10).
      *                   有効終了日
                          07  PTRES-NEED-END-DATE          PIC X(10).
      *           地域包括診療対象疾病
                  04  PTRES-COMMUNITY-DISEASE.
                      05  PTRES-TARGET-DISEASE     PIC X(05)
                                                   OCCURS   4.
      *H30.4
      *           患者個別情報
                  04  PTRES-PTCONF-INFO.
      *               妊婦設定
                      05  PTRES-NINPUKBN             PIC X(05).
      *H30.10
      *               認知症地域包括診療
                      05  PTRES-NINTIHOKATU          PIC X(05).
      *               小児かかりつけ診療
                      05  PTRES-SYOUNIKAKARI         PIC X(05).
      *
      *           患者個人番号情報
                  04  PTRES-PTMYNUMBER-G.
                      05  PTRES-PTMYNUMBER-TBL     OCCURS   20.
                          07  PTRES-ID-KEY           PIC X(10).
                          07  PTRES-MY-NUMBER        PIC X(20).
                          07  PTRES-DESCRIPTION      PIC X(100).
      *H30.10
      *
      *           管理料等自動算定
                  04  PTRES-AUTO-SRYCD-G.
                      05  PTRES-AUTO-SRYCD-TBL     OCCURS   3.
                          07  PTRES-AUTO-SRYCD       PIC X(09).
                          07  PTRES-AUTO-NAME        PIC X(80).
                          07  PTRES-AUTO-ENDYMD      PIC X(10).
      *
      *           患者禁忌薬剤
                  04  PTRES-KINKI-YKZAI-G.
                      05  PTRES-KINKI-YKZAI-TBL    OCCURS   100.
      *                   禁忌薬剤コード
                          07  PTRES-DRUGCD-CODE       PIC X(09).
                          07  PTRES-DRUGCD-NAME       PIC X(80).
      *                   有効終了日（マスタ終了日）
                          07  PTRES-DRUGCD-END-DATE   PIC X(10).
      *                   禁忌開始日
                          07  PTRES-START-DATE        PIC X(10).
      *
