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
      *    ＡＰＩレスポンス患者保険組合せ取得(xml2)
      *                                   COPY (CPPATIENTLST6V2RES.INC)
      ******************************************************************
        01  XML-PATIENTLST6RES.
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
              03  PTRES-API-RESULT         PIC X(3).
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
      *       保険組合せ情報
              03  PTRES-INSURANCE-INFOMATION.
      *           保険組合せ情報
                  04  PTRES-INSURANCE-INFO     OCCURS  200 TIMES.
      *               保険組合せ番号
                      06  PTRES-COMB-NUMBER            PIC X(04).
      *               保険組合せ　負担割合（入院）
                      06  PTRES-COMB-RATE-ADM          PIC X(04).
      *               保険組合せ　負担割合（外来）
                      06  PTRES-COMB-RATE-OUT          PIC X(04).
      *               保険組合せ開始日
                      06  PTRES-COMB-START-DATE        PIC X(10).
                      06  PTRES-COMB-END-DATE          PIC X(10).
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
      *               資格取得日
                      06  PTRES-MAIN-GET-DATE          PIC X(10).
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
      *                  災害区分
                         07  PTRES-ACCIDENT-CLASS          PIC X(01).
      *                  災害区分（名称）
                         07  PTRES-ACCIDENT-CLASS-NAME     PIC X(20).
      *                  労働基準監督署コード
                         07  PTRES-LABOR-ST-CODE           PIC X(05).
      *                  労働基準監督署コード
                         07  PTRES-LABOR-ST-NAME           PIC X(50).
      *
      *                  新継再別区分
                         07  PTRES-SINKEI                  PIC X(01).
                         07  PTRES-SINKEI-NAME             PIC X(10).
      *                  転帰事由
                         07  PTRES-TENKI                   PIC X(01).
                         07  PTRES-TENKI-NAME              PIC X(10).
      *                  四肢特例区分
                         07  PTRES-SISIKBN                 PIC X(01).
                         07  PTRES-SISIKBN-NAME            PIC X(10).
      *
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
      *                  労災レセ回数記載 基準年月
                         07  PTRES-RSI-KIJYUNYM            PIC X(07).
      *                  労災レセ回数記載 回数
                         07  PTRES-RESEPUTO-COUNT          PIC X(03).
      *自賠責
      *                  自賠責請求区分
                         07  PTRES-JIBAISEIKBN             PIC X(01).
                         07  PTRES-JIBAISEIKBN-NAME        PIC X(20).
      *                  自賠責保険会社名
                         07  PTRES-RSI-HOKENNAME           PIC X(80).
      *                  自賠責担当医
                         07  PTRES-RSI-DRCD                PIC X(05).
      *                  自賠責担当医名称
                         07  PTRES-RSI-DRCD-NAME           PIC X(100).
      *アフターケア
      *                  健康管理手帳番号
                         07  PTRES-RSI-KANRITCHO-NUMBER    PIC X(13).
      *                  損傷区分
                         07  PTRES-DAMAGE-CLASS            PIC X(03).
      *                  損傷区分名称
                         07  PTRES-DAMAGE-CLASS-NAME       PIC X(100).
      *第三者行為
      *                  現物支給区分
                         07  PTRES-DS-GENBTKBN             PIC X(01).
                         07  PTRES-DS-GENBTKBN-NAME        PIC X(20).
      *                  特記事項区分
                         07  PTRES-DS-TOKKIKBN             PIC X(01).
                         07  PTRES-DS-TOKKIKBN-NAME        PIC X(20).
      *
      *
