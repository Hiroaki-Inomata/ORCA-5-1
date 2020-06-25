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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ患者登録情報（労災保険情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3RES3.INC)
      ******************************************************************
       01  XML-PATIENTMODRES.
           02  PTMODRES-CONTEXT           PIC S9(9)   BINARY.
           02  PTMODRES-OBJECT            PIC X(36).
           02  PTMODRES-MODE              PIC S9(9)   BINARY.
           02  PTMODRES-RECORDNAME        PIC X(128).
           02  PTMODRES-APPOINTRES.
      *      リクエスト番号
             03  PTMODRES-REQUEST-NUMBER    PIC X(2).
      *      レスポンス番号
             03  PTMODRES-RESPONSE-NUMBER   PIC X(2).
      *      UID（電子カルテ）
             03  PTMODRES-KARTE-UID         PIC X(36).
      *      UID（オルカ）
             03  PTMODRES-ORCA-UID          PIC X(36).
      *      継続区分
             03  PTMODRES-CONT-MODE         PIC X(05).
      *      実施日
             03  PTMODRES-INFORMATION-DATE  PIC X(10).
      *      実施時間
             03  PTMODRES-INFORMATION-TIME  PIC X(8).
      *      返却コード
             03  PTMODRES-API-RESULT        PIC X(3).
      *      返却メッセージ
             03  PTMODRES-API-RESULT-MSG    PIC X(100).
      *
             03  PTMODRES-RESKEY           PIC X(100).
      *      基準日付
             03  PTMODRES-BASE-DATE        PIC X(10).
      *      患者番号
             03  PTMODRES-PATIENTID        PIC X(20).
      *      患者情報
             03  PTMODRES-PTINF-INFO.
      *        患者氏名
               05  PTMODRES-NAME             PIC X(100).
      *        患者カナ氏名
               05  PTMODRES-KANANAME         PIC X(100).
      *        生年月日
               05  PTMODRES-BIRTHDAY         PIC X(10).
      *        性別
               05  PTMODRES-SEX              PIC X(01).
      *        自宅情報
               05  PTMODRES-ADDRESS-INFO.
      *            住所
                   07  PTMODRES-HOME-ADDRESSES1   PIC X(200).
      *            番地番号
                   07  PTMODRES-HOME-ADDRESSES2   PIC X(200).
      *
      *      労災保険情報
             03  PTMODRES-ACCIDENT-INFO-G.
                 05  PTMODRES-ACCIDENT-INFO     OCCURS   40.
      *            処理区分
                   07  PTMODRES-MAIN-MODE              PIC X(06).
      *            労災保険ＩＤ
                   07  PTMODRES-MAIN-INSURANCE-ID      PIC X(10).
      *            公費ＩＤ（第三者行為）
                   07  PTMODRES-INSURANCE-ID           PIC X(10).
      *            保険の種類
                   07  PTMODRES-INSURANCE-CLASS        PIC X(03).
      *            保険の種類名称
                   07  PTMODRES-INSURANCE-NAME         PIC X(20).
      *労災情報
      *            労災保険区分
                   07  PTMODRES-ACCIDENT               PIC X(01).
      *            労災保険名称
                   07  PTMODRES-ACCIDENT-NAME          PIC X(50).
      *            傷病の部位
                   07  PTMODRES-DISEASE-LOCATION       PIC X(100).
      *            傷病年月日
                   07  PTMODRES-DISEASE-DATE           PIC X(10).
      *            療養開始日
                   07  PTMODRES-START-DATE             PIC X(10).
      *            療養終了日
                   07  PTMODRES-END-DATE               PIC X(10).
      *労災
      *            労働保険番号
                   07  PTMODRES-ACCIDENT-NUMBER        PIC X(14).
      *            年金証書番号
                   07  PTMODRES-PENSIONCERTIFICATE     PIC X(09).
      *            災害区分
                   07  PTMODRES-ACCIDENT-CLASS         PIC X(01).
      *            災害区分名称
                   07  PTMODRES-ACCIDENT-CLASS-NAME    PIC X(20).
      *            労働基準監督署コード
                   07  PTMODRES-LABOR-ST-CODE          PIC X(05).
      *            労働基準監督署コード
                   07  PTMODRES-LABOR-ST-NAME          PIC X(50).
      *
      *            新継再別区分
                   07  PTMODRES-SINKEI                 PIC X(01).
                   07  PTMODRES-SINKEI-NAME            PIC X(10).
      *            転帰事由
                   07  PTMODRES-TENKI                  PIC X(01).
                   07  PTMODRES-TENKI-NAME             PIC X(10).
      *            四肢特例区分
                   07  PTMODRES-SISIKBN                PIC X(01).
                   07  PTMODRES-SISIKBN-NAME           PIC X(10).
      *
      *            事業所名称
                   07  PTMODRES-JIGYOUNAME             PIC X(80).
      *            所在地都道府県名
                   07  PTMODRES-PREFNAME               PIC X(20).
      *            所在地都道府県区分
                   07  PTMODRES-PREFKBN                PIC X(01).
                   07  PTMODRES-PREFKBN-NAME           PIC X(02).
      *            所在地郡市区名
                   07  PTMODRES-CITYNAME               PIC X(80).
      *            所在地郡市区区分
                   07  PTMODRES-CITYKBN                PIC X(01).
      *            所在地郡市区名称
                   07  PTMODRES-CITYKBN-NAME           PIC X(02).
      *            労災レセ回数記載 基準年月
                   07  PTMODRES-KIJYUNYM               PIC X(07).
      *            労災レセ回数記載 回数
                   07  PTMODRES-RESEPUTO-COUNT         PIC X(03).
      *自賠責
      *            自賠責請求区分
                   07  PTMODRES-JIBAISEIKBN            PIC X(01).
                   07  PTMODRES-JIBAISEIKBN-NAME       PIC X(20).
      *            自賠責保険会社名
                   07  PTMODRES-HOKENNAME              PIC X(80).
      *            自賠責担当医
                   07  PTMODRES-RSI-DRCD               PIC X(05).
      *            自賠責担当医名称
                   07  PTMODRES-RSI-DRCD-NAME          PIC X(100).
      *            自賠責区分
                   07  PTMODRES-JIBAIKBN               PIC  X(01).
      *            自賠責区分名称
                   07  PTMODRES-JIBAIKBN-NAME          PIC  X(30).
      *アフターケア
      *            健康管理手帳番号
                   07  PTMODRES-KANRITCHO-NUMBER       PIC X(13).
      *            損傷区分
                   07  PTMODRES-DAMAGE-CLASS           PIC X(03).
      *            損傷区分名称
                   07  PTMODRES-DAMAGE-CLASS-NAME      PIC X(100).
      *第三者行為
      *            現物支給区分
                   07  PTMODRES-DS-GENBTKBN            PIC X(01).
                   07  PTMODRES-DS-GENBTKBN-NAME       PIC X(20).
      *            特記事項区分
                   07  PTMODRES-DS-TOKKIKBN            PIC X(01).
                   07  PTMODRES-DS-TOKKIKBN-NAME       PIC X(20).
      *
      *
      *      保険組合せ情報
             03  PTMODRES-COMBINATION-INF-G.
               05  PTMODRES-COMBINATION-INF   OCCURS 200 TIMES.
      *            処理区分（新規・削除のみ）
                   07  PTMODRES-COMB-MODE             PIC X(06).
      *            保険組合せ番号
                   07  PTMODRES-COMBINATION-NUMBER     PIC X(04).
      *            保険組合せ　負担割合（入院）
                   07  PTMODRES-COMBINATION-RATE-ADM   PIC X(04).
      *            保険組合せ　負担割合（外来）
                   07  PTMODRES-COMBINATION-RATE-OUT   PIC X(04).
      *            保険組合せの有効開始日
                   07  PTMODRES-COMB-START-DATE        PIC X(10).
      *            保険組合せの有効終了日
                   07  PTMODRES-COMB-END-DATE          PIC X(10).
      *
      *            保険ＩＤ
                   07  PTMODRES-COMB-MAIN-ID           PIC X(10).
      *            保険の種類
                   07  PTMODRES-COMB-MAIN-CLASS        PIC X(03).
      *            保険者番号
                   07  PTMODRES-COMB-MAIN-NUMBER       PIC X(08).
      *            保険の種類名称
                   07  PTMODRES-COMB-MAIN-NAME         PIC X(20).
      *            記号
                   07  PTMODRES-COMB-MARK              PIC X(80).
      *            番号
                   07  PTMODRES-COMB-NUMBER            PIC X(80).
      *            枝番
                   07  PTMODRES-COMB-EDABAN            PIC X(02).
      *            継続区分
                   07  PTMODRES-COMB-CONTINUATION       PIC X(01).
      *            補助区分
                   07  PTMODRES-COMB-ASSISTANCE         PIC X(01).
      *            補助区分名称
                   07  PTMODRES-COMB-ASSI-NAME          PIC X(20).
      *            本人家族区分
                   07  PTMODRES-COMB-FAMILY             PIC X(01).
      *            公費情報
                   07  PTMODRES-COMB-PUBLIC-INS-INF
                                              OCCURS  4 TIMES.
      *                公費ＩＤ
                       09  PTMODRES-COMB-INSURANCE-ID     PIC X(10).
      *                公費の種類
                       09  PTMODRES-COMB-INSURANCE-CLASS  PIC X(03).
      *                公費の種類名称
                       09  PTMODRES-COMB-INSURANCE-NAME   PIC X(20).
      *                負担者番号
                       09  PTMODRES-COMB-PROVIDER-NUMBER  PIC X(08).
      *                受給者番号
                       09  PTMODRES-COMB-RECIPIENT-NUMBER PIC X(20).
      *                入院−負担率（割）
                       09  PTMODRES-RATE-ADMISSION        PIC X(04).
      *                入院−固定額
                       09  PTMODRES-MONEY-ADMISSION       PIC X(06).
      *                外来−負担率（割）
                       09  PTMODRES-RATE-OUTPATIENT       PIC X(04).
      *                外来−固定額
                       09  PTMODRES-MONEY-OUTPATIENT      PIC X(06).
      *
      *      エラーメッセージ
             03  PTMODRES-PATIENT-ERRMSG-G.
               05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                   07  PTMODRES-ERROR          PIC X(04).
                   07  PTMODRES-ERROR-MSG      PIC X(200).
      *      ワーニングメッセージ
             03  PTMODRES-WARNING-MSG-G.
                 05  PTMODRES-WARNING-INF    OCCURS   50.
                     07  PTMODRES-WARNING      PIC X(04).
                     07  PTMODRES-WARNING-MSG  PIC X(200).
      *      確認メッセージ
             03  PTMODRES-SELECT-MSG-G.
                 05  PTMODRES-SELECT-CODE      PIC X(04).
                 05  PTMODRES-SELECT-MSG       PIC X(200).
      *
