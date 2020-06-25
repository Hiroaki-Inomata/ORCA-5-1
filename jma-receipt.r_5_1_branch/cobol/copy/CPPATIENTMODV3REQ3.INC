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
      *                                   COPY (CPPATIENTMODV3REQ3.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       リクエスト番号
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID（電子カルテ）
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID（オルカ）
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       基準日付（システム日付）
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       確認メッセージ返答
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
      *       継続区分
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       患者情報
              03  PTMODREQ-PTINF-INFO.
      *         患者番号
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         患者氏名
                05  PTMODREQ-NAME             PIC X(100).
      *         患者カナ氏名
                05  PTMODREQ-KANANAME         PIC X(100).
      *         生年月日
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         性別
                05  PTMODREQ-SEX              PIC X(01).
      *
      *        労災保険情報
              03  PTMODREQ-ACCIDENT-INFO-G.
                 05  PTMODREQ-ACCIDENT-INFO     OCCURS   40.
      *            処理区分
                   07  PTMODREQ-MAIN-MODE              PIC X(06).
      *            労災保険ＩＤ
                   07  PTMODREQ-MAIN-INSURANCE-ID      PIC X(10).
      *            公費ＩＤ（第三者行為）
                   07  PTMODREQ-INSURANCE-ID           PIC X(10).
      *            保険の種類
                   07  PTMODREQ-INSURANCE-CLASS        PIC X(03).
      *            保険の種類名称
                   07  PTMODREQ-INSURANCE-NAME         PIC X(20).
      *労災情報
      *            労災保険区分
                   07  PTMODREQ-ACCIDENT              PIC X(01).
      *            労災保険名称
                   07  PTMODREQ-ACCIDENT-NAME          PIC X(50).
      *            傷病の部位
                   07  PTMODREQ-DISEASE-LOCATION       PIC X(100).
      *            傷病年月日
                   07  PTMODREQ-DISEASE-DATE           PIC X(10).
      *            療養開始日
                   07  PTMODREQ-START-DATE             PIC X(10).
      *            療養終了日
                   07  PTMODREQ-END-DATE               PIC X(10).
      *労災
      *            労働保険番号
                   07  PTMODREQ-ACCIDENT-NUMBER        PIC X(14).
      *            年金証書番号
                   07  PTMODREQ-PENSIONCERTIFICATE     PIC X(09).
      *            災害区分
                   07  PTMODREQ-ACCIDENT-CLASS         PIC X(01).
      **           災害区分名称
      **           07  PTMODREQ-ACCIDENT-CLASS-NAME    PIC X(20).
      *            労働基準監督署コード
                   07  PTMODREQ-LABOR-ST-CODE          PIC X(05).
      *            労働基準監督署コード
      **           07  PTMODREQ-LABOR-ST-NAME          PIC X(50).
      *
      *            新継再別区分
                   07  PTMODREQ-SINKEI                 PIC X(01).
      **           07  PTMODREQ-SINKEI-NAME            PIC X(10).
      *            転帰事由
                   07  PTMODREQ-TENKI                  PIC X(01).
      **           07  PTMODREQ-TENKI-NAME             PIC X(10).
      *            四肢特例区分
                   07  PTMODREQ-SISIKBN                PIC X(01).
      **           07  PTMODREQ-SISIKBN-NAME           PIC X(10).
      *
      *            事業所名称
                   07  PTMODREQ-JIGYOUNAME             PIC X(80).
      *            所在地都道府県名
                   07  PTMODREQ-PREFNAME               PIC X(20).
      *            所在地都道府県区分
                   07  PTMODREQ-PREFKBN                PIC X(01).
                   07  PTMODREQ-PREFKBN-NAME           PIC X(02).
      *            所在地郡市区名
                   07  PTMODREQ-CITYNAME               PIC X(80).
      *            所在地郡市区区分
                   07  PTMODREQ-CITYKBN                PIC X(01).
      *            所在地郡市区名称
                   07  PTMODREQ-CITYKBN-NAME           PIC X(02).
      *            労災レセ回数記載 基準年月
                   07  PTMODREQ-KIJYUNYM               PIC X(07).
      *            労災レセ回数記載 回数
                   07  PTMODREQ-RESEPUTO-COUNT         PIC X(03).
      *自賠責
      *            自賠責請求区分
                   07  PTMODREQ-JIBAISEIKBN            PIC X(01).
      *            自賠責保険会社名
                   07  PTMODREQ-HOKENNAME              PIC X(80).
      *            自賠責担当医
                   07  PTMODREQ-RSI-DRCD               PIC X(05).
      *            自賠責区分
                   07  PTMODREQ-JIBAIKBN               PIC  X(01).
      *アフターケア
      *            健康管理手帳番号
                   07  PTMODREQ-KANRITCHO-NUMBER       PIC X(13).
      *            損傷区分
                   07  PTMODREQ-DAMAGE-CLASS           PIC X(03).
      *            損傷区分名称
      **           07  PTMODREQ-DAMAGE-CLASS-NAME      PIC X(100).
      *第三者行為
      *            現物支給区分
                   07  PTMODREQ-DS-GENBTKBN            PIC X(01).
      **           07  PTMODREQ-DS-GENBTKBN-NAME       PIC X(20).
      *            特記事項区分
                   07  PTMODREQ-DS-TOKKIKBN            PIC X(01).
      **           07  PTMODREQ-DS-TOKKIKBN-NAME       PIC X(20).
      *
      *
