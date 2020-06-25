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
      *    ＡＰＩ患者登録情報（所得者情報他情報）(一体化　V3)
      *                                   COPY (CPPATIENTMODV3RES4.INC)
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
             03  PTMODRES-RESKEY              PIC X(100).
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
      *
      *       低所得者２情報（低所得者履歴）
              03  PTMODRES-TSYRRK-INFO-G.
                 05  PTMODRES-TSYRRK-INFO     OCCURS   20.
      *            認定開始年月日
                   07  PTMODRES-TSYRRK-NINSTYMD    PIC X(10).
      *            認定終了年月日
                   07  PTMODRES-TSYRRK-NINEDYMD    PIC X(10).
      *            標準負担額減額開始日
                   07  PTMODRES-TSYRRK-SKJNINSTYMD PIC X(10).
      *            長期入院該当年月日
                   07  PTMODRES-TSYRRK-SKJGNSTYMD  PIC X(10).
      *            認定範囲区分
                   07  PTMODRES-TSYRRK-HKNTEKKBN   PIC X(01).
      *            認定範囲区分名称
                   07  PTMODRES-TSYRRK-HKNTEK-NAME PIC X(40).
      *H29.6       境界層該当区分
                   07  PTMODRES-TSYRRK-KYOUKAISOU   PIC X(01).
      *            境界層該当区分名称
                   07  PTMODRES-TSYRRK-KYOUKAI-NAME PIC X(20).
      *            エラーコード
                   07  PTMODRES-TSYRRK-ERRCD       PIC X(04).
                   07  PTMODRES-TSYRRK-ERRCD-MSG   PIC X(100).
      *
      *       低所得者１情報（年金履歴）
              03  PTMODRES-TNKRRK-INFO-G.
                 05  PTMODRES-TNKRRK-INFO        OCCURS   20.
      *            認定開始年月日
                   07  PTMODRES-TNKRRK-STYMD       PIC X(10).
      *            認定終了年月日
                   07  PTMODRES-TNKRRK-EDYMD       PIC X(10).
      *            標準負担額減額開始日
                   07  PTMODRES-TNKRRK-SKJNINSTYMD PIC X(10).
      *            老齢福祉年金受給者証区分
                   07  PTMODRES-TNKRRK-FUKUSHI-KBN PIC X(01).
      *            認定範囲区分
                   07  PTMODRES-TNKRRK-HKNTEKKBN   PIC X(01).
      *            認定範囲区分名称
                   07  PTMODRES-TNKRRK-HKNTEK-NAME PIC X(40).
      *H29.6       境界層該当区分
                   07  PTMODRES-TNKRRK-KYOUKAISOU   PIC X(01).
      *            境界層該当区分名称
                   07  PTMODRES-TNKRRK-KYOUKAI-NAME PIC X(20).
      *            エラーコード
                   07  PTMODRES-TNKRRK-ERRCD       PIC X(04).
                   07  PTMODRES-TNKRRK-ERRCD-MSG   PIC X(100).
      *
      *       旧姓履歴
              03  PTMODRES-KYUSEIRRK-INFO-G.
                  05  PTMODRES-KYUSEIRRK-INFO     OCCURS   20.
      *            変更年月日
                   07  PTMODRES-KYUSEI-CHGYMD      PIC X(10).
      *            旧姓カナ氏名
                   07  PTMODRES-KYUSEI-KANANAME    PIC X(100).
      *            旧姓氏名
                   07  PTMODRES-KYUSEI-NAME        PIC X(100).
      *            旧姓通称名
                   07  PTMODRES-KYUSEI-NICKNAME    PIC X(100).
      *            エラーコード
                   07  PTMODRES-KYUSEI-ERRCD       PIC X(04).
                   07  PTMODRES-KYUSEI-ERRCD-MSG   PIC X(100).
      *
      *       特記事項
              03  PTMODRES-PTTOKKI-INFO-G.
                 05  PTMODRES-PTTOKKI-INFO     OCCURS   50.
      *            入外区分
                   07  PTMODRES-PTTOKKI-NYUGAIKBN    PIC X(01).
                   07  PTMODRES-PTTOKKI-NYUGAI-NAME  PIC X(12).
      *            レセ電区分
                   07  PTMODRES-PTTOKKI-CD          PIC X(02).
      *            紙レセプト（特記区分内容）
                   07  PTMODRES-PTTOKKI-NAIYO       PIC X(12).
      *            開始年月
                   07  PTMODRES-PTTOKKI-STYM        PIC X(07).
      *            終了年月
                   07  PTMODRES-PTTOKKI-EDYM        PIC X(07).
      *            エラーコード
                   07  PTMODRES-PTTOKKI-ERRCD       PIC X(04).
                   07  PTMODRES-PTTOKKI-ERRCD-MSG   PIC X(100).
      *
      *       患者個別情報
              03  PTMODRES-PTCONF-INFO.
      *           出生時体重
                  05  PTMODRES-BIRTHWEIGHT         PIC X(04).
      *           地域包括診療対象疾病
                  05  PTMODRES-CHKHKTBYO-G.
                      07  PTMODRES-CHKHKTBYO       PIC X(05)
                                                   OCCURS   4.
                  05  PTMODRES-CHKHKTBYO-G2    REDEFINES
                                               PTMODRES-CHKHKTBYO-G.
      *               高血圧症
                      07  PTMODRES-CHKHKTBYO-1      PIC X(05).
      *               糖尿病
                      07  PTMODRES-CHKHKTBYO-2      PIC X(05).
      *               脂質異常症
                      07  PTMODRES-CHKHKTBYO-3      PIC X(05).
      *               認知症
                      07  PTMODRES-CHKHKTBYO-4      PIC X(05).
      *           認知症地域包括診療
                  05  PTMODRES-NINTIHOKATU          PIC X(05).
      *           小児かかりつけ診療
                  05  PTMODRES-SYOUNIKAKARI         PIC X(05).
      *H30.4      妊婦設定
                  05  PTMODRES-NINPUKBN             PIC X(05).
      *
      *      エラーメッセージ
             03  PTMODRES-PATIENT-ERRMSG-G.
                 05  PTMODRES-PATIENT-ERRMSG-INF  OCCURS   50.
                     07  PTMODRES-ERROR        PIC X(04).
                     07  PTMODRES-ERROR-MSG    PIC X(100).
      *
