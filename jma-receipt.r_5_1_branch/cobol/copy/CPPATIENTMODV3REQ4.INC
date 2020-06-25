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
      *                                   COPY (CPPATIENTMODV3REQ4.INC)
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
      *       低所得者２情報（低所得者履歴）
              03  PTMODREQ-TSYRRK-INFO-G.
      *          処理区分（MODIFY(NEW),DELETE)
                 05  PTMODREQ-TSYRRK-MODE        PIC X(06).
                 05  PTMODREQ-TSYRRK-INFO     OCCURS   20.
      *            認定開始年月日
                   07  PTMODREQ-TSYRRK-NINSTYMD    PIC X(10).
      *            認定終了年月日
                   07  PTMODREQ-TSYRRK-NINEDYMD    PIC X(10).
      *            標準負担額減額開始日
                   07  PTMODREQ-TSYRRK-SKJNINSTYMD PIC X(10).
      *            長期入院該当年月日
                   07  PTMODREQ-TSYRRK-SKJGNSTYMD  PIC X(10).
      *            認定範囲区分
                   07  PTMODREQ-TSYRRK-HKNTEKKBN   PIC X(01).
      *H29.6       境界層該当区分
                   07  PTMODREQ-TSYRRK-KYOUKAISOU  PIC X(01).
      *
      *       低所得者１情報（年金履歴）
              03  PTMODREQ-TNKRRK-INFO-G.
      *          処理区分
                 05  PTMODREQ-TNKRRK-MODE        PIC X(06).
                 05  PTMODREQ-TNKRRK-INFO        OCCURS   20.
      *            認定開始年月日
                   07  PTMODREQ-TNKRRK-STYMD       PIC X(10).
      *            認定終了年月日
                   07  PTMODREQ-TNKRRK-EDYMD       PIC X(10).
      *            標準負担額減額開始日
                   07  PTMODREQ-TNKRRK-SKJNINSTYMD PIC X(10).
      *            老齢福祉年金受給者証区分
                   07  PTMODREQ-TNKRRK-FUKUSHI-KBN PIC X(01).
      *            認定範囲区分
                   07  PTMODREQ-TNKRRK-HKNTEKKBN   PIC X(01).
      *H29.6       境界層該当区分
                   07  PTMODREQ-TNKRRK-KYOUKAISOU  PIC X(01).
      *
      *       旧姓履歴
              03  PTMODREQ-KYUSEIRRK-INFO-G.
      *           処理区分
                  05  PTMODREQ-KYUSEI-MODE        PIC X(06).
                  05  PTMODREQ-KYUSEIRRK-INFO     OCCURS   20.
      *            変更年月日
                   07  PTMODREQ-KYUSEI-CHGYMD      PIC X(10).
      *            旧姓カナ氏名
                   07  PTMODREQ-KYUSEI-KANANAME    PIC X(100).
      *            旧姓氏名
                   07  PTMODREQ-KYUSEI-NAME        PIC X(100).
      *            旧姓通称名
                   07  PTMODREQ-KYUSEI-NICKNAME    PIC X(100).
      *
      *       特記事項
              03  PTMODREQ-PTTOKKI-INFO-G.
      *          処理区分
                 05  PTMODREQ-PTTOKKI-MODE        PIC X(06).
                 05  PTMODREQ-PTTOKKI-INFO     OCCURS   50.
      *            入外区分
                   07  PTMODREQ-PTTOKKI-NYUGAIKBN   PIC X(01).
      *            レセ電区分
                   07  PTMODREQ-PTTOKKI-CD          PIC X(02).
      *            紙レセプト（特記区分内容）
                   07  PTMODREQ-PTTOKKI-NAIYO       PIC X(12).
      *            開始年月
                   07  PTMODREQ-PTTOKKI-STYM        PIC X(07).
      *            終了年月
                   07  PTMODREQ-PTTOKKI-EDYM        PIC X(07).
      *
      *       患者個別情報
              03  PTMODREQ-PTCONF-INFO.
      *           処理区分
                  05  PTMODREQ-PTCONF-MODE        PIC X(06).
      *           出生時体重
                  05  PTMODREQ-BIRTHWEIGHT         PIC X(04).
      *           地域包括診療対象疾病
                  05  PTMODREQ-CHKHKTBYO-G.
                      07  PTMODREQ-CHKHKTBYO       PIC X(05)
                                                   OCCURS   4.
                  05  PTMODREQ-CHKHKTBYO-G2    REDEFINES
                                               PTMODREQ-CHKHKTBYO-G.
      *               高血圧症
                      07  PTMODREQ-CHKHKTBYO-1      PIC X(05).
      *               糖尿病
                      07  PTMODREQ-CHKHKTBYO-2      PIC X(05).
      *               脂質異常症
                      07  PTMODREQ-CHKHKTBYO-3      PIC X(05).
      *               認知症
                      07  PTMODREQ-CHKHKTBYO-4      PIC X(05).
      *           認知症地域包括診療
                  05  PTMODREQ-NINTIHOKATU          PIC X(05).
      *           小児かかりつけ診療
                  05  PTMODREQ-SYOUNIKAKARI         PIC X(05).
      *H30.4      妊婦設定
                  05  PTMODREQ-NINPUKBN             PIC X(05).
      *
