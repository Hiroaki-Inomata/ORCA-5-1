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
      *    ＡＰＩレスポンス中途データ情報取得(xml2)
      *                                      COPY (CPTMDCMODV2RES.INC)
      ******************************************************************
       01  XML-TMEDICALMODRES.
            03  TMDCRES-CONTEXT             PIC S9(9)   BINARY.
            03  TMDCRES-OBJECT              PIC X(36).
            03  TMDCRES-MODE                PIC S9(9)   BINARY.
            03  TMDCRES-RECORDNAME          PIC X(128).
            03  TMDCRES-MEDICALRES.
      *        実施日
               05  TMDCRES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               05  TMDCRES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               05  TMDCRES-API-RESULT        PIC X(3).
      *        返却メッセージ
               05  TMDCRES-API-RESULT-MSG    PIC X(100).
      *
               05  TMDCRES-RESKEY            PIC X(100).
      *
      *        診療日付
               05  TMDCRES-PERFORM-DATE      PIC X(10).
      *        患者情報
               05  TMDCRES-PATIENT-INFOMATION.
      *            患者番号
                   07  TMDCRES-PATIENTID        PIC X(20).
      *            患者氏名
                   07  TMDCRES-NAME             PIC X(100).
      *            患者カナ氏名
                   07  TMDCRES-KANANAME         PIC X(100).
      *            誕生日
                   07  TMDCRES-BIRTHDAY         PIC X(10).
      *            性別
                   07  TMDCRES-SEX              PIC X(1).
      *        入外区分
               05  TMDCRES-OUTPATIENT-CLASS     PIC X(1).
      *        同日再入院（１：同日再入院）
               05  TMDCRES-DOUJITSUKBN          PIC X(01).
      *        診療科
               05  TMDCRES-DEP-CODE             PIC X(02).
      *        診療科名称
               05  TMDCRES-DEP-NAME             PIC X(20).
      *        ドクターコード
               05  TMDCRES-DR-CODE              PIC X(5).
      *        ドクター氏名
               05  TMDCRES-DR-NAME              PIC X(80).
      *        保険組合せ情報
               05  TMDCRES-INSURANCE-INFO.
      *            保険組合せ番号
                   07  TMDCRES-COMBINATION-NUMBER   PIC X(04).
      *            保険の種類
                   07  TMDCRES-MAIN-INSURANCE-CLASS   PIC X(03).
      *            保険の種類名称
                   07  TMDCRES-MAIN-INSURANCE-NAME    PIC X(20).
      *            保険者番号
                   07  TMDCRES-MAIN-INSURANCE-NUMBER  PIC X(08).
      *            記号
                   07  TMDCRES-MAIN-MARK              PIC X(80).
      *            番号
                   07  TMDCRES-MAIN-NUMBER            PIC X(80).
      *            公費情報
                   07  TMDCRES-PUBLIC-INSURANCE-INFO
                                              OCCURS  4 TIMES.
      *               公費の種類
                      09  TMDCRES-INSURANCE-CLASS         PIC X(03).
      *               公費の種類名称
                      09  TMDCRES-INSURANCE-NAME          PIC X(20).
      *               負担者番号
                      09  TMDCRES-PROVIDER-NUMBER         PIC X(08).
      *               受給者番号
                      09  TMDCRES-RECIPIENT-NUMBER        PIC X(20).
      *            保険組合せ番号エラー
                   07  TMDCRES-COMBINATION-ERR            PIC X(01).
      *
      *        電子カルテキー（ＵＩＤ）
               05  TMDCRES-MEDICAL-UID         PIC X(36).
      *        展開中区分
               05  TMDCRES-MODE-FLG            PIC X(01).
      *HAORI処理
      *        UID（電子カルテ）
               05  TMDCRES-KARTE-UID         PIC X(36).
      *        UID（オルカ）
               05  TMDCRES-ORCA-UID          PIC X(36).
      *
      *        診療内容返却
               05  TMDCRES-MEDICAL-INF-G.
                   07  TMDCRES-MDC-INFO          OCCURS   120.
      *                診療区分
                       09  TMDCRES-MDC-CLASS-KBN         PIC X(2).
      *                診療種別区分
                       09  TMDCRES-MDC-CLASS             PIC X(3).
      *                診療種別名称
                       09  TMDCRES-MDC-CLASS-NAME        PIC X(40).
      *                回数
                       09  TMDCRES-MDC-CLASS-NUMBER      PIC X(03).
      *                入院　算定日
                       09  TMDCRES-MDC-DAY-G.
                           11  TMDCRES-MDC-DAY           PIC X(01)
                                                      OCCURS   31.
      *
                       09  TMDCRES-PRSCRPT-INFO-G.
                           10  TMDCRES-PRSCRPT-INFO    OCCURS   50.
      *                        診療コード
                               11  TMDCRES-PRSCRPT-CODE    PIC X(9).
      *                        診療名称
                               11  TMDCRES-PRSCRPT-NAME    PIC X(140).
      *                        数量
                               11  TMDCRES-PRSCRPT-NUMBER  PIC X(11).
      *                        単位コード
                               11  TMDCRES-PRSCRPT-TANICD  PIC X(03).
      *                        単位名称
                               11  TMDCRES-PRSCRPT-TANICD-NAME
                                                           PIC X(24).
      *                        自費金額
                               11  TMDCRES-PRSCRPT-MONEY   PIC X(07).
      *                        コメント埋め込み値
                               11  TMDCRES-PRSCRPT-ATAI-G.
                                   13  TMDCRES-PRSCRPT-ATAI  PIC X(10)
                                                         OCCURS   5.
      *                        フィルム分画数
                               11  TMDCRES-PRSCRPT-FILMNUM   PIC X(03).
      *                        継続指示区分
                               11  TMDCRES-PRSCRPT-CONTKBN   PIC X(01).
      *                        内服種類数指示区分
                               11  TMDCRES-PRSCRPT-NAIFKBN   PIC X(01).
      *
