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
      *    ＡＰＩレスポンス医療機関情報取得
      *                                 COPY (CPAPISYS1001V2RES.INC)
      ******************************************************************
        01  XML-SYSTEM1001RES.
            02  SYS101RES-CONTEXT            PIC S9(9)   BINARY.
            02  SYS101RES-OBJECT             PIC X(36).
            02  SYS101RES-MODE               PIC S9(9)   BINARY.
            02  SYS101RES-RECORDNAME         PIC X(128).
            02  SYS101RES-APPOINTRES.
      *       実施日
              03  SYS101RES-INFORMATION-DATE    PIC X(10).
      *       実施時間
              03  SYS101RES-INFORMATION-TIME    PIC X(8).
      *       返却コード
              03  SYS101RES-API-RESULT          PIC X(2).
      *       返却メッセージ
              03  SYS101RES-API-RESULT-MSG      PIC X(100).
      *
              03  SYS101RES-RESKEY              PIC X(100).
      *       基準日
              03  SYS101RES-BASE-DATE           PIC X(10).
      *       医療機関情報
              03  SYS101RES-MEDICAL-INFOMATION.
      *           都道府県番号
                  05  SYS101RES-PREFECTURES-NUMBER  PIC X(02).
      *           点数表
                  05  SYS101RES-POINT-LIST          PIC X(01).
      *           医療機関コード
                  05  SYS101RES-INSTITUTION-CODE    PIC X(07).
      *           医療機関種別
                  05  SYS101RES-INSTITUTION-SPECIATION PIC X(01).
      *           医療機関ＩＤ
                  05  SYS101RES-INSTITUTION-ID         PIC X(24).
      *           医療機関名称
                  05  SYS101RES-INSTITUTION-WHOLENAME  PIC X(120).
      *           短縮医療機関名称
                  05  SYS101RES-TANHOSPNAME     PIC X(20).
      *           開設者名称
                  05  SYS101RES-ESTABLISHER-WHOLENAME  PIC X(120).
      *           管理者名称
                  05  SYS101RES-ADMINISTRATOR-WHOLENAME PIC X(80).
      *           病床数
                  05  SYS101RES-BEDSU           PIC X(04).
      *           病床数（一般）
                  05  SYS101RES-BEDSUIPN        PIC X(04).
      *           老人支払区分
                  05  SYS101RES-ROUPAYKBN       PIC X(01).
                  05  SYS101RES-ROUPAYKBNMEI    PIC X(06).
      *           旧総合病院
                  05  SYS101RES-HOSPSBT1        PIC X(01).
                  05  SYS101RES-HOSPSBT1MEI     PIC X(18).
      *           院外処方区分
                  05  SYS101RES-SYOHOKBN        PIC X(01).
                  05  SYS101RES-SYOHOKBNMEI     PIC X(06).
      *           医療機関コード（漢字）
                  05  SYS101RES-HOSPCDN         PIC X(40).
      *           分娩機関管理番号
                  05  SYS101RES-SANKANUM        PIC X(10).
      *           請求書発行フラグ
                  05  SYS101RES-SKYPRTFLG       PIC X(01).
                  05  SYS101RES-SKYPRTFLGMEI    PIC X(28).
      *           院外処方せん発行フラグ
                  05  SYS101RES-SYOHOPRTFLG     PIC X(01).
                  05  SYS101RES-SYOHOPRTFLGMEI  PIC X(28).
      *           前回処方表示フラグ
                  05  SYS101RES-ZENSYOHOFLG     PIC X(01).
                  05  SYS101RES-ZENSYOHOFLGMEI  PIC X(12).
      *           薬剤情報発行フラグ
                  05  SYS101RES-YAKUJYOPRTFLG   PIC X(01).
                  05  SYS101RES-YAKUJYOPRTFLGMEI
                                                PIC X(28).
      *           診療費明細書発行区分
                  05  SYS101RES-MEIPRTFLG       PIC X(01).
                  05  SYS101RES-MEIPRTFLGMEI    PIC X(30).
      *           お薬手帳発行フラグ
                  05  SYS101RES-OKUSURIPRTFLG   PIC X(01).
                  05  SYS101RES-OKUSURIPRTFLGMEI
                                                PIC X(34).
      *           予約票発行フラグ
                  05  SYS101RES-YYKHYOPRTFLG    PIC X(01).
                  05  SYS101RES-YYKHYOPRTFLGMEI PIC X(18).
      *           データ作成フラグ
                  05  SYS101RES-DATACREATEFLG   PIC X(01).
                  05  SYS101RES-DATACREATEFLGMEI
                                                PIC X(12).
      *           データ提出方法区分
                  05  SYS101RES-DATATEISYUTUKBN PIC X(01).
                  05  SYS101RES-DATATEISYUTUKBNMEI
                                                PIC X(12).
      *           データ感染症区分
                  05  SYS101RES-DATAINFECTIONKBN
                                                PIC X(01).
                  05  SYS101RES-DATAINFECTIONKBNMEI
                                                PIC X(12).
      *           減免計算対象区分
                  05  SYS101RES-GENTAIKBN       PIC X(01).
                  05  SYS101RES-GENTAIKBNMEI    PIC X(20).
      *           請求額端数設定（減免有）
                  05  SYS101RES-DISCOUNT-HASU-KBN
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBNNM
                                                PIC X(30).
      *           請求額端数設定医保（減免無・保険分）
                  05  SYS101RES-DISCOUNT-HASU-KBN2
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN2NM
                                                PIC X(30).
      *           請求額端数設定医保（減免無・自費分）
                  05  SYS101RES-DISCOUNT-HASU-KBN2J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN2JNM
                                                PIC X(30).
      *           請求額端数設定労災（減免無・保険分）
                  05  SYS101RES-DISCOUNT-HASU-KBN3
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN3NM
                                                PIC X(30).
      *           請求額端数設定労災（減免無・自費分）
                  05  SYS101RES-DISCOUNT-HASU-KBN3J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN3JNM
                                                PIC X(30).
      *           請求額端数設定自賠責（減免無・保険分）
                  05  SYS101RES-DISCOUNT-HASU-KBN4
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN4NM
                                                PIC X(30).
      *           請求額端数設定自賠責（減免無・自費分）
                  05  SYS101RES-DISCOUNT-HASU-KBN4J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN4JNM
                                                PIC X(30).
      *           請求額端数設定公害（減免無・自費分）
                  05  SYS101RES-DISCOUNT-HASU-KBN5J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN5JNM
                                                PIC X(30).
      *           請求額端数設定第三者行為（減免無・保険分）
                  05  SYS101RES-DISCOUNT-HASU-KBN6
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN6NM
                                                PIC X(30).
      *           請求額端数設定第三者行為（減免無・自費分）
                  05  SYS101RES-DISCOUNT-HASU-KBN6J
                                                PIC X(01).
                  05  SYS101RES-DISCOUNT-HASU-KBN6JNM
                                                PIC X(30).
      *           第三者行為（医療費）負担金額計算区分
                  05  SYS101RES-DAISANKBN       PIC X(01).
                  05  SYS101RES-DAISANKBNMEI    PIC X(30).
      *           消費税端数設定
                  05  SYS101RES-TAX-HASU-KBN    PIC X(01).
                  05  SYS101RES-TAX-HASU-KBNMEI PIC X(30).
      *           自費保険集計先区分
                  05  SYS101RES-JIHIPOSITION    PIC X(01).
                  05  SYS101RES-JIHIPOSITIONMEI PIC X(38).
      *           地方公費保険番号タブ区分
                  05  SYS101RES-CHIHOUNUMKBN    PIC X(01).
                  05  SYS101RES-CHIHOUNUMKBNMEI PIC X(08).
      *           更正・育成限度額日割計算区分
                  05  SYS101RES-HIWARIKBN       PIC X(01).
                  05  SYS101RES-HIWARIKBNMEI    PIC X(18).
      *           自費コード端数設定
                  05  SYS101RES-JIHI-HASU-KBN   PIC X(01).
                  05  SYS101RES-JIHI-HASU-KBNMEI
                                                PIC X(38).
      *           医療機関所在地
                  05  SYS101RES-ADDRESS-INFORMATION.
      *           所在地
                      07  SYS101RES-WHOLEADDRESS       PIC X(200).
      *           郵便番号
                      07  SYS101RES-ADDRESS-ZIPCODE    PIC X(07).
      *           電話番号
                      07  SYS101RES-PHONENUMBER        PIC X(15).
      *           FAX番号
                      07  SYS101RES-FAXNUMBER          PIC X(15).
      *           ｅメールアドレス
                      07  SYS101RES-EMAIL-ADDRESS      PIC X(100).
      *           ＨＰアドレス
                      07  SYS101RES-HOMEPAGE-ADDRESS   PIC X(100).
      *
