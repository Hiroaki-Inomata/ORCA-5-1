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
      ************************************************************
      *         システム管理マスタ     (CPSK1001.INC)   *
      *         （１００１）                            *
      *         医療機関情報−基本情報                  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01.06.18  院外処方せん発行フラグ
      *                                     前回処方表示フラグ　　　追加
      *  01.00.02    NACL-森脇    02.08.16  薬剤情報発行フラグ追加
      *  01.00.03    NACL-多々納  03.02.10  病床数（一般）追加
      *  01.00.04    NACL-多々納  05.10.14  データ収集フラグ追加
      *  01.00.05    NACL-太田    05.10.14  自費保険集計先区分追加
      *  01.00.06    NACL-多々納  06.04.07  診療費明細書発行区分追加
      *  04.02.00    NACL-多々納  08.03.07  お薬手帳発行区分追加
      *  04.04.00    NACL-多々納  09.01.23  予約票発行区分追加
      *  04.04.00    NACL-小豆沢  09.01.26  本院分院対応
      *                                     短縮医療機関名称　追加       
      *  04.06.00    NACL-伊藤    10.02.13  データ感染症区分追加
      *  04.08.00    NACL-多々納  17.09.05  自費コード端数処理追加
      ************************************************************
       01  SYS-1001-REC.
           03  SYS-1001-KEY.
               05  SYS-1001-HOSPNUM     PIC 9(2).
      *管理コード
               05  SYS-1001-KANRICD     PIC  X(04).
      *区分コード
               05  SYS-1001-KBNCD       PIC  X(08).
      *有効開始年月日
               05  SYS-1001-STYUKYMD    PIC  X(08).
      *有効終了年月日
               05  SYS-1001-EDYUKYMD    PIC  X(08).
      *管理テーブル
           03  SYS-1001-TBL.
      *県番号
               05  SYS-1001-PREFNUM     PIC  9(02).
      *点数表区分
               05  SYS-1001-TENHYOKBN   PIC  9(01).
      *医療機関コード
               05  SYS-1001-HOSPCD      PIC  9(07).
      *医療機関種別
               05  SYS-1001-HOSPSBT     PIC  9(01).
      *医療機関ＩＤ
               05  SYS-1001-HOSPID      PIC  X(24).
      *医療機関名称
               05  SYS-1001-HOSPNAME    PIC  X(120).
      *開設者名
               05  SYS-1001-KAISETUNAME PIC  X(120).
      *管理者名
               05  SYS-1001-KANRINAME   PIC  X(80).
      *病床数
               05  SYS-1001-BEDSU       PIC  9(04).
      *老人支払区分
               05  SYS-1001-ROUPAYKBN   PIC  X(01).
      *旧総合病院
               05  SYS-1001-HOSPSBT1    PIC  9(01).
      *医療機関コード
               05  SYS-1001-HOSPCDN     PIC  X(40).
      *院外処方区分
               05  SYS-1001-SYOHOKBN    PIC  X(01).
      *請求書発行フラグ
               05  SYS-1001-SKYPRTFLG   PIC  X(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *院外処方せん発行フラグ
               05  SYS-1001-SYOHOPRTFLG      PIC  X(01).
      *前回処方表示フラグ
               05  SYS-1001-ZENSYOHOFLG      PIC  X(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *----(01.00.02) LINE ADD START ----------------------------------
      *薬剤情報発行フラグ
               05  SYS-1001-YAKUJYOPRTFLG    PIC  X(01).
      *病床数（一般）
               05  SYS-1001-BEDSUIPN    PIC  9(04).
      *請求額端数設定（減免有）
               05  SYS-1001-DISCOUNT-HASU-KBN
                                        PIC  X(01).
      *消費税端数設定
               05  SYS-1001-TAX-HASU-KBN
                                        PIC  X(01).
      *請求額端数設定医保（減免無・保険分）
               05  SYS-1001-DISCOUNT-HASU-KBN2
                                        PIC  X(01).
      *更正・育成限度額日割計算区分
               05  SYS-1001-HIWARIKBN   PIC  X(01).
      *データ作成フラグ
               05  SYS-1001-DATACREATEFLG    PIC  X(01).
      *データ提出方法区分
               05  SYS-1001-DATATEISYUTUKBN  PIC  X(01).
      *自費保険集計先区分
               05  SYS-1001-JIHIPOSITION     PIC  X(01).
      *診療費明細書発行区分
               05  SYS-1001-MEIPRTFLG        PIC  X(01).
      *地方公費保険番号タブ区分
               05  SYS-1001-CHIHOUNUMKBN     PIC  X(01).
      *減免計算対象区分
               05  SYS-1001-GENTAIKBN        PIC  X(01).
      *お薬手帳発行フラグ
               05  SYS-1001-OKUSURIPRTFLG    PIC  X(01).
      *予約票発行フラグ
               05  SYS-1001-YYKHYOPRTFLG     PIC  X(01).
      *短縮医療機関名称
               05  SYS-1001-TANHOSPNAME      PIC  X(20).
      *請求額端数設定医保（減免無・自費分）
               05  SYS-1001-DISCOUNT-HASU-KBN2J
                                        PIC  X(01).
      *請求額端数設定労災（減免無・保険分）
               05  SYS-1001-DISCOUNT-HASU-KBN3
                                        PIC  X(01).
      *請求額端数設定労災（減免無・自費分）
               05  SYS-1001-DISCOUNT-HASU-KBN3J
                                        PIC  X(01).
      *請求額端数設定自賠責（減免無・保険分）
               05  SYS-1001-DISCOUNT-HASU-KBN4
                                        PIC  X(01).
      *請求額端数設定自賠責（減免無・自費分）
               05  SYS-1001-DISCOUNT-HASU-KBN4J
                                        PIC  X(01).
      *請求額端数設定公害（減免無・自費分）
               05  SYS-1001-DISCOUNT-HASU-KBN5J
                                        PIC  X(01).
      *分娩機関管理番号
               05  SYS-1001-SANKANUM    PIC  9(10).
      *データ感染症区分
               05  SYS-1001-DATAINFECTIONKBN
                                        PIC  X(01).
      *請求額端数設定第三者行為（減免無・保険分）
               05  SYS-1001-DISCOUNT-HASU-KBN6
                                        PIC  X(01).
      *請求額端数設定第三者行為（減免無・自費分）
               05  SYS-1001-DISCOUNT-HASU-KBN6J
                                        PIC  X(01).
      *第三者行為（医療費）負担金額計算区分
               05  SYS-1001-DAISANKBN   PIC  X(01).
      *自費コード端数設定
               05  SYS-1001-JIHI-HASU-KBN
                                        PIC  X(01).
      *予備
      ***  03                           PIC  X(94).
      ***  03                           PIC  X(86).
      **** 03                           PIC  X(83).
      ***  03  SYS-1001-YOBI            PIC  X(42).
      ***  03  SYS-1001-YOBI            PIC  X(41).
      ***  03  SYS-1001-YOBI            PIC  X(38).
           03  SYS-1001-YOBI            PIC  X(37).
      *----(01.00.02) LINE ADD END   ----------------------------------
      *端末ＩＤ
           03  SYS-1001-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  SYS-1001-OPID            PIC  X(16).
      *作成年月日
           03  SYS-1001-CREYMD          PIC  X(08).
      *更新年月日
           03  SYS-1001-UPYMD           PIC  X(08).
      *更新時間
           03  SYS-1001-UPHMS           PIC  X(06).
      *主キー更新用項目
           03  SYS-1001-UPD-KEY.
               05  SYS-1001-UPD-HOSPNUM     PIC 9(2).
               05  SYS-1001-UPD-KANRICD     PIC X(04).
               05  SYS-1001-UPD-KBNCD       PIC X(08).
               05  SYS-1001-UPD-STYUKYMD    PIC X(08).
               05  SYS-1001-UPD-EDYUKYMD    PIC X(08).
