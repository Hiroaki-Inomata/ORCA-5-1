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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM039.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・総括表（医科・歯科）
      *                      ２００８．４改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/04/04    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    12/04/17  平成２４年４月改正対応
      *                                     平成２４年４月分以降はまとめる
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *
      *  05.00.01    NACL-藤原    18/11/29  肝がん治療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
           COPY    "HCM3E.INC".
           COPY    "HCM3F.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOH-END         PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *     
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *     
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-KOHNUM9         PIC X(03).
           03  WRK-NISSUZ          PIC ZZZZZZ.
           03  WRK-KENSUZ          PIC ZZZZZZ.
           03  WRK-TENSUZ          PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-FTNMONEYZ       PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-KINGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-FTNGAKZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-PAGE            PIC ZZZ9.
      *             
           03  WRK-KOHKBN.
               05  WRK-KOHNUM      PIC X(04).
               05  WRK-HKNNAME     PIC X(40). 
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-KAISETUNAME     PIC X(80).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *     
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TABLE.
      *        国民健康保険・老人保健         
               05  SUM-TBL             OCCURS  11.
                   07  SUM-TBL-G       OCCURS  2.
                       09  SUM-KENSU           PIC  9(06).
                       09  SUM-NISSU           PIC  9(06).
                       09  SUM-TOTALTEN        PIC  9(10).
                       09  SUM-FTNMONEY        PIC  9(10).
                   07  SUM-SHOKUJIKENSU        PIC  9(06).
                   07  SUM-SHOKUJINISSU        PIC  9(06).
                   07  SUM-SHOKUJIRYOYOHI      PIC  9(10).
                   07  SUM-SHOKUJIFTN          PIC  9(10).
      *             
      *        公費負担 
               05  SUM-KOH-TABLE. 
                   07  SUM-KOH-TBL     OCCURS  100.
                       09  SUM-KOH-KOHNUM      PIC  X(03).
                       09  SUM-TBL-G   OCCURS  2.
                           11  SUM-KOH-KENSU   PIC  9(06).
                           11  SUM-KOH-NISSU   PIC  9(06).
                           11  SUM-KOH-TOTALTEN
                                               PIC  9(10).
                           11  SUM-KOH-FTNMONEY
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIKENSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJINISSU
                                               PIC  9(06).
                       09  SUM-KOH-SHOKUJIRYOYOHI
                                               PIC  9(10).
                       09  SUM-KOH-SHOKUJIFTN  PIC  9(10).
                       09  SUM-KOH-MEISYO      PIC  X(44).
      *         
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
           03  WRK-PARA-PAGE       PIC 9(10).
      *
       01  WRK-SYOKBN              PIC X(01).
      *
       01  WRK-PRTFLG              PIC 9(01).
      *
       01  WRK-INDEX               PIC 9(02).
      *
       01  KEY-O-HKNJANUM          PIC X(08). 
      *
           COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //LNK-RECEERR//.
      *
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SUM-AREA
                                               WRK-PARA
                                               WRK-SYOKBN
                                               WRK-PRTFLG
                                               WRK-INDEX
                                               KEY-O-HKNJANUM
                                               SPA-AREA
                                               LNK-RECEERR
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM039 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    LNK-RECEERR         TO  RECEERR
      *
      *    システム管理マスタ（医療機関情報）
           PERFORM 120-HOSPINF-GET-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *
      *    診療年月編集
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
      *     
           MOVE    WRK-SRYYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)
                                   TO  WRK-SRYYMWH 
      *
      *    請求年月日編集
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA          
           IF      STS-DAY-RC1         =   ZERO 
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *    医療機関情報取得
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD (1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
      *
      *    医療機関情報−所在地、連絡先取得
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD (1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD 
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF    
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票編集<見出し>処理
           PERFORM 20021-HEAD-HEN-01-SEC
      *    帳票編集<見出し>処理
           PERFORM 20022-HEAD-HEN-02-SEC
      *    帳票編集<明細>処理
           PERFORM 20023-BODY-HEN-SEC
      *    帳票印刷処理
           PERFORM 20024-PRINT-OUT-SEC
      *
           MOVE    1                  TO  FLG-END
      *
           .
       2002-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       20021-HEAD-HEN-01-SEC          SECTION.
      *
           INITIALIZE                      HCM3E      
      *
           MOVE    WRK-SEIYMDWH        TO  HCM3E-SEIYMD
      *
           MOVE    WRK-SRYYMWH         TO  HCM3E-PRTYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM3E-ADRS
           INSPECT HCM3E-ADRS          REPLACING   ALL "  "    BY  "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM3E-HOSPNAME
           INSPECT HCM3E-HOSPNAME      REPLACING   ALL "  "    BY  "　"
      *    
           MOVE    WRK-KAISETUNAME     TO  HCM3E-KAISETUNAME
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM3E-HOSPCD
      *
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    KEY-O-HKNJANUM      TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-INV-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  HCM3E-HKNJANAME
           END-IF                                
           MOVE    KEY-O-HKNJANUM      TO  HCM3E-HKNJANUM
      *
           EVALUATE    WRK-INDEX
               WHEN    1
                   IF      WRK-PRTFLG          <   5
                       MOVE    "その他のレセ"  TO  HCM3E-TITLE
                   END-IF
               WHEN    2
                   MOVE    "在医総管または在医総を算定したレセ"
                                           TO  HCM3E-TITLE
           END-EVALUATE           
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"    TO  HCM3E-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-SYOKBN1
               WHEN    "2"
                   MOVE    "特別療養費"    TO  HCM3E-MOJI
           END-EVALUATE           
      *
           .
       20021-HEAD-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       20022-HEAD-HEN-02-SEC          SECTION.
      *
           INITIALIZE                      HCM3F
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM3F-HOSPCD 
      *
           EVALUATE    WRK-INDEX
               WHEN    1
                   IF      WRK-PRTFLG          <   5
                       MOVE    "その他のレセ"      TO  HCM3F-TITLE
                   END-IF 
               WHEN    2
                   MOVE    "在医総管または在医総を算定したレセ"
                                               TO  HCM3F-TITLE
           END-EVALUATE   
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"    TO  HCM3F-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-SYOKBN1
               WHEN    "2"
                   MOVE    "特別療養費"    TO  HCM3F-MOJI
           END-EVALUATE           
      *        
           MOVE    KEY-O-HKNJANUM      TO  HCM3F-HKNJANUM
      *
           .
       20022-HEAD-HEN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20023-BODY-HEN-SEC      SECTION.
      *
      *    国民健康保険
           MOVE    ZERO                TO  IDXX
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       9
             IF      IDX             =   6   OR  7
               CONTINUE
             ELSE
               ADD     1                   TO  IDXX
      * 
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       2
                   MOVE    SUM-KENSU   (IDX IDY)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM3E-KENSU (IDXX IDY)
                   MOVE    SUM-NISSU   (IDX IDY)
                                           TO  WRK-NISSUZ
                   MOVE    WRK-NISSUZ      TO  HCM3E-NISSU (IDXX IDY) 
                   MOVE    SUM-TOTALTEN(IDX IDY)
                                           TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ      TO  HCM3E-TENSU (IDXX IDY)
                   MOVE    SUM-FTNMONEY    (IDX IDY)
                                           TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ   TO  HCM3E-ITBFTN(IDXX IDY)
               END-PERFORM     
               MOVE    SUM-SHOKUJIKENSU(IDX)   TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3E-SYOKENSU (IDXX)
               MOVE    SUM-SHOKUJINISSU(IDX)   TO  WRK-NISSUZ
               MOVE    WRK-NISSUZ              TO  HCM3E-SYONISSU (IDXX)
               MOVE    SUM-SHOKUJIRYOYOHI(IDX) TO  WRK-KINGAKZ
               MOVE    WRK-KINGAKZ             TO  HCM3E-SYOKINGAK(IDXX)
               MOVE    SUM-SHOKUJIFTN   (IDX)  TO  WRK-FTNGAKZ
               MOVE    WRK-FTNGAKZ             TO  HCM3E-SYOFTNGAK(IDXX)
             END-IF
           END-PERFORM
      *     
      *    帳票印刷処理（１頁目）
           MOVE    ZERO                TO  FLG-PRTKBN
           PERFORM 20024-PRINT-OUT-SEC
           MOVE    1                   TO  FLG-PRTKBN
      *    公費負担分編集処理
           MOVE    ZERO                TO  FLG-KOH-END
           MOVE    1                   TO  IDX2
      * 
           PERFORM 200231-KOH-HENSYU-SEC
                                   UNTIL   FLG-KOH-END   =   1
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費負担分>処理
      *****************************************************************
      *    
       200231-KOH-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       2         
                   OR      FLG-KOH-END     =   1
               IF      SUM-KOH-KOHNUM (IDX2)   =   SPACE
                   MOVE    1                   TO  FLG-KOH-END
               ELSE    
                   PERFORM 2002311-HKNNUM-HENSYU-SEC     
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       2
                       MOVE    SUM-KOH-KENSU   (IDX2 IDZ)
                                           TO  WRK-KENSUZ
                       MOVE    WRK-KENSUZ  TO  HCM3F-KOH-KENSU (IDY IDZ)
                       MOVE    SUM-KOH-NISSU   (IDX2 IDZ)
                                           TO  WRK-NISSUZ
                       MOVE    WRK-NISSUZ  TO  HCM3F-KOH-NISSU (IDY IDZ) 
                       MOVE    SUM-KOH-TOTALTEN(IDX2 IDZ)
                                           TO  WRK-TENSUZ
                       MOVE    WRK-TENSUZ  TO  HCM3F-KOH-TENSU (IDY IDZ)
                       MOVE    SUM-KOH-FTNMONEY(IDX2 IDZ)
                                           TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ
                                           TO  HCM3F-KOH-ITBFTN(IDY IDZ)
                   END-PERFORM     
                   MOVE    SUM-KOH-SHOKUJIKENSU (IDX2)
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM3F-KOH-SYOKENSU (IDY)
                   MOVE    SUM-KOH-SHOKUJINISSU (IDX2)
                                           TO  WRK-NISSUZ
                   MOVE    WRK-NISSUZ      TO  HCM3F-KOH-SYONISSU (IDY)
                   MOVE    SUM-KOH-SHOKUJIRYOYOHI (IDX2)
                                           TO  WRK-KINGAKZ
                   MOVE    WRK-KINGAKZ     TO  HCM3F-KOH-SYOKINGAK(IDY)
                   MOVE    SUM-KOH-SHOKUJIFTN    (IDX2)
                                           TO  WRK-FTNGAKZ
                   MOVE    WRK-FTNGAKZ     TO  HCM3F-KOH-SYOFTNGAK(IDY)
      *             
                   ADD     1               TO  IDX2   
               END-IF        
           END-PERFORM
      *
           IF      SUM-KOH-KOHNUM (IDX2)   =   SPACE
               MOVE    1                   TO  FLG-KOH-END
           ELSE    
      *        帳票印刷処理（２頁以降）
               PERFORM 20024-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 20022-HEAD-HEN-02-SEC
           END-IF     
      *
           .
       200231-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       2002311-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHKBN
                                           WRK-KOHNUM9
      *
      **** IF      SUM-KOH-KOHNUM (IDX2) (1:1)
      ****                             =   "0"
      ****     MOVE    SUM-KOH-KOHNUM (IDX2)
      ****                                 TO  WRK-KOHNUM9
      **** ELSE
      *        法別番号取得
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE    SUM-KOH-KOHNUM (IDX2)
                                           TO  HKN-HKNNUM
               MOVE    WRK-SRYYMD          TO  HKN-TEKSTYMD
                                               HKN-TEKEDYMD
               MOVE    HKNNUM-REC          TO  MCPDATA-REC
               PERFORM 900-HKNNUM-INV-SEC
               IF      FLG-HKNNUM          =   ZERO
                   MOVE   HKN-HBTNUM           TO  WRK-KOHNUM9 (2:2)
               ELSE
                   MOVE    SUM-KOH-KOHNUM (IDX2)
                                               TO  WRK-KOHNUM9
               END-IF
      **** END-IF
           MOVE    WRK-KOHNUM9             TO  HCM3F-KOH-NUM (IDY)
      *
      *    特定疾患のとき 
           IF      SUM-KOH-KOHNUM (IDX2)    =   "051"
               MOVE    "特定疾患"          TO  HCM3F-KOH-NAME(IDY)
           ELSE
      *        公費番号まとめ情報からの集計か
               IF      SUM-KOH-MEISYO  (IDX2)
                                           =   SPACE
      *            保険番号読込み  
                   INITIALIZE                      HKNNUM-REC
                   MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE   SUM-KOH-KOHNUM (IDX2)
                                               TO    HKN-HKNNUM
                   MOVE   HKNNUM-REC           TO    MCPDATA-REC
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-HKNNUM-READ-SEC
                   ELSE
                       MOVE    1                  TO  FLG-HKNNUM
                   END-IF
      *
                   PERFORM UNTIL       FLG-HKNNUM      =   1
                     IF      HKN-TEKSTYMD            <=  WRK-SRYYMD
                     AND     HKN-TEKEDYMD            >=  WRK-SRYYMD
                         MOVE    HKN-TANSEIDONAME    TO  WRK-HKNNAME
                         MOVE    1                   TO  FLG-HKNNUM   
                     END-IF
      *
                     IF      FLG-HKNNUM           =   ZERO
                         MOVE    "tbl_hknnum"        TO  MCP-TABLE
                         MOVE    "key2"              TO  MCP-PATHNAME
                         PERFORM 900-HKNNUM-READ-SEC
                     END-IF
                   END-PERFORM
      *            カーソルクロース
                   MOVE    "tbl_hknnum"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
                   INSPECT WRK-HKNNAME  REPLACING   ALL "（" BY  "・"
                                                    ALL "）" BY  "　"  
                   MOVE    WRK-HKNNAME         TO  HCM3F-KOH-NAME(IDY)
               ELSE
                   MOVE    SUM-KOH-MEISYO (IDX2)
                                               TO  HCM3F-KOH-NAME(IDY)
               END-IF
           END-IF
           . 
       2002311-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20024-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           EVALUATE    FLG-PRTKBN
               WHEN    ZERO 
                   MOVE    WRK-PAGE            TO  HCM3E-PAGE
                   MOVE    "HCM3E.red"         TO  SPRT-PRTID
                   MOVE    HCM3E               TO  SPRT-PRTDATA
               WHEN    1                            
                   MOVE    WRK-PAGE            TO  HCM3F-PAGE
                   MOVE    "HCM3F.red"         TO  SPRT-PRTID
                   MOVE    HCM3F               TO  SPRT-PRTDATA
           END-EVALUATE        
           EVALUATE    WRK-INDEX    ALSO    WRK-SYOKBN
                                            ALSO    WRK-PARA-SYOKBN1
               WHEN    1            ALSO    "1"     
                                            ALSO   "1"
                   MOVE    "国保総括表"        TO  SPRT-TITLE
               WHEN    2            ALSO    "1"
                                            ALSO   "1"
                   MOVE    "国保総括表（在医総等）"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "国保総括表［返戻分］" 
                                               TO  SPRT-TITLE
               WHEN    2            ALSO    "2"
                                            ALSO   "1"
                   MOVE    "国保総括表（在医総等）［返戻分］"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "1"     
                                            ALSO   "2"
                   MOVE    "国保総括表［特別療養費］"
                                               TO  SPRT-TITLE
               WHEN    1            ALSO    "2"
                                            ALSO   "2"
                   MOVE    "国保総括表［返戻分］［特別療養費］" 
                                               TO  SPRT-TITLE
           END-EVALUATE        
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *     
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF                                                              
           .
       20024-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM039 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING   
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           ADD     CNT-PAGE        TO  WRK-PARA-PAGE
      *
           DISPLAY "*** ORCBM039 PAGE  "  CNT-PAGE 
           DISPLAY "*** ORCBM039 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD (1:6)
           END-IF
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8)  NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関名称編集情報（２）
                   PERFORM 900-1903-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)
                                   TO  SYS-1901-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD (1:6)
           END-IF
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)
                                   TO  SYS-1902-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD (1:6)
           END-IF
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報（２）読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1903-REC
           INITIALIZE                  SYS-1903-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1903-HOSPNUM
           MOVE    "1903"          TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)
                                   TO  SYS-1903-KBNCD
           IF      WRK-PARA-STSRYYM    =   "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD (1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD (1:6)
           END-IF
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                      SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-KEY10-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ読込
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNNUM
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCBM039 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
