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
       PROGRAM-ID.             ORCBM037.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 社保・診療報酬請求書（医科・入院外）
      *                      ２００８．４改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/04/04    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-藤原    08/09/24  平成２０年１０月改正対応
      *
      *  04.05.01    NACL-藤原    11/04/04  災害対応
      *                                     備考欄の記載等追加
      *  04.05.02    NACL-藤原    11/04/18  災害対応
      *                                     確定分の記載を中止
      *  04.05.03    NACL-藤原    12/04/17  平成２４年４月改正対応
      *                                     平成２４年４月分以降はまとめる
      *  04.05.04    NACL-藤原    12/09/10  一部負担金の記載対応
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-藤原    12/02/13  健保組合への直接請求と審査
      *                                     支払機関請求対応
      *  04.07.03    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    16/06/13  社保診療報酬請求書の災害該当
      *                                     分のまとめ対応
      *  04.08.03    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
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
           COPY    "HCM3CV02.INC"  REPLACING  //HCM3CV02//
                                   BY         //HCM3C//.
      *****COPY    "HCM3D.INC".
           COPY    "HCM3DV01.INC"  REPLACING  //HCM3DV01//
                                   BY         //HCM3D//.
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
      *
           03  FLG-MF01-END        PIC 9(01).
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).      
           03  IDX3                PIC 9(04).      
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
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ERRMSG          PIC X(300). 
      *
           03  WRK-Z9              PIC ZZZZZ.
           03  WRK-ZZ9             PIC ZZ,ZZZ,ZZZ.
           03  WRK-NISSUZ          PIC ZZZZZZ.
           03  WRK-KENSUZ          PIC ZZZZZZ.
           03  WRK-TENSUZ          PIC ZZ,ZZZ,ZZZ.
           03  WRK-FTNMONEYZ       PIC Z,ZZZ,ZZZ. 
           03  WRK-GKENSUZ         PIC ZZZZZZZ. 
      * 
           03  WRK-NISSU9          PIC ZZZZZ9.
           03  WRK-KENSU9          PIC ZZZZZ9.
           03  WRK-TENSU9          PIC ZZ,ZZZ,ZZ9.
           03  WRK-FTNMONEY9       PIC Z,ZZZ,ZZ9. 
      *
           03  WRK-SPRT-TITLE      PIC X(100).
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
           COPY    "CPSHELLTBL.INC".
      *
      *    固定項目
       01  WRK-CONS-AREA.
      *    ２００８年１０月改正
           03  WRK-CONS-SRYYM-200810   PIC X(06)   VALUE   "200810".
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
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *    
           COPY    "CPSK1005.INC".
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
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
         02  SUM-TABLE-OCC         OCCURS  100.
           03  SUM-TBL-SRYKA       PIC X(02).
      *    1:その他のレセ、2:在総診を算定したレセ 
           03  SUM-TBL-OCC         OCCURS  2.
      *        医療保険　1,2:７０以上 3:本人 4:家族 5:三歳 
               05  SUM-TABLE           OCCURS  5. 
                   07  SUM-TBL             OCCURS  12.
                       09  SUM-KENSU          PIC  9(06).
                       09  SUM-NISSU          PIC  9(06).
                       09  SUM-TOTALTEN       PIC  9(10).
                       09  SUM-FTNMONEY       PIC  9(10).
                       09  SUM-YKZFTN         PIC  9(10).
                       09  SUM-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-SHOKUJINISSU   PIC  9(06).
                       09  SUM-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-SHOKUJIFTN     PIC  9(10).
      *        老人保健
               05  SUM-ROU-TABLE           OCCURS  2. 
                   07  SUM-ROU-TBL             OCCURS  3.
                       09  SUM-ROU-KENSU          PIC  9(06).
                       09  SUM-ROU-NISSU          PIC  9(06).
                       09  SUM-ROU-TOTALTEN       PIC  9(10).
                       09  SUM-ROU-FTNMONEY       PIC  9(10).
                       09  SUM-ROU-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-ROU-SHOKUJINISSU   PIC  9(06).
                       09  SUM-ROU-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-ROU-SHOKUJIFTN     PIC  9(10).
               05  SUM-ROU-GOKEI.
                   07  SUM-ROU-GOK-NISSU          PIC  9(06).
                   07  SUM-ROU-GOK-TOTALTEN       PIC  9(10).
                   07  SUM-ROU-GOK-FTNMONEY       PIC  9(10).
                   07  SUM-ROU-GOK-SHOKUJINISSU   PIC  9(06).
                   07  SUM-ROU-GOK-SHOKUJIRYOYOHI PIC  9(10).
                   07  SUM-ROU-GOK-SHOKUJIFTN     PIC  9(10).
      *        公費負担 
               05  SUM-KOH-TABLE           OCCURS  3. 
                   07  SUM-KOH-TBL         OCCURS  100.
                       09  SUM-KOH-KOHNUM         PIC  X(03).
                       09  SUM-KOH-KENSU          PIC  9(06).
                       09  SUM-KOH-TOTALTEN       PIC  9(10).
                       09  SUM-KOH-FTNMONEY       PIC  9(10).
                       09  SUM-KOH-YKZFTN         PIC  9(10).
                       09  SUM-KOH-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-KOH-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-KOH-SHOKUJIFTN     PIC  9(10).
                       09  SUM-KOH-MEISYO         PIC  X(44).
      *        老人保健（再掲）      
               05  SUM-ROU-KOH-TABLE           OCCURS  10. 
                   07  SUM-ROU-KOH-TBL.
                       09  SUM-ROU-KOH-KENSU          PIC  9(06).
                       09  SUM-ROU-KOH-NISSU          PIC  9(06).
                       09  SUM-ROU-KOH-ITBFTN         PIC  9(08).
                       09  SUM-ROU-KOH-FTNMONEY       PIC  9(09).
                       09  SUM-ROU-KOH-SHOKUJIKENSU   PIC  9(06).
                       09  SUM-ROU-KOH-SHOKUJINISSU   PIC  9(06).
                       09  SUM-ROU-KOH-SHOKUJIRYOYOHI PIC  9(10).
                       09  SUM-ROU-KOH-SHOKUJIFTN     PIC  9(10).
      *        合計         
               05  SUM-GOK-TBL         OCCURS  4.
                   07  SUM-GOK-KENSU      PIC 9(06).    
                   07  SUM-GOK-SHOKENSU   PIC 9(06).
      *
      *        備考欄（未確定分） 
               05  SUM-BIKOU. 
                   07  SUM-BIKOU-KENSU          PIC  9(06).
                   07  SUM-BIKOU-NISSU          PIC  9(06).
                   07  SUM-BIKOU-TOTALTEN       PIC  9(10).
                   07  SUM-BIKOU-FTNMONEY       PIC  9(10).
                   07  SUM-BIKOU-SHOKUJIKENSU   PIC  9(06).
                   07  SUM-BIKOU-SHOKUJINISSU   PIC  9(06).
                   07  SUM-BIKOU-SHOKUJIRYOYOHI PIC  9(10).
                   07  SUM-BIKOU-SHOKUJIFTN     PIC  9(10).
      *         
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-NYUGAIKBN  PIC X(01).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-MARUCHO    PIC X(01).
           03  WRK-PARA-GENMENKBN  PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
       01  WRK-PARA1.
           03  WRK-PARA-HKNJANUM-S PIC X(08).
           03  WRK-PARA-HKNJANAME  PIC X(30).
      *
       01  WRK-SYOKBN              PIC X(01).
      *
       01  WRK-PRTFLG              PIC 9(01).
      *
           COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //LNK-RECEERR//.
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SUM-AREA
                                               WRK-PARA
                                               WRK-SYOKBN
                                               WRK-PRTFLG
                                               SPA-AREA
                                               LNK-RECEERR
                                               WRK-PARA1
           .
      *
      *****************************************************************
      *    主処理
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
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM037 START ***"
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    LNK-RECEERR         TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
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
      *    システム日付セット
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
      *    医療機関情報
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
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
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
      *    医療機関情報−所在地、連絡先
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
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
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
      *
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-MF01-END 
      *
           IF      SUM-TBL-SRYKA (2)   =   SPACE
               MOVE    100                 TO  IDXX
           ELSE
               MOVE    1                   TO  IDXX
           END-IF
      *       
           PERFORM VARYING IDX3    FROM   IDXX    BY  1
                   UNTIL   IDX3    >      100
               IF    ( SUM-TBL-SRYKA (IDX3)   =   SPACE )
               AND   ( IDX3                   <   100   )
                   CONTINUE
               ELSE
      *            主科名称
                   IF      SUM-TBL-SRYKA (IDX3)   =   "00"
                       MOVE    "主科未設定"       TO  SYS-1005-SRYKANAME
                   ELSE
                       INITIALIZE                  SYS-1005-REC
                       MOVE    "1005"          TO  SYS-1005-KANRICD
                       MOVE    SUM-TBL-SRYKA (IDX3)
                                               TO  SYS-1005-KBNCD
                       MOVE    LNK-PRTKANRI-SRYYM
                                               TO SYS-1005-STYUKYMD(1:6)
                       MOVE    "01"            TO SYS-1005-STYUKYMD(7:2)
                       MOVE    SYS-1005-STYUKYMD
                                               TO  SYS-1005-EDYUKYMD 
                       MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
                       MOVE    SYS-1005-REC    TO  MCPDATA-REC
                       PERFORM 800-SYSKANRI-READ-SEC
                       IF      FLG-SYSKANRI    =   ZERO
                           MOVE    MCPDATA-REC     TO  SYS-1005-REC
                       ELSE
                           INITIALIZE                  SYS-1005-REC
                       END-IF
                   END-IF
      *        
                   PERFORM VARYING IDX1    FROM   1   BY  1
                           UNTIL   IDX1    >      2
                       IF    ( SUM-GOK-KENSU (IDX3 IDX1 4) >   ZERO )
                       OR    ( SUM-BIKOU-KENSU (IDX3 IDX1) >   ZERO )
                           MOVE    ZERO            TO  FLG-PRTKBN
      *           
      *                    帳票編集<見出し・ＨＣＭ３Ｃ>処理
                           PERFORM 310-HEAD-HEN-01-SEC
      *                    帳票編集<見出し・ＨＣＭ３Ｄ>処理
                           PERFORM 310-HEAD-HEN-02-SEC
      *                    帳票編集<明細>処理
                           PERFORM 320-BODY-HEN-SEC
      *                    帳票編集<合計>処理
                           PERFORM 330-TERM-HEN-SEC
      *                    帳票印刷処理（２頁以降）
                           PERFORM 390-PRINT-OUT-SEC
                       END-IF    
                   END-PERFORM 
               END-IF   
           END-PERFORM    
      *
           MOVE    1                  TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し・ＨＣＭ３Ｃ>処理
      *****************************************************************
       310-HEAD-HEN-01-SEC          SECTION.
      *
           INITIALIZE                      HCM3C
      *
           IF      LNK-PRTKANRI-SRYYM  <   WRK-CONS-SRYYM-200810
               MOVE    "（政）"            TO  HCM3C-SEIDONAME (1)
                                               HCM3C-SEIDONAME (2)
                                               HCM3C-SEIDONAME (3)
                                               HCM3C-SEIDONAME (4)
                                               HCM3C-SEIDONAME (5)
           ELSE
               MOVE    "（協会）"          TO  HCM3C-SEIDONAME (1)
                                               HCM3C-SEIDONAME (2)
                                               HCM3C-SEIDONAME (3)
                                               HCM3C-SEIDONAME (4)
                                               HCM3C-SEIDONAME (5)
           END-IF
      *
           MOVE    WRK-SEIYMDWH        TO  HCM3C-SEIYMD
      *
           MOVE    WRK-SRYYMWH         TO  HCM3C-PRTYM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM3C-ADRS
           INSPECT HCM3C-ADRS          REPLACING   ALL "  "    BY  "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM3C-HOSPNAME
           INSPECT HCM3C-HOSPNAME      REPLACING   ALL "  "    BY  "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME     TO  HCM3C-KAISETUNAME
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM3C-HOSPCD
      *
           EVALUATE    IDX1
               WHEN    1
                   IF      WRK-PRTFLG          <   5
                       MOVE    "その他のレセ"      TO  HCM3C-TITLE
                   END-IF
               WHEN    2
                   IF      WRK-PRTFLG          =   1
                       MOVE    "在総診または在医総を算定したレセ"
                                                   TO  HCM3C-TITLE
                   ELSE   
                       MOVE    "在医総管または在医総を算定したレセ"
                                                   TO  HCM3C-TITLE
                   END-IF
           END-EVALUATE
      * 
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"  TO  HCM3C-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-MARUCHO
               WHEN    "2"
                   MOVE    "◯"          TO  HCM3C-MARUCHO (1)
                   MOVE    "長"          TO  HCM3C-MARUCHO (2)
           END-EVALUATE           
      *
      *    新潟県中越沖地震対応
           EVALUATE    WRK-PARA-GENMENKBN
               WHEN    ZERO
               WHEN    "9"
                   MOVE    SYS-1005-SRYKANAME    TO  HCM3C-SRYKA
               WHEN    "1"
                   STRING  "（未確定分）"      DELIMITED   BY  SIZE
                           SYS-1005-SRYKANAME  DELIMITED   BY  SPACE  
                                               INTO  HCM3C-SRYKA
                   END-STRING
           END-EVALUATE
           .
       310-HEAD-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し・ＨＣＭ３Ｄ>処理
      *****************************************************************
       310-HEAD-HEN-02-SEC          SECTION.
      *
           INITIALIZE                      HCM3D
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM3D-HOSPCD 
      *
           EVALUATE    IDX1
               WHEN    1
                   IF      WRK-PRTFLG          <   5
                       MOVE    "その他のレセ"      TO  HCM3D-TITLE
                   END-IF
               WHEN    2
                   IF      WRK-PRTFLG          =   1
                       MOVE    "在総診または在医総を算定したレセ"
                                                   TO  HCM3D-TITLE     
                   ELSE   
                       MOVE    "在医総管または在医総を算定したレセ"
                                                   TO  HCM3D-TITLE
                   END-IF
           END-EVALUATE
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"  TO  HCM3D-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-MARUCHO
               WHEN    "2"
                   MOVE    "◯"        TO  HCM3D-MARUCHO (1)
                   MOVE    "長"        TO  HCM3D-MARUCHO (2)
           END-EVALUATE           
      *
      *    新潟県中越沖地震対応
           EVALUATE    WRK-PARA-GENMENKBN
               WHEN    ZERO
               WHEN    "9"
                   MOVE    SYS-1005-SRYKANAME    TO  HCM3D-SRYKA
               WHEN    "1"
                   STRING  "（未確定分）"      DELIMITED   BY  SIZE
                           SYS-1005-SRYKANAME  DELIMITED   BY  SPACE  
                                               INTO  HCM3D-SRYKA
                   END-STRING
           END-EVALUATE
      *
           .
       310-HEAD-HEN-02-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    医保単独（７０以上９割）
           MOVE    1                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3C-KENSU1 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM3C-NISSU1 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM3C-TENSU1 (IDY)
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN1(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSU1 (11)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM3C-NISSU1 (11) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM3C-TENSU1 (11)
           MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)   TO  WRK-FTNMONEYZ
           MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN1(11)
      *
      *    医保単独（７０以上８割）
           MOVE    2                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3C-KENSU2 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM3C-NISSU2 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM3C-TENSU2 (IDY)
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN2(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSU2 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM3C-NISSU2 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM3C-TENSU2 (9)
           MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)   TO  WRK-FTNMONEYZ
           MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN2(9)
      *
      *    医保単独（本人）
           MOVE    3                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       11
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3C-KENSU3 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM3C-NISSU3 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM3C-TENSU3 (IDY)
               IF      WRK-PRTFLG      =   5
                   MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ       TO  HCM3C-ITBFTN3 (IDY)
               ELSE
                   IF      IDY             =   3
                       MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ   TO  HCM3C-ITBFTN3 (IDY)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSU3 (12)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM3C-NISSU3 (12) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM3C-TENSU3 (12)
           IF      WRK-PRTFLG      =   5
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN3(12)
           END-IF
      *
      *    医保単独（家族）
           MOVE    4                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3C-KENSU4 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM3C-NISSU4 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM3C-TENSU4 (IDY)
               IF      WRK-PRTFLG      =   5
                   MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ       TO  HCM3C-ITBFTN4 (IDY)
               END-IF
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSU4 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM3C-NISSU4 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM3C-TENSU4 (9)
           IF      WRK-PRTFLG      =   5
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN4 (9)
           END-IF
      *
      *    医保単独（三歳）
           MOVE    5                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM3C-KENSU5 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM3C-NISSU5 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM3C-TENSU5 (IDY)
               IF      WRK-PRTFLG      =   5
                   MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ       TO  HCM3C-ITBFTN5 (IDY)
               END-IF
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSU5 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM3C-NISSU5 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM3C-TENSU5 (9)
           IF      WRK-PRTFLG      =   5
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM3C-ITBFTN5 (9)
           END-IF
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 1)  TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3C-KENSUG   
      *     
      *    帳票印刷処理（１頁目）
           MOVE    ZERO                TO  FLG-PRTKBN
           PERFORM 390-PRINT-OUT-SEC
           MOVE    1                   TO  FLG-PRTKBN
      *    公費負担分編集処理
           MOVE    ZERO                TO  FLG-MF01-END
           MOVE    1                   TO  IDX2
      * 
           PERFORM 3201-KOH-HENSYU-SEC
                                   UNTIL   FLG-MF01-END   =   1
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<公費負担分>処理
      *****************************************************************
       3201-KOH-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       6         
                   OR      FLG-MF01-END    =   1  
               IF      SUM-KOH-KOHNUM (IDX3 IDX1 1 IDX2)    =   SPACE
               AND     SUM-KOH-KOHNUM (IDX3 IDX1 2 IDX2)    =   SPACE
               AND     SUM-KOH-KOHNUM (IDX3 IDX1 3 IDX2)    =   SPACE
                   MOVE    1                   TO  FLG-MF01-END
               ELSE    
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       3
                       IF      SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
                                               =   SPACE
                           CONTINUE
                       ELSE    
                           PERFORM 3202-HKNNUM-HENSYU-SEC     
                           MOVE    SUM-KOH-KENSU (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-KENSUZ        
                           MOVE    WRK-KENSUZ
                                           TO  HCM3D-KOH-KENSU (IDX IDY)
                           MOVE    SUM-KOH-TOTALTEN (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-TENSUZ         
                           MOVE    WRK-TENSUZ
                                           TO  HCM3D-KOH-TENSU (IDX IDY)
                           MOVE    SUM-KOH-FTNMONEY (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-FTNMONEYZ
                           MOVE    WRK-FTNMONEYZ
                                           TO  HCM3D-KOH-ITBFTN(IDX IDY)
                       END-IF
                   END-PERFORM
                   ADD     1                   TO  IDX2   
               END-IF        
           END-PERFORM
      *
           IF      SUM-KOH-KOHNUM (IDX3 IDX1 1 IDX2)    =   SPACE
           AND     SUM-KOH-KOHNUM (IDX3 IDX1 2 IDX2)    =   SPACE
           AND     SUM-KOH-KOHNUM (IDX3 IDX1 3 IDX2)    =   SPACE
               MOVE    1                      TO  FLG-MF01-END
           ELSE
      *        帳票印刷処理（２頁以降）
               PERFORM 390-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-02-SEC
           END-IF     
      *
           .
       3201-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       3202-HKNNUM-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-KOHKBN
      *
      *    公費番号まとめ情報からの集計か
           IF      SUM-KOH-MEISYO  (IDX3 IDX1 IDX IDX2)
                                       =   SPACE
      *        保険番号読込み  
               INITIALIZE                      HKNNUM-REC
               MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE   SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
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
                   IF    ( HKN-TEKSTYMD            <=  WRK-SRYYMD )
                   AND   ( HKN-TEKEDYMD            >=  WRK-SRYYMD )
      *                特定疾患のとき 
                       IF      SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
                                               =   "051"
                           MOVE    "（特定疾患）"  TO   WRK-HKNNAME
                       ELSE
      *                その他の保険のとき 
                           STRING  "（"        DELIMITED  BY  SIZE
                                   HKN-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                                   "）"        DELIMITED  BY  SIZE
                                               INTO   WRK-HKNNAME
                           END-STRING
                       END-IF
                       MOVE    1               TO  FLG-HKNNUM 
                   END-IF     
      *
                   IF      FLG-HKNNUM           =   ZERO
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-HKNNUM-READ-SEC
                   ELSE
                       MOVE    HKN-HBTNUM      TO  WRK-Z9
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"                 TO  KANACHK-SYORI 
                       MOVE    WRK-Z9 (4:2)    TO  KANACHK-MAE-INPUT 
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF    ( KANACHK-RC      =   ZERO )
                       AND   ( KANACHK-MAX     >   ZERO )
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                       TO  WRK-KOHNUM
                       END-IF    
                   END-IF 
               END-PERFORM
      *    
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           ELSE
               MOVE    SUM-KOH-MEISYO (IDX3 IDX1 IDX IDX2)
                                           TO   WRK-KOHKBN
           END-IF
      *
           MOVE    WRK-KOHKBN          TO  HCM3D-KOHKBN (IDX IDY)
           .
       3202-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計>処理
      *****************************************************************
       330-TERM-HEN-SEC         SECTION.
      *    合計・総合計
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 3)  TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM3D-KENSUG
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 4)  TO  WRK-GKENSUZ
           MOVE    WRK-GKENSUZ             TO  HCM3D-GKENSU
      *
      *    備考欄
           IF    ( WRK-PARA-GENMENKBN  =   "1"     )
           OR    ( WRK-PARA-GENMENKBN  =   "9" AND
                   SUM-BIKOU-KENSU (IDX3 IDX1)
                                       >   ZERO    )
               MOVE    "未確定分："    TO  HCM3D-BIKOH-TITLE
               MOVE    "件数"          TO  HCM3D-BIKOH-TITLE-KENSU
               MOVE    "日数"          TO  HCM3D-BIKOH-TITLE-NISSU
               MOVE    "点数"          TO  HCM3D-BIKOH-TITLE-TENSU
               MOVE    "一部負担金"    TO  HCM3D-BIKOH-TITLE-ITBFTN
               MOVE    SUM-BIKOU-KENSU     (IDX3 IDX1)
                                       TO  WRK-KENSU9        
               MOVE    WRK-KENSU9      TO  HCM3D-BIKOH-KENSU
               MOVE    SUM-BIKOU-NISSU     (IDX3 IDX1)
                                       TO  WRK-NISSU9 
               MOVE    WRK-NISSU9      TO  HCM3D-BIKOH-NISSU
               MOVE    SUM-BIKOU-TOTALTEN  (IDX3 IDX1)
                                       TO  WRK-TENSU9         
               MOVE    WRK-TENSU9      TO  HCM3D-BIKOH-TENSU
               MOVE    SUM-BIKOU-FTNMONEY  (IDX3 IDX1)
                                       TO  WRK-FTNMONEY9
               MOVE    WRK-FTNMONEY9   TO  HCM3D-BIKOH-ITBFTN
           END-IF
           .
       330-TERM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           ADD     1                   TO  CNT-PAGE
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
           IF      FLG-PRTKBN          =   ZERO                            
               MOVE    "HCM3CV02.red"      TO  SPRT-PRTID
               MOVE    HCM3C               TO  SPRT-PRTDATA
           ELSE                          
      *********MOVE    "HCM3D.red"         TO  SPRT-PRTID
               MOVE    "HCM3DV01.red"       TO  SPRT-PRTID
               MOVE    HCM3D               TO  SPRT-PRTDATA
           END-IF    
           IF      WRK-PARA-MARUCHO    =   "2"
               EVALUATE    IDX1    ALSO    WRK-SYOKBN
                   WHEN    1       ALSO    "1"                      
                       MOVE    "マル長分　社保総括表"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "1"                        
                       MOVE    "マル長分　社保総括表（在医総等）"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "2"                        
                       MOVE    "マル長分　社保総括表［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "2"                        
                       MOVE
                           "マル長分　社保総括表（在医総等）［返戻分］"
                                                   TO  SPRT-TITLE
               END-EVALUATE
           ELSE
               EVALUATE    IDX1    ALSO    WRK-SYOKBN
                   WHEN    1       ALSO    "1"                      
                       MOVE    "社保総括表"        TO  SPRT-TITLE
                   WHEN    2       ALSO    "1"                        
                       MOVE    "社保総括表（在医総等）"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "2"                        
                       MOVE    "社保総括表［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "2"                        
                       MOVE    "社保総括表（在医総等）［返戻分］"
                                                   TO  SPRT-TITLE
               END-EVALUATE
           END-IF
           IF      WRK-PARA-GENMENKBN  =   "1" 
               MOVE    SPRT-TITLE          TO  WRK-SPRT-TITLE
               MOVE    SPACE               TO  SPRT-TITLE
               STRING  "未確定分・"        DELIMITED  BY  SIZE
                       WRK-SPRT-TITLE      DELIMITED  BY  SPACE
                                           INTO    SPRT-TITLE
               END-STRING
           END-IF
      *
           IF      WRK-PARA-HKNJANAME  NOT =   SPACE    
               MOVE    SPRT-TITLE          TO  WRK-SPRT-TITLE
               MOVE    SPACE               TO  SPRT-TITLE
               STRING  "【"                DELIMITED  BY  SIZE
                       WRK-PARA-HKNJANAME  DELIMITED  BY  SPACE
                       "】"                DELIMITED  BY  SIZE
                       WRK-SPRT-TITLE      DELIMITED  BY  SPACE
                                           INTO    SPRT-TITLE
               END-STRING
           END-IF
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
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
       390-PRINT-OUT-EXT.
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
           STRING  "ORCBM037 "         DELIMITED  BY  SIZE
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
           DISPLAY "*** ORCBM037 PAGE  "  CNT-PAGE 
           DISPLAY "*** ORCBM037 END ***"
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
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(7)  NOT =   SPACE
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
           MOVE    SYS-1900-PRTKBN(7)
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           MOVE    SYS-1900-PRTKBN(7)
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           MOVE    SYS-1900-PRTKBN(7)
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
           PERFORM 800-SYSKANRI-READ-SEC
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
               PERFORM 900-DBFETCH-SEC
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
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
