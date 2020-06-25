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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM005.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求帳票
      *  コンポーネント名  : チェックリスト（ＨＣ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/05    MCC-森脇      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/05/22  テスト患者は処理しない     
      *                                     診療年月が違って請求年月が同じ時
      *                                     出力の対象とする
      *
      *  01.01.01    NACL-藤原    02.09.17  印刷サブの追加
      *  01.01.02    NACL-藤原    03.05.09  入院対応
      *                                     dia→monpeに変更
      *  01.01.03    NACL-藤原    04.02.27  実日数の追加
      *                                     プレビュー・再印刷処理に対応
      *  ver.2.5.0
      *  01.02.01    NACL-藤原    05.03.08  実日数１、実日数２の追加
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *                                     HCM05V02.red に変更
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      * 
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
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
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *****COPY    "HCM05.INC"     REPLACING  //HCM05//
      *****COPY    "HCM05V01.INC"  REPLACING  //HCM05V01//
           COPY    "HCM05V02.INC"  REPLACING  //HCM05V02//
                                   BY         //HC05//.
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-IN              PIC 9(05).
           03  CNT-OUT             PIC 9(05).
           03  CNT-RENNUM          PIC 9(05). 
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC X(200).        
      *
           03  WRK-SYSYMDWH          PIC X(22).
      *
           03  WRK-MSG               PIC X(10). 
      *
           03  WRK-SRYYM             PIC X(16).     
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-PAGE              PIC ZZ9.
           03  WRK-RENNUM            PIC ZZZZ9.
           03  WRK-TEN               PIC ZZZ,ZZZ,ZZ9.
           03  WRK-JNISSU            PIC ZZZZZZZ9.
      *
      *    集計領域
       01  SUM-AREA                               VALUE   ZERO.
           03  SUM-GHKNTEN           PIC 9(08).
           03  SUM-GMONEY1           PIC 9(08).
           03  SUM-GMONEY2           PIC 9(08).
           03  SUM-GJNISSU           PIC 9(08).
           03  SUM-GJNISSU1          PIC 9(08).
           03  SUM-GJNISSU2          PIC 9(08).
           03  SUM-RENNUM            PIC 9(05).
      *
      *    キー領域
       01  KEY-AREA                               VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-NYUGAIKBN       PIC 9(01).
               05  KEY-N-TEISYUTUSAKI    PIC 9(01).
           03  KEY-OLD.
               05  KEY-O-NYUGAIKBN       PIC 9(01).
               05  KEY-O-TEISYUTUSAKI    PIC 9(01).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      * 
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".          
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
                           USING
                           COMMAND-PARAM.
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
                                       SUM-AREA
                                       CNT-AREA
                                       SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM    
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM005"      TO  JOB-PGID
           MOVE    "総括チェックリスト"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    システム日付の編集
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDWH   
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    医保分
           MOVE    ZERO            TO  FLG-END
           MOVE    1               TO  FLG-SYORI
      *
           PERFORM 900-SEIKYU-SELECT-SEC
           PERFORM 2001-HENSYU-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 900-SEIKYU-CLOSE-SEC
      *
      *    特別療養費分
           MOVE    ZERO            TO  FLG-END
           MOVE    2               TO  FLG-SYORI
      *
           PERFORM 900-SEIKYU-SELECT-SEC
           PERFORM 2001-HENSYU-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 900-SEIKYU-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票作成処理
      *****************************************************************
       2001-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO            TO  SUM-AREA
                                       CNT-RENNUM
           MOVE    KEY-NEW         TO  KEY-OLD 
      *     
      *    帳票編集<見出し>処理
           PERFORM 310-HEAD-HEN-SEC
      *     
      *    帳票編集<明細>処理
           PERFORM             UNTIL   FLG-END     =   1
                               OR      KEY-NEW NOT =   KEY-OLD
               PERFORM 320-BODY-HEN-SEC
           END-PERFORM    
      *
      *    帳票編集<合計>処理
              PERFORM 330-SUM-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
           .
       2001-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           ADD     1                   TO  CNT-PAGE
      *
           MOVE    SPACE               TO  HC05
      *
           MOVE    WRK-SRYYM           TO  HC05-HSRYYM
           MOVE    WRK-SYSYMDWH        TO  HC05-SYSYMD
           MOVE    CNT-PAGE            TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC05-PAGE
      * 
           MOVE    "＊＊＊　総括チェックリスト　＊＊＊"
                                       TO  HC05-TITLE
      *
           EVALUATE    FLG-SYORI   ALSO    KEY-N-TEISYUTUSAKI  ALSO
                                       KEY-N-NYUGAIKBN
               WHEN    1   ALSO    1   ALSO    1 
                   MOVE    "社保・入院"    TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    2   ALSO    1    
                   MOVE    "国保・入院"    TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    5   ALSO    1    
                   MOVE    "公害・入院"    TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    6   ALSO    1    
                   MOVE    "広域・入院"    TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    1   ALSO    2 
                   MOVE    "社保・入院外"  TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    2   ALSO    2    
                   MOVE    "国保・入院外"  TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    5   ALSO    2    
                   MOVE    "公害・入院外"  TO  HC05-TEISYUTUNAME
               WHEN    1   ALSO    6   ALSO    2    
                   MOVE    "広域・入院外"  TO  HC05-TEISYUTUNAME
               WHEN    2   ALSO    2   ALSO    1    
               WHEN    2   ALSO    6   ALSO    1    
                   MOVE    "特療・入院"    TO  HC05-TEISYUTUNAME
               WHEN    2   ALSO    2   ALSO    2    
               WHEN    2   ALSO    6   ALSO    2    
                   MOVE    "特療・入院外"  TO  HC05-TEISYUTUNAME
           END-EVALUATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           IF      CNT-LINE        >=  22
      *        帳票印刷処理
               PERFORM 390-PRINT-OUT-SEC
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
           END-IF                    
      *
           ADD     1                 TO  CNT-LINE
           ADD     1                 TO  CNT-RENNUM
      *
           MOVE    CNT-RENNUM        TO  WRK-RENNUM
           MOVE    WRK-RENNUM        TO  HC05-RENNUM(CNT-LINE)
      *
           MOVE    RECE10-PTNUM      TO  HC05-PTNUM(CNT-LINE)
      *
           MOVE    PTINF-NAME          TO  HC05-NAME(CNT-LINE)
      *
           IF      RECE10-HKNJANUM     =    "00000000"
               CONTINUE
           ELSE
               MOVE    RECE10-HKNJANUM     TO  HC05-HKNJANUM(CNT-LINE)
           END-IF
      *
           MOVE    RECE10-SRYYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  HC05-SRYYM(CNT-LINE)
      *
           MOVE    RECE10-HENREIYM     TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  HC05-HENYM(CNT-LINE)
      *
           MOVE    RECE10-SKYYM        TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  HC05-SEIYM(CNT-LINE)
      *
           IF      KEY-N-TEISYUTUSAKI  =   5 
               MOVE    RECE10-FTNMONEY (1) TO  WRK-TEN
               MOVE    WRK-TEN             TO  HC05-HKNTEN(CNT-LINE)
           ELSE   
               MOVE    RECE10-TOTALTEN(1)  TO  WRK-TEN
               MOVE    WRK-TEN             TO  HC05-HKNTEN(CNT-LINE)   
               MOVE    RECE10-TOTALTEN(2)  TO  WRK-TEN
               MOVE    WRK-TEN             TO  HC05-MONEY1(CNT-LINE)
               MOVE    RECE10-TOTALTEN(3)  TO  WRK-TEN
               MOVE    WRK-TEN             TO  HC05-MONEY2(CNT-LINE)
               MOVE    RECE10-JNISSU  (2)  TO  WRK-JNISSU
               MOVE    WRK-JNISSU          TO  HC05-JNISSU1(CNT-LINE)
               MOVE    RECE10-JNISSU  (3)  TO  WRK-JNISSU
               MOVE    WRK-JNISSU          TO  HC05-JNISSU2(CNT-LINE)
           END-IF
           MOVE    RECE10-JNISSU  (1)  TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC05-JNISSU(CNT-LINE)
      *
           EVALUATE    RECE10-SKYKBN
               WHEN    "0"
                   MOVE    "請求しない"    TO  HC05-SKYKBN (CNT-LINE) 
               WHEN    "1"
                   MOVE    "未請求"        TO  HC05-SKYKBN (CNT-LINE) 
               WHEN    "2"
                   MOVE    "請求済み"      TO  HC05-SKYKBN (CNT-LINE) 
           END-EVALUATE            
      *
           IF      RECE10-SKYKBN       =   "1"
               MOVE    "未"                TO  HC05-MSG (CNT-LINE)
           END-IF
           IF      RECE10-SRYYM    NOT =   LNK-PRTKANRI-SRYYM
               MOVE    "他"                TO  HC05-MSG (CNT-LINE) (3:)
           END-IF
      *
           IF    ( RECE10-SKYKBN   NOT =   ZERO                        )
               IF    ( RECE10-SRYYM    =   LNK-PRTKANRI-SRYYM          )
               AND   ( RECE10-SKYYM    =   LNK-PRTKANRI-SRYYM  OR  ZERO)
                   MOVE    "※"                TO  HC05-TAISYO(CNT-LINE)
                   ADD     1                   TO  SUM-RENNUM
                   ADD     RECE10-TOTALTEN(1)  TO  SUM-GHKNTEN
                   ADD     RECE10-TOTALTEN(2)  TO  SUM-GMONEY1
                   ADD     RECE10-TOTALTEN(3)  TO  SUM-GMONEY2
                   ADD     RECE10-JNISSU  (1)  TO  SUM-GJNISSU
                   ADD     RECE10-JNISSU  (2)  TO  SUM-GJNISSU1
                   ADD     RECE10-JNISSU  (3)  TO  SUM-GJNISSU2
               ELSE
                   IF    ( RECE10-SKYKBN   NOT =   ZERO               )
                   AND   ( RECE10-SKYYM        =   LNK-PRTKANRI-SRYYM )
                       MOVE    "※"                TO 
                                                   HC05-TAISYO(CNT-LINE)
                       ADD     1                   TO  SUM-RENNUM
                       ADD     RECE10-TOTALTEN(1)  TO  SUM-GHKNTEN
                       ADD     RECE10-TOTALTEN(2)  TO  SUM-GMONEY1
                       ADD     RECE10-TOTALTEN(3)  TO  SUM-GMONEY2
                       ADD     RECE10-JNISSU  (1)  TO  SUM-GJNISSU
                       ADD     RECE10-JNISSU  (2)  TO  SUM-GJNISSU1
                       ADD     RECE10-JNISSU  (3)  TO  SUM-GJNISSU2
                   END-IF
               END-IF
           END-IF        
      *
           ADD     1                   TO  CNT-OUT
      *
           PERFORM 900-SEIKYU-FETCH-SEC         
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計部>処理
      *****************************************************************
       330-SUM-HEN-SEC          SECTION.
      *
           MOVE    SUM-RENNUM        TO  WRK-RENNUM
           MOVE    WRK-RENNUM        TO  HC05-GKENSU
           MOVE    SUM-GHKNTEN       TO  WRK-TEN
           MOVE    WRK-TEN           TO  HC05-GHKNTEN
           MOVE    SUM-GMONEY1       TO  WRK-TEN
           MOVE    WRK-TEN           TO  HC05-GMONEY1
           MOVE    SUM-GMONEY2       TO  WRK-TEN
           MOVE    WRK-TEN           TO  HC05-GMONEY2
           MOVE    SUM-GJNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU        TO  HC05-GJNISSU
           MOVE    SUM-GJNISSU1      TO  WRK-JNISSU
           MOVE    WRK-JNISSU        TO  HC05-GJNISSU1
           MOVE    SUM-GJNISSU2      TO  WRK-JNISSU
           MOVE    WRK-JNISSU        TO  HC05-GJNISSU2
      *
           .
       330-SUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
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
      *****MOVE    "HCM05.red"         TO  SPRT-PRTID
      *****MOVE    "HCM05V01.red"      TO  SPRT-PRTID
           MOVE    "HCM05V02.red"      TO  SPRT-PRTID
           MOVE    "総括チェックリスト"TO  SPRT-TITLE
           MOVE    HC05                TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC                           
           END-IF                                                              
           .
       390-PRINT-OUT-EXT.
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
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT                        
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM005 IN  "  CNT-IN
           DISPLAY "***          OUT "  CNT-OUT 
           DISPLAY "*** ORCBM005 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           IF      WRK-SYMD            =   ZERO   OR  SPACE
                MOVE    SPACE              TO  WRK-HENYMDG
           ELSE    
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
               INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           END-IF    
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC                 SECTION.
      *
           MOVE    SPACE                TO  RECE10-REC  
grpsys     MOVE    WRK-PARA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM   TO  RECE10-SKYYM
           MOVE    RECE10-REC           TO  MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               MOVE    HIGH-VALUE          TO  KEY-NEW 
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ 検索処理
      *****************************************************************
       900-SEIKYU-FETCH-SEC                SECTION.
      *           
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      RECE10-TEISYUTUSAKI =   "3" OR  "4"
                   GO  TO  900-SEIKYU-FETCH-SEC
               END-IF
      *
               EVALUATE    FLG-SYORI
                   WHEN    1
                       IF      RECE10-RECESYUBETU  =    "6666"
                           GO  TO  900-SEIKYU-FETCH-SEC
                       END-IF
                   WHEN    2
                       IF      RECE10-RECESYUBETU  =    "6666"
                           CONTINUE
                       ELSE
                           GO  TO  900-SEIKYU-FETCH-SEC
                       END-IF
               END-EVALUATE
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
grpsys         MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-FETCH-SEC
               END-IF
      *
               MOVE    RECE10-TEISYUTUSAKI TO  KEY-N-TEISYUTUSAKI
               IF      FLG-SYORI           =   2
                   MOVE    2                   TO  KEY-N-TEISYUTUSAKI
               END-IF 
               MOVE    RECE10-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
               ADD     1                   TO  CNT-IN
      *
               DISPLAY "PTNUM=" RECE10-PTNUM 
           ELSE
               MOVE    1                   TO  FLG-END 
               MOVE    HIGH-VALUE          TO  KEY-NEW
           END-IF  
           .
       900-SEIKYU-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ クローズ処理
      *****************************************************************
       900-SEIKYU-CLOSE-SEC                 SECTION.
      *
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SEIKYU-CLOSE-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
