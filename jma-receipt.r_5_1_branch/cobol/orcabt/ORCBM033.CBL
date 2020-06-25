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
       PROGRAM-ID.             ORCBM033.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 社保・総括表（医科・入院）
      *                      ２００６．１０改正対応
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/10/01    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.03.01    NACL-藤原    07/02/10  同一地方公費のまとめ集計
      *                                     岡山対応
      *
      *  03.04.01    NACL-森脇    07/01/16  医療機名称、住所編集
      *  03.04.02    NACL-藤原    07/03/13  備考欄の公費名称を差し込み項
      *                                     目とする
      *                                     HCM2TV02.red →  HCM2TV03.red
      *  03.04.03    NACL-藤原    07/04/13  ２００７．４改正対応
      *                                     １０（結３４）→１０（感３７の２）
      *                                     １１（結３５）→１１（結核入院）
      *  03.04.04    NACL-藤原    07/08/20  同一地方公費のまとめ集計
      *                                     北海道対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/09/11  新潟県中越沖地震対応
      *                                     ２００７年９月〜１２月対象 
      *
      *  04.01.01    NACL-藤原    07/10/24  総括表の地方公費記載対応
      *
      *  04.05.01    NACL-藤原    11/04/04  災害対応
      *                                     備考欄の記載等追加
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-藤原    15/08/18  医療機関編集情報への開設者名
      *                                     追加対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
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
           COPY    "HCM2SV02.INC"  REPLACING  //HCM2SV02//
                                   BY         //HCM2S//.
           COPY    "HCM2TV03.INC"  REPLACING  //HCM2TV03//
                                   BY         //HCM2T//.
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
           03  WRK-SEIYMD        PIC X(08).
           03  WRK-SEIYMDWH      PIC X(22).
           03  WRK-SRYYMWH       PIC X(16).
           03  WRK-SRYYMD        PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-HENYMDG           PIC X(22).
      *
           03  WRK-RECEERR             PIC X(200). 
      *
           03  WRK-Z9                PIC ZZZZZ.
           03  WRK-ZZ9               PIC ZZ,ZZZ,ZZZ.
           03  WRK-NISSUZ            PIC ZZZZZZ.
           03  WRK-KENSUZ            PIC ZZZZZZ.
           03  WRK-TENSUZ            PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-FTNMONEYZ         PIC Z,ZZZ,ZZZ,ZZZ. 
           03  WRK-SHOKENSUZ         PIC ZZ,ZZZ.
           03  WRK-SHONISSUZ         PIC ZZ,ZZZ.
           03  WRK-SHORYOYOHIZ       PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-SHOFTNZ           PIC Z,ZZZ,ZZZ,ZZZ.
           03  WRK-GKENSUZ           PIC ZZZZZZZ. 
      *             
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(04).
               05  WRK-HKNNAME       PIC X(40). 
      *----(03.04.01)--ADD-START---
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-KAISETUNAME         PIC X(80).
      *----(03.04.01)--ADD-END-----
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    固定項目
       01  WRK-CONS-AREA.
      *    ２００７年４月改正
           03  WRK-CONS-SRYYM-200704   PIC X(06)   VALUE   "200704".
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
      *----(03.04.01)--ADD-START---
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *----(03.04.01)--ADD-END-----
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    サブプロ用　領域
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
      *
       01  WRK-NYUGAIKBN           PIC X(01).
      *
       01  WRK-SYOKBN              PIC X(01).
      *
       01  WRK-PRTFLG              PIC 9(01).
grpsys*
grpsys     COPY    "COMMON-SPA".
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
                                               WRK-NYUGAIKBN
                                               WRK-SYOKBN
                                               WRK-PRTFLG
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
           DISPLAY "*** ORCBM033 START ***"
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
      *    医療機関名編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD 
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
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
      *----(03.04.01)--ADD-START---
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *----(03.04.01)--ADD-END-----
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
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
grpsys                 MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
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
                       IF      SUM-GOK-KENSU (IDX3 IDX1 4)     >   ZERO 
                           MOVE    ZERO            TO  FLG-PRTKBN
      *           
      *                    帳票編集<見出し・ＨＣＭ２Ｉ>処理
                           PERFORM 310-HEAD-HEN-01-SEC
      *                    帳票編集<見出し・ＨＣＭ２Ｊ>処理
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
      *    帳票編集<見出し・ＨＣＭ２Ａ>処理
      *****************************************************************
       310-HEAD-HEN-01-SEC          SECTION.
      *
           INITIALIZE                      HCM2S
      *
           MOVE    WRK-SEIYMDWH        TO  HCM2S-SEIYMD
      *
           MOVE    WRK-SRYYMWH         TO  HCM2S-PRTYM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  HCM2S-ADRS
           INSPECT HCM2S-ADRS          REPLACING   ALL "  "    BY  "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  HCM2S-HOSPNAME
           INSPECT HCM2S-HOSPNAME      REPLACING   ALL "  "    BY  "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME     TO  HCM2S-KAISETUNAME
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM2S-HOSPCD
      *
           IF      WRK-NYUGAIKBN       =   "2"
               EVALUATE    IDX1
                   WHEN    1
                       MOVE    "その他のレセ"      TO  HCM2S-TITLE  
                   WHEN    2
                       IF      WRK-PRTFLG          =   1
                           MOVE    "在総診または在医総を算定したレセ"
                                                   TO  HCM2S-TITLE
                       ELSE   
                           MOVE    "在医総管または在医総を算定したレセ"
                                                   TO  HCM2S-TITLE
                       END-IF
               END-EVALUATE
           END-IF           
      *
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "○"      TO  HCM2S-NYUGAIKBN (1)
               WHEN    "2"
                   MOVE    "○"      TO  HCM2S-NYUGAIKBN (2)
           END-EVALUATE           
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"  TO  HCM2S-TITLE1
           END-EVALUATE           
      *
           IF      WRK-PRTFLG          =   1
               MOVE    "日数"              TO  HCM2S-TITLE2
           ELSE   
               MOVE    "回数"              TO  HCM2S-TITLE2
           END-IF
      *
           MOVE    "食事療養・生活療養"
                                 TO  HCM2S-TITLE3
           MOVE    "７"          TO  HCM2S-TITLE4
           MOVE    "七"          TO  HCM2S-TITLE5
      *
           EVALUATE    WRK-PARA-MARUCHO
               WHEN    "2"
                   MOVE    "◯"          TO  HCM2S-MARUCHO (1)
                   MOVE    "長"          TO  HCM2S-MARUCHO (2)
           END-EVALUATE           
      *
      *    新潟県中越沖地震対応
           EVALUATE    WRK-PARA-GENMENKBN
               WHEN    ZERO
                   MOVE    SYS-1005-SRYKANAME    TO  HCM2S-SRYKA
               WHEN    "1"
                   STRING  "（未確定分）"      DELIMITED   BY  SIZE
                           SYS-1005-SRYKANAME  DELIMITED   BY  SPACE  
                                               INTO  HCM2S-SRYKA
                   END-STRING
           END-EVALUATE
      *
           .
       310-HEAD-HEN-01-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し・ＨＣＭ２Ｂ>処理
      *****************************************************************
       310-HEAD-HEN-02-SEC          SECTION.
      *
           INITIALIZE                      HCM2T
      *    
           MOVE    SYS-1001-HOSPCDN    TO  HCM2T-HOSPCD 
      *
           IF      WRK-NYUGAIKBN       =   "2"
               EVALUATE    IDX1
                   WHEN    1
                       MOVE    "その他のレセ"      TO  HCM2T-TITLE  
                   WHEN    2
                       IF      WRK-PRTFLG          =   1
                           MOVE    "在総診または在医総を算定したレセ"
                                                       TO  HCM2T-TITLE
                       ELSE   
                           MOVE    "在医総管または在医総を算定したレセ"
                                                       TO  HCM2T-TITLE
                       END-IF
               END-EVALUATE
           END-IF           
      *
           EVALUATE    WRK-NYUGAIKBN
              WHEN    "1"
                   MOVE    "○"      TO  HCM2T-NYUGAIKBN (1)
               WHEN    "2"
                   MOVE    "○"      TO  HCM2T-NYUGAIKBN (2)
           END-EVALUATE           
      *
           EVALUATE    WRK-SYOKBN
               WHEN    "2"
                   MOVE    "（返戻分）"  TO  HCM2T-TITLE1
           END-EVALUATE           
      *
           EVALUATE    WRK-PARA-MARUCHO
               WHEN    "2"
                   MOVE    "◯"          TO  HCM2T-MARUCHO (1)
                   MOVE    "長"          TO  HCM2T-MARUCHO (2)
           END-EVALUATE           
      *
      *    新潟県中越沖地震対応
           EVALUATE    WRK-PARA-GENMENKBN
               WHEN    ZERO
                   MOVE    SYS-1005-SRYKANAME    TO  HCM2T-SRYKA
               WHEN    "1"
                   STRING  "（未確定分）"      DELIMITED   BY  SIZE
                           SYS-1005-SRYKANAME  DELIMITED   BY  SPACE  
                                               INTO  HCM2T-SRYKA
                   END-STRING
           END-EVALUATE
      *
           MOVE    "４３（老人被爆）"    TO  HCM2T-BKO-KOHKBN (1)
           IF      LNK-PRTKANRI-SRYYM    <   WRK-CONS-SRYYM-200704
               MOVE    "１０（結３４）"      TO  HCM2T-BKO-KOHKBN (2)
               MOVE    "１１（結３５）"      TO  HCM2T-BKO-KOHKBN (3)
           ELSE
               MOVE    "１０（感37の2） "    TO  HCM2T-BKO-KOHKBN (2)
               MOVE    "１１（結核入院）"    TO  HCM2T-BKO-KOHKBN (3)
           END-IF
           MOVE    "１２（生保）"        TO  HCM2T-BKO-KOHKBN (4)
           MOVE    "１５（更生）"        TO  HCM2T-BKO-KOHKBN (5)
           MOVE    "２０（精２９）"      TO  HCM2T-BKO-KOHKBN (6)
           MOVE    "２１（精神通院）"    TO  HCM2T-BKO-KOHKBN (7)
           MOVE    "２２（麻薬）"        TO  HCM2T-BKO-KOHKBN (8)
           MOVE    "２８（感染症）"      TO  HCM2T-BKO-KOHKBN (9)
           MOVE    "５１（特疾）"        TO  HCM2T-BKO-KOHKBN (10)
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
               MOVE    WRK-KENSUZ              TO  HCM2S-KENSU1 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM2S-NISSU1 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM2S-TENSU1 (IDY)
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM2S-ITBFTN1(IDY)
               MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-SHOKENSUZ
               MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU1 (IDY)
               MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHONISSUZ 
               MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU1 (IDY)
               MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHORYOYOHIZ
               MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK1(IDY)
               MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHOFTNZ
               MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK1(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSU1 (11)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM2S-NISSU1 (11) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM2S-TENSU1 (11)
           MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)   TO  WRK-FTNMONEYZ
           MOVE    WRK-FTNMONEYZ           TO  HCM2S-ITBFTN1(11)
           MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX 12) 
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU1 (11)
           MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHONISSUZ 
           MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU1 (11)
           MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHORYOYOHIZ
           MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK1(11)
           MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX 12)  TO  WRK-SHOFTNZ
           MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK1(11)
      *
      *    医保単独（７０以上８割）
           MOVE    2                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM2S-KENSU2 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM2S-NISSU2 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM2S-TENSU2 (IDY)
               MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
               MOVE    WRK-FTNMONEYZ           TO  HCM2S-ITBFTN2(IDY)
               MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-SHOKENSUZ
               MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU2 (IDY)
               MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHONISSUZ 
               MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU2 (IDY)
               MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHORYOYOHIZ
               MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK2(IDY)
               MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHOFTNZ
               MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK2(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSU2 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM2S-NISSU2 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM2S-TENSU2 (9)
           MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX 12)   TO  WRK-FTNMONEYZ
           MOVE    WRK-FTNMONEYZ           TO  HCM2S-ITBFTN2(9)
           MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX 12) 
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU2 (9)
           MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHONISSUZ 
           MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU2 (9)
           MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHORYOYOHIZ
           MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK2(9)
           MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX 12)  TO  WRK-SHOFTNZ
           MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK2(9)
      *
      *    医保単独（本人）
           MOVE    3                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       11
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM2S-KENSU3 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM2S-NISSU3 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM2S-TENSU3 (IDY)
               IF      IDY             =   3
                   MOVE    SUM-FTNMONEY (IDX3 IDX1 IDX IDY)
                                               TO  WRK-FTNMONEYZ
                   MOVE    WRK-FTNMONEYZ       TO  HCM2S-ITBFTN3
               END-IF
               MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-SHOKENSUZ
               MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU3 (IDY)
               MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHONISSUZ 
               MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU3 (IDY)
               MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHORYOYOHIZ
               MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK3(IDY)
               MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHOFTNZ
               MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK3(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSU3 (12)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM2S-NISSU3 (12) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM2S-TENSU3 (12)
           MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX 12) 
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU3 (12)
           MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHONISSUZ 
           MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU3 (12)
           MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHORYOYOHIZ
           MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK3(12)
           MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX 12)  TO  WRK-SHOFTNZ
           MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK3(12)
      *
      *    医保単独（家族）
           MOVE    4                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM2S-KENSU4 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM2S-NISSU4 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM2S-TENSU4 (IDY)
               MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-SHOKENSUZ
               MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU4 (IDY)
               MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHONISSUZ 
               MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU4 (IDY)
               MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHORYOYOHIZ
               MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK4(IDY)
               MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHOFTNZ
               MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK4(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSU4 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM2S-NISSU4 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM2S-TENSU4 (9)
           MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX 12) 
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU4 (9)
           MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHONISSUZ 
           MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU4 (9)
           MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHORYOYOHIZ
           MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK4(9)
           MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX 12)  TO  WRK-SHOFTNZ
           MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK4(9)
      *
      *    医保単独（三歳）
           MOVE    5                   TO  IDX
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       8
               MOVE    SUM-KENSU    (IDX3 IDX1 IDX IDY)  TO  WRK-KENSUZ
               MOVE    WRK-KENSUZ              TO  HCM2S-KENSU5 (IDY)
               MOVE    SUM-NISSU    (IDX3 IDX1 IDX IDY)  TO  WRK-NISSUZ 
               MOVE    WRK-NISSUZ              TO  HCM2S-NISSU5 (IDY) 
               MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX IDY)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM2S-TENSU5 (IDY)
               MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                               TO  WRK-SHOKENSUZ
               MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU5 (IDY)
               MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHONISSUZ 
               MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU5 (IDY)
               MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHORYOYOHIZ
               MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK5(IDY)
               MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                               TO  WRK-SHOFTNZ
               MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK5(IDY)
           END-PERFORM
           MOVE    SUM-KENSU    (IDX3 IDX1 IDX 12)   TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSU5 (9)
           MOVE    SUM-NISSU    (IDX3 IDX1 IDX 12)   TO  WRK-NISSUZ 
           MOVE    WRK-NISSUZ              TO  HCM2S-NISSU5 (9) 
           MOVE    SUM-TOTALTEN (IDX3 IDX1 IDX 12)   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCM2S-TENSU5 (9)
           MOVE    SUM-SHOKUJIKENSU (IDX3 IDX1 IDX 12) 
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSU5 (9)
           MOVE    SUM-SHOKUJINISSU (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHONISSUZ 
           MOVE    WRK-SHONISSUZ           TO  HCM2S-SYONISSU5 (9)
           MOVE    SUM-SHOKUJIRYOYOHI (IDX3 IDX1 IDX 12)
                                           TO  WRK-SHORYOYOHIZ
           MOVE    WRK-SHORYOYOHIZ         TO  HCM2S-SYOKINGAK5(9)
           MOVE    SUM-SHOKUJIFTN (IDX3 IDX1 IDX 12)  TO  WRK-SHOFTNZ
           MOVE    WRK-SHOFTNZ             TO  HCM2S-SYOFTNGAK5(9)
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 1)  TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2S-KENSUG   
           MOVE    SUM-GOK-SHOKENSU (IDX3 IDX1 1)
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2S-SYOKENSUG  
      *
      *    老人保健
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       3
                   MOVE    SUM-ROU-KENSU    (IDX3 IDX1 IDX IDY) 
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2T-ROU-KENSU (IDX IDY)
                   MOVE    SUM-ROU-SHOKUJIKENSU (IDX3 IDX1 IDX IDY) 
                                           TO  WRK-SHOKENSUZ
                   MOVE    WRK-SHOKENSUZ   TO
                                           HCM2T-ROU-SYOKENSU  (IDX IDY)
                   IF      IDY                 <   3
                       MOVE    SUM-ROU-NISSU    (IDX3 IDX1 IDX IDY)
                                           TO  WRK-NISSUZ 
                       MOVE    WRK-NISSUZ  TO  HCM2T-ROU-NISSU (IDX IDY) 
                       MOVE    SUM-ROU-TOTALTEN (IDX3 IDX1 IDX IDY)
                                           TO  WRK-TENSUZ
                       MOVE    WRK-TENSUZ  TO  HCM2T-ROU-TENSU (IDX IDY) 
                       MOVE    SUM-ROU-FTNMONEY (IDX3 IDX1 IDX IDY)
                                           TO  WRK-FTNMONEYZ
                       MOVE    WRK-FTNMONEYZ
                                           TO  HCM2T-ROU-ITBFTN(IDX IDY)
                       MOVE    SUM-ROU-SHOKUJINISSU (IDX3 IDX1 IDX IDY)
                                           TO  WRK-SHONISSUZ 
                       MOVE    WRK-SHONISSUZ
                                           TO
                                           HCM2T-ROU-SYONISSU  (IDX IDY)
                       MOVE    SUM-ROU-SHOKUJIRYOYOHI(IDX3 IDX1 IDX IDY)
                                           TO  WRK-SHORYOYOHIZ
                       MOVE    WRK-SHORYOYOHIZ
                                           TO
                                           HCM2T-ROU-SYOKINGAK (IDX IDY)
                       MOVE    SUM-ROU-SHOKUJIFTN (IDX3 IDX1 IDX IDY)
                                           TO  WRK-SHOFTNZ
                       MOVE    WRK-SHOFTNZ TO
                                           HCM2T-ROU-SYOFTNGAK (IDX IDY)
                   END-IF
               END-PERFORM    
           END-PERFORM
      *     
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 2)  TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2T-KENSUG (1)    
           MOVE    SUM-GOK-SHOKENSU (IDX3 IDX1 2)
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2T-SYOKENSUG (1)
      *
      *    公費再掲編集処理
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               IF      SUM-ROU-KOH-KENSU (IDX3 IDX1 IDX)    >   ZERO
                   MOVE    SUM-ROU-KOH-KENSU    (IDX3 IDX1 IDX) 
                                           TO  WRK-KENSUZ
                   MOVE    WRK-KENSUZ      TO  HCM2T-BKO-KENSU (IDX) 
                   MOVE    SUM-ROU-KOH-NISSU    (IDX3 IDX1 IDX)
                                           TO  WRK-NISSUZ 
                   MOVE    WRK-NISSUZ      TO  HCM2T-BKO-NISSU (IDX) 
                   MOVE    SUM-ROU-KOH-ITBFTN  (IDX3 IDX1 IDX)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9 (2:)    TO  HCM2T-BKO-ITBFTN(IDX)
                   MOVE    SUM-ROU-KOH-FTNMONEY (IDX3 IDX1 IDX)
                                           TO  WRK-ZZ9
                   MOVE    WRK-ZZ9 (2:)    TO  HCM2T-BKO-KOJYO (IDX)
                   IF      IDX         NOT =   2   AND     7
                       MOVE    SUM-ROU-KOH-SHOKUJIKENSU  (IDX3 IDX1 IDX) 
                                           TO  WRK-SHOKENSUZ
                       MOVE    WRK-SHOKENSUZ
                                           TO  HCM2T-BKO-SYOKENSU (IDX) 
                       MOVE    SUM-ROU-KOH-SHOKUJINISSU  (IDX3 IDX1 IDX)
                                           TO  WRK-SHONISSUZ
                       MOVE    WRK-SHONISSUZ
                                           TO  HCM2T-BKO-SYONISSU (IDX)
                       MOVE    SUM-ROU-KOH-SHOKUJIFTN (IDX3 IDX1 IDX)
                                           TO  WRK-SHOFTNZ
                       MOVE    WRK-SHOFTNZ TO  HCM2T-BKO-SYOFTNGAK(IDX)
                   END-IF 
                   IF      IDX         NOT =   7
                       MOVE    SUM-ROU-KOH-SHOKUJIRYOYOHI
                                                       (IDX3 IDX1 IDX)
                                           TO  WRK-SHORYOYOHIZ
                       MOVE    WRK-SHORYOYOHIZ
                                           TO  HCM2T-BKO-SYOKINGAK(IDX)
                   END-IF
               END-IF
           END-PERFORM    
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
      *    
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
                                           TO  HCM2T-KOH-KENSU (IDX IDY)
                           MOVE    SUM-KOH-TOTALTEN (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-TENSUZ         
                           MOVE    WRK-TENSUZ
                                           TO  HCM2T-KOH-TENSU (IDX IDY)
                           IF      IDX         =   1          
                               MOVE    SUM-KOH-SHOKUJIFTN
                                                   (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-SHOFTNZ
                               MOVE    WRK-SHOFTNZ TO
                                           HCM2T-KOH-SYOFTNGAK (IDX IDY)
                           END-IF
                           MOVE    SUM-KOH-FTNMONEY (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-FTNMONEYZ
                           MOVE    WRK-FTNMONEYZ
                                           TO  HCM2T-KOH-ITBFTN(IDX IDY)
                           MOVE    SUM-KOH-SHOKUJIKENSU
                                                    (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-SHOKENSUZ
                           MOVE    WRK-SHOKENSUZ   TO
                                           HCM2T-KOH-SYOKENSU  (IDX IDY)
                           MOVE    SUM-KOH-SHOKUJIRYOYOHI
                                                    (IDX3 IDX1 IDX IDX2)
                                           TO  WRK-SHORYOYOHIZ
                           MOVE    WRK-SHORYOYOHIZ
                                           TO
                                           HCM2T-KOH-SYOKINGAK (IDX IDY)
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
grpsys         MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
               MOVE   SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
                                           TO    HKN-HKNNUM
               MOVE   HKNNUM-REC           TO    MCPDATA-REC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
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
                       IF     SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
                                                       =   "027"
      *                    老人保険のとき 
                           IF    ( SYS-1001-HOSPSBT
                                                   =   HKN-PAYKBN(1:1) )
                           AND   ( SYS-1001-ROUPAYKBN
                                                   =   HKN-PAYKBN(2:1) )
                               STRING  "（"        DELIMITED  BY  SIZE
                                       HKN-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                                       "）"        DELIMITED  BY  SIZE
                                                   INTO   WRK-HKNNAME
                               END-STRING
                               MOVE    1               TO  FLG-HKNNUM
                           END-IF
                       ELSE
      *                    特定疾患のとき 
                           IF      SUM-KOH-KOHNUM (IDX3 IDX1 IDX IDX2)
                                                   =   "051"
                               MOVE    "（特定疾患）"  TO   WRK-HKNNAME
                           ELSE
      *                    その他の保険のとき 
                               STRING  "（"        DELIMITED  BY  SIZE
                                       HKN-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                                       "）"        DELIMITED  BY  SIZE
                                                   INTO   WRK-HKNNAME
                               END-STRING
                           END-IF
                           MOVE    1               TO  FLG-HKNNUM 
                       END-IF     
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
           MOVE    WRK-KOHKBN          TO  HCM2T-KOHKBN (IDX IDY)
           .
       3202-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<合計>処理
      *****************************************************************
      *    
       330-TERM-HEN-SEC         SECTION.
      *    合計・総合計
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 3)  TO  WRK-KENSUZ
           MOVE    WRK-KENSUZ              TO  HCM2T-KENSUG (2)
           MOVE    SUM-GOK-SHOKENSU (IDX3 IDX1 3)
                                           TO  WRK-SHOKENSUZ
           MOVE    WRK-SHOKENSUZ           TO  HCM2T-SYOKENSUG (2) 
      *
           MOVE    SUM-GOK-KENSU (IDX3 IDX1 4)  TO  WRK-GKENSUZ
           MOVE    WRK-GKENSUZ             TO  HCM2T-GKENSU
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
      *********MOVE    "HCM2I.red"         TO  SPRT-PRTID
      *********MOVE    "HCM2IV01.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2IV02.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2IV03.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2S.red"         TO  SPRT-PRTID
      *********MOVE    "HCM2SV01.red"      TO  SPRT-PRTID
               MOVE    "HCM2SV02.red"      TO  SPRT-PRTID
               MOVE    HCM2S               TO  SPRT-PRTDATA
           ELSE                          
      *********MOVE    "HCM2J.red"         TO  SPRT-PRTID
      *********MOVE    "HCM2JV01.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2JV02.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2JV03.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2JV04.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2T.red"         TO  SPRT-PRTID
      *********MOVE    "HCM2TV01.red"      TO  SPRT-PRTID
      *********MOVE    "HCM2TV02.red"      TO  SPRT-PRTID
               MOVE    "HCM2TV03.red"      TO  SPRT-PRTID
               MOVE    HCM2T               TO  SPRT-PRTDATA
           END-IF    
           IF      WRK-PARA-MARUCHO    =   "2"
               EVALUATE    IDX1    ALSO    WRK-SYOKBN
                                               ALSO    WRK-NYUGAIKBN 
                   WHEN    ANY     ALSO    "1" ALSO    "1"                           
                       MOVE    "マル長分　社保総括表・入院"
                                                   TO  SPRT-TITLE
                   WHEN    ANY     ALSO    "2" ALSO    "1"                           
                       MOVE    "マル長分　社保総括表・入院［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "1" ALSO    "2"                           
                       MOVE    "マル長分　社保総括表・入院外"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "1" ALSO    "2"
                       MOVE
                           "マル長分　社保総括表・入院外（在医総等）"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "2" ALSO    "2"                            
                       MOVE    "マル長分　社保総括表・入院外［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "2" ALSO    "2"          
                       MOVE
                   "マル長分　社保総括表・入院外（在医総等）［返戻分］"
                                                   TO  SPRT-TITLE
               END-EVALUATE
           ELSE
               EVALUATE    IDX1    ALSO    WRK-SYOKBN
                                               ALSO    WRK-NYUGAIKBN 
                   WHEN    ANY     ALSO    "1" ALSO    "1"                           
                       MOVE    "社保総括表・入院"  TO  SPRT-TITLE
                   WHEN    ANY     ALSO    "2" ALSO    "1"                           
                       MOVE    "社保総括表・入院［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "1" ALSO    "2"                           
                       MOVE    "社保総括表・入院外"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "1" ALSO    "2"
                       MOVE    "社保総括表・入院外（在医総等）"
                                                   TO  SPRT-TITLE
                   WHEN    1       ALSO    "2" ALSO    "2"                            
                       MOVE    "社保総括表・入院外［返戻分］"
                                                   TO  SPRT-TITLE
                   WHEN    2       ALSO    "2" ALSO    "2"          
                       MOVE
                           "社保総括表・入院外（在医総等）［返戻分］"
                                                   TO  SPRT-TITLE
               END-EVALUATE
           END-IF
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
           ADD     CNT-PAGE        TO  WRK-PARA-PAGE
      *
           DISPLAY "*** ORCBM033 PAGE  "  CNT-PAGE 
           DISPLAY "*** ORCBM033 END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
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
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
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
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
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
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
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
           MOVE    WRK-SYMD        TO  SYS-1903-STYUKYMD
                                       SYS-1903-EDYUKYMD
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
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
