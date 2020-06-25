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
       PROGRAM-ID.             ORCSDTCHK05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データチェック
      *  コンポーネント名  : 保険・公費情報取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/02    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/04  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-HKNNUM                  PIC 9(05).
           03  IDX-HKNINF                  PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNINF-G.
               05  WRK-HKNKOHKBN                   PIC X(01).
               05  WRK-HKNNUM                      PIC X(03).
               05  WRK-PAYKBN                      PIC X(02).
               05  WRK-HKNJANUM                    PIC X(08).
               05  WRK-HONKZKKBN                   PIC X(01).
               05  WRK-RSIHKNKBN                   PIC X(01).
      *
       01  SRH-AREA.
           03  SRH-IDX                             PIC 9(05).
           03  SRH-HKNNUM-DAT.
               05  SRH-HKNNUM                      PIC X(03).
               05  SRH-HKNNUMMEI                   PIC X(10).
               05  SRH-HKNNUM-MAX                  PIC 9(05).
               05  SRH-HKNNUM-OCC                  OCCURS  50.
                   07  SRH-HKNNUML                 PIC X(03).
                   07  SRH-HKNNUMMEIL              PIC X(10).
           03  SRH-KOHNUM-DAT.
               05  SRH-KOHNUM                      PIC X(03).
               05  SRH-KOHNUMMEI                   PIC X(10).
               05  SRH-PAYKBN                      PIC X(02).
               05  SRH-KOHNUM-MAX                  PIC 9(05).
               05  SRH-KOHNUM-OCC                  OCCURS  150.
                   07  SRH-KOHNUML                 PIC X(03).
                   07  SRH-PAYKBNL                 PIC X(02).
                   07  SRH-KOHNUMMEIL              PIC X(10).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-SDTCHK05-OT-MAX               PIC 9(05)
                                                   VALUE   50.
           03  CONST-SYAHO                         PIC X(01)
                                                   VALUE   "1".
           03  CONST-KOKUHO                        PIC X(01)
                                                   VALUE   "2".
           03  CONST-ROSAI                         PIC X(01)
                                                   VALUE   "3".
           03  CONST-JIBAI                         PIC X(01)
                                                   VALUE   "4".
           03  CONST-JIHI                          PIC X(01)
                                                   VALUE   "5".
           03  CONST-KOUGAI                        PIC X(01)
                                                   VALUE   "6".
           03  CONST-KOUKI                         PIC X(01)
                                                   VALUE   "7".
           03  CONST-CHIKEN                        PIC X(01)
                                                   VALUE   "8".
           03  CONST-DAISAN                        PIC X(01)
                                                   VALUE   "9".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者労災マスタ
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *
      *    保険組合せ取得サブ
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSDTCHK05.INC".
           COPY    "CPORCBSD01PARA.INC".
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
       SDTCHK05AREA
       ORCBSD01PARA
       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SRH-AREA
                                       SDTCHK05-OT
      *
           PERFORM 1001-HKNKOH-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類・公費の種類編集処理
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRH-HKNNUM-DAT
           INITIALIZE                  SRH-KOHNUM-DAT
      *
      *    保険番号検索
           PERFORM 900-HKNNUM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SRH-HKNNUM-MAX  TO  IDX5
      *            保険
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  NOT =   SRH-HKNNUML (IDX5))
      *                    保険情報編集
                           PERFORM 10011-HKN-HEN-SEC
                       END-IF
                   ELSE
      *                保険情報編集
                       PERFORM 10011-HKN-HEN-SEC
                   END-IF
      *
               ELSE
      *            公費
                   MOVE    SRH-KOHNUM-MAX  TO  IDX5
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  =   SRH-KOHNUML (IDX5))
                           IF    ( HKN-HKNNUM  =   "027"  ) 
                             AND ( HKN-PAYKBN  =   "01" OR "02" OR "03")
                               IF ( HKN-PAYKBN
                                           NOT =   SRH-PAYKBNL (IDX5))
      *                            公費情報編集
                                   PERFORM 10011-KOH-HEN-SEC
                               END-IF
                           END-IF
                       ELSE
      *                    公費情報編集
                           PERFORM 10011-KOH-HEN-SEC
                       END-IF
                   ELSE
      *                公費情報編集
                       PERFORM 10011-KOH-HEN-SEC
                   END-IF
               END-IF
      *
      *        保険番号読み込み
               PERFORM 900-HKNNUM-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    保険番号クローズ
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-HKNKOH-CMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       10011-HKN-HEN-SEC          SECTION.
      *
           IF    ( SRH-HKNNUM-MAX      >=  50 )
               GO  TO  10011-HKN-HEN-EXT
           END-IF
      *
           COMPUTE SRH-HKNNUM-MAX  =   SRH-HKNNUM-MAX   +   1
      *
           MOVE    SRH-HKNNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-HKNNUML   (IDX5)
           MOVE    HKN-TANSEIDONAME    TO  SRH-HKNNUMMEIL(IDX5)
      *
           .
       10011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       10011-KOH-HEN-SEC           SECTION.
      *
           IF    ( SRH-KOHNUM-MAX      >=  150 )
               GO  TO  10011-KOH-HEN-EXT
           END-IF
      *
           COMPUTE SRH-KOHNUM-MAX  =   SRH-KOHNUM-MAX  +   1
      *
           MOVE    SRH-KOHNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-KOHNUML (IDX5)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" AND "03" )
               MOVE    "老人保健"      TO  SRH-KOHNUMMEIL (IDX5)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  SRH-KOHNUMMEIL (IDX5)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" OR "03" )
               MOVE    HKN-PAYKBN
                                       TO  SRH-PAYKBNL (IDX5)
           ELSE
               MOVE    SPACE           TO  SRH-PAYKBNL (IDX5)
           END-IF
      *
           .
       10011-KOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2101-PTHKN-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       2101-PTHKN-HEN-SEC              SECTION.
      *
      *    保険組み合わせ
           INITIALIZE                  SDTCHK02AREA
                                       LNK-HKNCOMBI-AREA
      *
           MOVE    SDTCHK05-HOSPNUM     TO  SDTCHK02-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  SDTCHK02-PTID
           CALL    "ORCSDTCHK02"       USING   SDTCHK02AREA
                                               LNK-HKNCOMBI-AREA
                                               SPA-AREA
      *
           PERFORM 21011-PTHKNINF-HEN-SEC
      *
           PERFORM 21011-PTKOHINF-HEN-SEC
      *
           .
       2101-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       21011-PTHKNINF-HEN-SEC          SECTION.
      *
           PERFORM 900-PTHKNINF-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTHKNINF    NOT =   ZERO )
      *
               PERFORM 210111-PTHKNINF-CHK-SEC
      *
               PERFORM 900-PTHKNINF-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-PTHKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       210111-PTHKNINF-CHK-SEC         SECTION.
      *
           IF    ( PTHKN-SAKJOKBN   =  SPACE )
            AND  ( ORCBSD01-SRYYM  >=  PTHKN-TEKSTYMD (1:6))
            AND  ( ORCBSD01-SRYYM  <=  PTHKN-TEKEDYMD (1:6))
               INITIALIZE                  WRK-HKNINF-G
               MOVE    "0"             TO  WRK-HKNKOHKBN
               MOVE    PTHKN-HKNNUM    TO  WRK-HKNNUM
               MOVE    PTHKN-HKNJANUM  TO  WRK-HKNJANUM
               MOVE    PTHKN-HONKZKKBN TO  WRK-HONKZKKBN
               IF    ( PTHKN-HKNNUM    =   "971"   OR  "973" )
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
                   MOVE    PTRSI-HKNKBN
                                       TO  WRK-RSIHKNKBN
               END-IF
               IF    ( CONST-SDTCHK05-OT-MAX   >   SDTCHK05-OT-MAX )
                   PERFORM 210111-HKNNUM-CHK-SEC
               END-IF
           END-IF
      *
           .
       210111-PTHKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険番号チェック処理
      *****************************************************************
       210111-HKNNUM-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TARGET
      *
           MOVE    ZERO            TO  IDX-HKNNUM
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   SDTCHK02-O-HKNINF-MAX )
                    OR   ( IDX-HKNNUM  NOT =   ZERO )
      *
               IF    ( SDTCHK02-O-HKNNUM (IDX0)
                                   =   WRK-HKNNUM )
                   MOVE    IDX0    TO  IDX-HKNNUM
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-HKNNUM      NOT =   ZERO )
               IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                   EVALUATE    SDTCHK05-HKNKBN
                   WHEN    CONST-SYAHO
                       IF   ( SDTCHK02-O-SYAHO (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOKUHO
                       IF   ( SDTCHK02-O-KOKUHO(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-ROSAI
                       IF   ( SDTCHK02-O-ROSAI (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-JIBAI
                       IF   ( SDTCHK02-O-JIBAI (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOUGAI
                       IF   ( SDTCHK02-O-KOUGAI(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOUKI
                       IF   ( SDTCHK02-O-KOUKI(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-JIHI
                       IF   ( SDTCHK02-O-JIHI  (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-CHIKEN
                       IF   ( SDTCHK02-O-CHIKEN  (IDX-HKNNUM)  =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-DAISAN
                       IF   ( SDTCHK02-O-DAISAN  (IDX-HKNNUM)  =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   END-EVALUATE
               ELSE
                   IF    ( ORCBSD01-SYAHOPROCKBN   =   "1" )
                    AND  ( SDTCHK02-O-SYAHO    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KOKUHOPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-KOKUHO   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-RSIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-ROSAI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-JBIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-JIBAI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KGIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-KOUGAI   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KOUKIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-KOUKI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-JIHIPROCKBN    =   "1" )
                    AND  ( SDTCHK02-O-JIHI     (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-CHIKENPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-CHIKEN   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-DAISANPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-DAISAN   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-TARGET              =   1 )
               MOVE    ZERO            TO  IDX-HKNINF
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   SDTCHK05-OT-MAX )
      *
                   IF    ( SDTCHK05-HKNKOHKBN (IDX1)
                                               =   WRK-HKNKOHKBN )
                    AND  ( SDTCHK05-HKNNUM    (IDX1)
                                               =   WRK-HKNNUM )
                    AND  ( SDTCHK05-PAYKBN    (IDX1)
                                               =   WRK-PAYKBN )
                    AND  ( SDTCHK05-HKNJANUM  (IDX1)
                                               =   WRK-HKNJANUM )
                    AND  ( SDTCHK05-HONKZKKBN (IDX1)
                                               =   WRK-HONKZKKBN )
                    AND  ( SDTCHK05-RSIHKNKBN (IDX1)
                                               =   WRK-RSIHKNKBN )
                       MOVE    IDX1    TO  IDX-HKNINF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-HKNINF      =   ZERO )
      *
                   COMPUTE SDTCHK05-OT-MAX
                       =   SDTCHK05-OT-MAX
                       +   1
      *
                   MOVE    SDTCHK05-OT-MAX     TO  IDX-HKNINF
      *
                   MOVE    WRK-HKNKOHKBN       TO  SDTCHK05-HKNKOHKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-HKNNUM          TO  SDTCHK05-HKNNUM
                                                           (IDX-HKNINF)
      *
                   IF    ( WRK-HKNKOHKBN       =   ZERO )
                       IF    ( WRK-HKNNUM      =   "971" OR  "973" )
                           EVALUATE    WRK-HKNNUM  ALSO  WRK-RSIHKNKBN
                           WHEN    "971"       ALSO    "1"
                               MOVE    "労災保険　短"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "2"
                               MOVE    "労災保険　傷"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "3"
                               MOVE    "労災保険　ア"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "5"
                               MOVE    "労災保険　公"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    ANY
                               MOVE    "労災保険"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "973"       ALSO    ANY
                               MOVE    "自賠責保険"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           END-EVALUATE
                       ELSE
                           MOVE    WRK-HKNNUM  TO  SRH-HKNNUM
                           PERFORM 2111-HKNNUM-GET-SEC
                           MOVE    SRH-HKNNUMMEI
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                       END-IF
                   ELSE
                       MOVE    WRK-HKNNUM      TO  SRH-KOHNUM
                       MOVE    WRK-PAYKBN      TO  SRH-PAYKBN
                       PERFORM 2111-KOHNUM-GET-SEC
                       MOVE    SRH-KOHNUMMEI   TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                   END-IF
                   MOVE    WRK-PAYKBN          TO  SDTCHK05-PAYKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-HKNJANUM        TO  SDTCHK05-HKNJANUM
                                                           (IDX-HKNINF)
                   MOVE    WRK-HONKZKKBN       TO  SDTCHK05-HONKZKKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-RSIHKNKBN       TO  SDTCHK05-RSIHKNKBN
                                                           (IDX-HKNINF)
               END-IF
      *
           END-IF
      *
           .
       210111-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報編集処理
      *****************************************************************
       21011-PTKOHINF-HEN-SEC          SECTION.
      *
           PERFORM 900-PTKOHINF-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTKOHINF    NOT =   ZERO )
      *
               PERFORM 210111-PTKOHINF-CHK-SEC
      *
               PERFORM 900-PTKOHINF-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報編集処理
      *****************************************************************
       210111-PTKOHINF-CHK-SEC         SECTION.
      *
           IF    ( PTKOH-SAKJOKBN   =  SPACE )
            AND  ( ORCBSD01-SRYYM  >=  PTKOH-TEKSTYMD (1:6))
            AND  ( ORCBSD01-SRYYM  <=  PTKOH-TEKEDYMD (1:6))
               INITIALIZE                  WRK-HKNINF-G
               MOVE    "1"             TO  WRK-HKNKOHKBN
               MOVE    PTKOH-KOHNUM    TO  WRK-HKNNUM
               IF    ( PTKOH-KOHNUM    =   "027" )
                AND  ( PTKOH-PAYKBN    =   "01" OR "02" OR "03" )
                   MOVE    PTKOH-PAYKBN
                                       TO  WRK-PAYKBN
               END-IF
               IF    ( CONST-SDTCHK05-OT-MAX   >   SDTCHK05-OT-MAX )
                   PERFORM 210111-HKNNUM-CHK-SEC
               END-IF
           END-IF
      *
           .
       210111-PTKOHINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類取得処理
      *****************************************************************
       2111-HKNNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-HKNNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-HKNNUM-MAX )
               IF    ( SRH-HKNNUM  =   SRH-HKNNUML (SRH-IDX) )
                   MOVE    SRH-HKNNUMMEIL (SRH-IDX)
                                       TO  SRH-HKNNUMMEI
                   MOVE    SRH-HKNNUM-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
      *
           .
       2111-HKNNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類取得処理
      *****************************************************************
       2111-KOHNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-KOHNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-KOHNUM-MAX )
               IF    ( SRH-KOHNUM  =   "027" )
                AND  ( SRH-PAYKBN  =   "01" OR "02" OR "03" )
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBN  =   SRH-PAYKBNL (SRH-IDX) )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               ELSE
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBNL (SRH-IDX)   =   SPACE )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2111-KOHNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY6-SEL-SEC       SECTION.
      *
      *    患者保険マスタ検索
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                      PTHKNINF-REC
           MOVE    SDTCHK05-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           .
       900-PTHKNINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報FETCH処理
      *****************************************************************
       900-PTHKNINF-KEY6-FET-SEC       SECTION.
      *
      *    患者保険マスタ検索
           MOVE    ZERO                TO  FLG-PTHKNINF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           .
       900-PTHKNINF-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報検索処理
      *****************************************************************
       900-HKNNUM-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SDTCHK05-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  HKN-TEKSTYMD
           MOVE    "01"                TO  HKN-TEKSTYMD (7:2)
           MOVE    HKN-TEKSTYMD        TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号情報FETCH処理
      *****************************************************************
       900-HKNNUM-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY2-SEL-SEC       SECTION.
      *
      *    患者公費マスタ検索
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                      PTKOHINF-REC
           MOVE    SDTCHK05-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報FETCH処理
      *****************************************************************
       900-PTKOHINF-KEY2-FET-SEC       SECTION.
      *
      *    患者公費マスタ検索
           MOVE    ZERO                TO  FLG-PTKOHINF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者労災情報検索処理
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
      *    患者公費マスタ検索
           MOVE    ZERO                TO  FLG-PTRSIINF
           INITIALIZE                      PTRSIINF-REC
           MOVE    PTHKN-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    PTHKN-PTID          TO  PTRSI-PTID
           MOVE    PTHKN-HKNID         TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
