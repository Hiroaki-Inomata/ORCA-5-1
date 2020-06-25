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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSTOKNYUIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院
      *  コンポーネント名  : 特定入院料コード取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/08    NACL−小豆沢  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      *令和２年４月改正対応
      *  05.00.00    NACL-小豆沢  20/03/09  短期滞在３追加対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSK5001.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    特定入院料コード取得パラメタ
           COPY    "CPORCSTOKNYUIN.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSTOKNYUIN-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF  LNK-TOKNYUIN-SANTEIYM    >=   "202004"
               PERFORM    206-EDIT-SEC
           ELSE
             IF  LNK-TOKNYUIN-SANTEIYM    >=   "201804"
                 PERFORM    205-EDIT-SEC
             ELSE
               IF  LNK-TOKNYUIN-SANTEIYM    >=   "201604"
                   PERFORM    204-EDIT-SEC
               ELSE
                 IF  LNK-TOKNYUIN-SANTEIYM    >=   "201404"
                     PERFORM    203-EDIT-SEC
                 ELSE
                    IF  LNK-TOKNYUIN-SANTEIYM    >=   "201204"
                        PERFORM    202-EDIT-SEC
                    ELSE
                        IF  LNK-TOKNYUIN-SANTEIYM    >=   "201004"
                            PERFORM    201-EDIT-SEC
                        ELSE
                            PERFORM    200-EDIT-SEC
                        END-IF
                    END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
           .
      *
       000-PROC-EXT.
           EXIT   PROGRAM
           .
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年３月まで）
      *****************************************************************
       200-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料２
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料３
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料４
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料Ａ
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料Ｂ
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料Ｃ
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料Ｄ
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料Ｅ
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料Ａ（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料Ｂ（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料Ｃ（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料Ｄ（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料Ｅ（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２２年４月から平成２４年３月まで）
      *****************************************************************
       201-EDIT-SEC                    SECTION.
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症病棟入院料１
                       MOVE   "190739910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                老人性認知症疾患療養病棟入院料
                       MOVE   "190740110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科救急入院料
                       MOVE   "190111410"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "11"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190066910"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "12"
                   IF  LNK-TOKNYUIN-SANTEIYM <= "200603"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190067010"   TO  LNK-TOKNYUIN-SRYCD
                   ELSE
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
                   END-IF
               WHEN    "13"
      *                回復期リハビリテーション病棟入院料
                       MOVE   "190075910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症病棟入院料２
                       MOVE   "190813410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                回復期リハビリテーション病棟入院料（生活療養）
                       MOVE   "190124910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                回復期リハビリテーション病棟入院料１（基準未適合）
                       MOVE   "190129410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                回復期リハビリテーション病棟入院料１（生活療養）（基準未適合）
                       MOVE   "190129510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                回復期リハビリテーション病棟入院料２（基準未適合）
                       MOVE   "190129610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                回復期リハビリテーション病棟入院料２（生活療養）（基準未適合）
                       MOVE   "190129710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                後期高齢者特定入院基本料
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                後期高齢者特定入院基本料（後期高齢者特別入院基本料算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       201-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２４年４月から平成２６年３月まで）
      *****************************************************************
       202-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料１
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")   AND
                            (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                亜急性期入院医療管理料１（指定地域）
                       MOVE   "190150310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "93"
      *                亜急性期入院医療管理料２（指定地域）
                       MOVE   "190150410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "95"
      *                特定一般病棟入院料（亜急性期入院医療管理）
                       MOVE   "190152610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "96"
      *                特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
                       MOVE   "190152710"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
           IF    LNK-TOKNYUIN-SRYCD    =     SPACE
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       202-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２６年４月から平成２８年３月まで））
      *****************************************************************
       203-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190175310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190175410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190175710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190175810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190176110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190176210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190176510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190176610"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                診療所後期高齢者医療管理料
                       MOVE   "190774110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                救命救急入院料１（高度）
                       MOVE   "190110210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                救命救急入院料１（Ａ）
                       MOVE   "190110310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                救命救急入院料２（高度）
                       MOVE   "190110410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                救命救急入院料２（Ａ）
                       MOVE   "190110510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                特定集中治療室管理料（重症者等比率基準適合）
                       MOVE   "190024610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "13"
      *                広範囲熱傷特定集中治療室管理料
                       MOVE   "190024810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190075510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                老人一般病棟入院医療管理料
                       MOVE   "190788810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                ハイケアユニット入院医療管理料
                       MOVE   "190117310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "18"
      *                亜急性期入院医療管理料１
                       MOVE   "190117410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                診療所後期高齢者医療管理料（３０日を経過しない日の再入院）
                       MOVE   "190808910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                診療所後期高齢者医療管理料（３０日未経過の再入院）（生活療養）
                       MOVE   "190125210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                短期滞在手術基本料３
                       MOVE   "190130410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                短期滞在手術基本料３（生活療養）
                       MOVE   "190130510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                亜急性期入院医療管理料２
                       MOVE   "190129910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "92"
      *                亜急性期入院医療管理料１（指定地域）
                       MOVE   "190150310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "93"
      *                亜急性期入院医療管理料２（指定地域）
                       MOVE   "190150410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "95"
      *                特定一般病棟入院料（亜急性期入院医療管理）
                       MOVE   "190152610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "96"
      *                特定一般病棟入院料（亜急性期入院医療管理・リハビリ算定患者）
                       MOVE   "190152710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "97"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））
                       MOVE   "190177610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "98"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））（生活）
                       MOVE   "190177710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "99"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））
                       MOVE   "190177810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9A"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））（生活）
                       MOVE   "190177910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9H"
      *                短手３（腋臭症手術（皮膚有毛部切除術））
                       MOVE   "190178610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9I"
      *                短手３（腋臭症手術（皮膚有毛部切除術））（生活）
                       MOVE   "190178710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他のもの）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他のもの）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9P"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合））
                       MOVE   "190179410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Q"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合））（生活）
                       MOVE   "190179510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A3"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））
                       MOVE   "190180810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A4"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））（生活）
                       MOVE   "190180910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190175510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190175610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190175910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190176010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190176310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190176410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190176710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190176810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア入院医療管理１）
                       MOVE   "190182310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア入院医療管理２）
                       MOVE   "190182410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AW"
      *                短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）
                       MOVE   "190182750"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AX"
      *                短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）（生活）
                       MOVE   "190182850"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
      *            緩和ケア病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "41"
               WHEN    "45"
               WHEN    "AL"
               WHEN    "AP"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "42"
               WHEN    "46"
               WHEN    "AM"
               WHEN    "AQ"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "29"
               WHEN    "43"
               WHEN    "47"
               WHEN    "AN"
               WHEN    "AR"
      *            回復期リハビリテーション病棟入院料３
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "30"
               WHEN    "44"
               WHEN    "48"
               WHEN    "AO"
               WHEN    "AS"
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
      *            特定一般病棟入院料（地域包括ケア入院医療管理１）
      *            特定一般病棟入院料（地域包括ケア入院医療管理２）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       203-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成２８年４月から平成３０年３月まで）
      *****************************************************************
       204-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190141610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190141710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190141810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190141910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190150110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190150210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190175310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190175410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190175710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190175810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190176110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190176210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190176510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190176610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院基本料１（入院基本料Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院基本料１（入院基本料Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院基本料１（入院基本料Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院基本料１（入院基本料Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院基本料１（入院基本料Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院基本料１（入院基本料Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院基本料１（入院基本料Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院基本料１（入院基本料Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院基本料１（入院基本料Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院基本料１（入院基本料Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院基本料１（入院基本料Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院基本料１（入院基本料Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院基本料１（入院基本料Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院基本料１（入院基本料Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院基本料１（入院基本料Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院基本料１（入院基本料Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院基本料１（入院基本料Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院基本料１（入院基本料Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院基本料２（入院基本料Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院基本料２（入院基本料Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院基本料２（入院基本料Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院基本料２（入院基本料Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院基本料２（入院基本料Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院基本料２（入院基本料Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院基本料２（入院基本料Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院基本料２（入院基本料Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院基本料２（入院基本料Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院基本料２（入院基本料Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院基本料２（入院基本料Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院基本料２（入院基本料Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院基本料２（入院基本料Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院基本料２（入院基本料Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院基本料２（入院基本料Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院基本料２（入院基本料Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院基本料２（入院基本料Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院基本料２（入院基本料Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "97"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））
                       MOVE   "190177610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "98"
      *                短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））（生活）
                       MOVE   "190177710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "99"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））
                       MOVE   "190177810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9A"
      *                短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））（生活）
                       MOVE   "190177910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9H"
      *                短手３（腋臭症手術（皮膚有毛部切除術））
                       MOVE   "190178610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9I"
      *                短手３（腋臭症手術（皮膚有毛部切除術））（生活）
                       MOVE   "190178710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9P"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合）・片側）
                       MOVE   "190179410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Q"
      *                短手３（水晶体再建術（眼内レンズ挿入しない場合）・片側）（生活）
                       MOVE   "190179510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190175510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190175610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190175910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190176010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190176310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190176410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190176710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190176810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア入院医療管理１）
                       MOVE   "190182310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア入院医療管理２）
                       MOVE   "190182410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B0"
      *                短手３（水晶体再建術・眼内レンズ挿入しない・両側）
                       MOVE   "190196110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B1"
      *                短手３（水晶体再建術・眼内レンズ挿入しない・両側）（生活）
                       MOVE   "190196210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・一般病棟７対１入院基本料
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・一般病棟１０対１入院基本料
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・一般病棟１３対１入院基本料
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・一般病棟１５対１入院基本料
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
      *            緩和ケア病棟入院料
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "41"
               WHEN    "45"
               WHEN    "AL"
               WHEN    "AP"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "42"
               WHEN    "46"
               WHEN    "AM"
               WHEN    "AQ"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "29"
               WHEN    "43"
               WHEN    "47"
               WHEN    "AN"
               WHEN    "AR"
      *            回復期リハビリテーション病棟入院料３
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "30"
               WHEN    "44"
               WHEN    "48"
               WHEN    "AO"
               WHEN    "AS"
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院基本料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
      *            特定一般病棟入院料（地域包括ケア入院医療管理１）
      *            特定一般病棟入院料（地域包括ケア入院医療管理２）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       204-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（平成３０年４月から令和２年３月まで）
      *****************************************************************
       205-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料１
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190208110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190208210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190208310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190208410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190207910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190208010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190209110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190209210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190209510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190209610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190210710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190210810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190211110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190211210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                緩和ケア病棟入院料２
                       MOVE   "190212710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                回復期リハビリテーション病棟入院料４
                       MOVE   "190208510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                回復期リハビリテーション病棟入院料４（生活療養）
                       MOVE   "190208610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                回復期リハビリテーション病棟入院料５
                       MOVE   "190208710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                回復期リハビリテーション病棟入院料５（生活療養）
                       MOVE   "190208810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                回復期リハビリテーション病棟入院料６
                       MOVE   "190208910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                回復期リハビリテーション病棟入院料６（生活療養）
                       MOVE   "190209010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                地域包括ケア病棟入院料３
                       MOVE   "190209910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                地域包括ケア病棟入院料３（生活療養）
                       MOVE   "190210010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                地域包括ケア病棟入院料４
                       MOVE   "190210310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                地域包括ケア病棟入院料４（生活療養）
                       MOVE   "190210410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                地域包括ケア病棟入院料３（特定地域）
                       MOVE   "190211510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                地域包括ケア病棟入院料３（生活療養）（特定地域）
                       MOVE   "190211610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                地域包括ケア病棟入院料４（特定地域）
                       MOVE   "190211910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                地域包括ケア病棟入院料４（生活療養）（特定地域）
                       MOVE   "190212010"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院料１（Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院料１（Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院料１（Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院料１（Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院料１（Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院料１（Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院料１（Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院料１（Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院料１（Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院料１（Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院料１（Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院料１（Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院料１（Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院料１（Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院料１（Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院料１（Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院料１（Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院料１（Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院料２（Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院料２（Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院料２（Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院料２（Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院料２（Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院料２（Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院料２（Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院料２（Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院料２（Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院料２（Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院料２（Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院料２（Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院料２（Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院料２（Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院料２（Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院料２（Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院料２（Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院料２（Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190209310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190209410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190209710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190209810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190210910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190211010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190211310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190211410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア１）
                       MOVE   "190213310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア２）
                       MOVE   "190213410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・急性期一般入院料１
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・急性期一般入院料７
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・地域一般入院料２
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・地域一般入院料３
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BZ"
      *                地域包括ケア入院医療管理料３
                       MOVE   "190210110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C0"
      *                地域包括ケア入院医療管理料３（生活療養）
                       MOVE   "190210210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C1"
      *                地域包括ケア入院医療管理料４
                       MOVE   "190210510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C2"
      *                地域包括ケア入院医療管理料４（生活療養）
                       MOVE   "190210610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C3"
      *                地域包括ケア入院医療管理料３（特定地域）
                       MOVE   "190211710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C4"
      *                地域包括ケア入院医療管理料３（生活療養）（特定地域）
                       MOVE   "190211810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C5"
      *                地域包括ケア入院医療管理料４（特定地域）
                       MOVE   "190212110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C6"
      *                地域包括ケア入院医療管理料４（生活療養）（特定地域）
                       MOVE   "190212210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C7"
      *                療養病棟・急性期一般入院料２
                       MOVE   "190201910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C8"
      *                療養病棟・急性期一般入院料３
                       MOVE   "190202010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C9"
      *                療養病棟・急性期一般入院料４
                       MOVE   "190202110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CA"
      *                療養病棟・急性期一般入院料５
                       MOVE   "190202210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CB"
      *                療養病棟・急性期一般入院料６
                       MOVE   "190202310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CC"
      *                療養病棟・地域一般入院料１
                       MOVE   "190202410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CD"
      *                特定一般病棟入院料（地域包括ケア３）
                       MOVE   "190213510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CE"
      *                特定一般病棟入院料（地域包括ケア４）
                       MOVE   "190213610"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "57"
               WHEN    "59"
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
      *            回復期リハビリテーション病棟入院料５
      *            回復期リハビリテーション病棟入院料６
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "58"
               WHEN    "60"
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            回復期リハビリテーション病棟入院料６（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "C1"
               WHEN    "C5"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "C2"
               WHEN    "C6"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       205-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定入院料コードセット（令和２年４月から）
      *****************************************************************
       206-EDIT-SEC                    SECTION.
      *
      *    システム管理検索（相互算定区分取得）
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                      SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    LNK-TOKNYUIN-BTUNUM TO  SYS-5001-KBNCD
           MOVE    LNK-TOKNYUIN-SANTEIYM
                                       TO  SYS-5001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-5001-STYUKYMD(7:2)
           MOVE    SYS-5001-STYUKYMD   TO  SYS-5001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYS-5001-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    SPACE               TO  SYS-5001-REC
               INITIALIZE                  SYS-5001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  LNK-TOKNYUIN-HENKYAKU-AREA
           MOVE    ZERO                TO  LNK-TOKNYUIN-RC
      *
      *    病棟
           IF  LNK-TOKNYUIN-KBN   =   "2"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                小児入院医療管理料３
                       MOVE   "190111110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "02"
      *                小児入院医療管理料４
                       MOVE   "190111210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                特殊疾患病棟入院料１
                       MOVE   "190055210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "04"
      *                特殊疾患病棟入院料２
                       MOVE   "190055310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "05"
      *                認知症治療病棟入院料１
                       MOVE   "190151610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "07"
      *                精神療養病棟入院料
                       MOVE   "190055010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "08"
      *                精神療養病棟入院料２
                       MOVE   "190055110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "09"
      *                緩和ケア病棟入院料１
                       MOVE   "190028910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                精神科救急入院料１
                       MOVE   "190121010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                精神科急性期治療病棟入院料１
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                精神科急性期治療病棟入院料２
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                小児入院医療管理料５
                       MOVE   "190075810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "15"
      *                認知症治療病棟入院料２
                       MOVE   "190151710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "17"
      *                小児入院医療管理料１
                       MOVE   "190129310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                精神科救急入院料２
                       MOVE   "190130010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "23"
      *                精神科救急・合併症入院料
                       MOVE   "190130210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                精神科急性期治療病棟入院料１（急性増悪の再入院）
                       MOVE   "190121110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                精神科急性期治療病棟入院料２（急性増悪の再入院）
                       MOVE   "190121210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "26"
      *                小児入院医療管理料２
                       MOVE   "190140710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "27"
      *                回復期リハビリテーション病棟入院料２
                       MOVE   "190208110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "28"
      *                回復期リハビリテーション病棟入院料２（生活療養）
                       MOVE   "190208210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "29"
      *                回復期リハビリテーション病棟入院料３
                       MOVE   "190208310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "30"
      *                回復期リハビリテーション病棟入院料３（生活療養）
                       MOVE   "190208410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                回復期リハビリテーション病棟入院料１
                       MOVE   "190207910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "32"
      *                回復期リハビリテーション病棟入院料１（生活療養）
                       MOVE   "190208010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "33"
      *                特定一般病棟入院料１
                       MOVE   "190151910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "34"
      *                特定一般病棟入院料２
                       MOVE   "190152010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "35"
      *                療養病床・有床診療所入院基本料４
                       MOVE   "099999950"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "36"
      *                療養病床・有床診療所入院基本料５
                       MOVE   "099999951"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病床・有床診療所入院基本料６
                       MOVE   "099999952"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病床・有床診療所入院基本料１
                       MOVE   "099999955"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病床・有床診療所入院基本料２
                       MOVE   "099999956"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病床・有床診療所入院基本料３
                       MOVE   "099999957"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                地域包括ケア病棟入院料１
                       MOVE   "190209110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                地域包括ケア病棟入院料１（生活療養）
                       MOVE   "190209210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                地域包括ケア病棟入院料２
                       MOVE   "190209510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "44"
      *                地域包括ケア病棟入院料２（生活療養）
                       MOVE   "190209610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "45"
      *                地域包括ケア病棟入院料１（特定地域）
                       MOVE   "190210710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "46"
      *                地域包括ケア病棟入院料１（生活療養）（特定地域）
                       MOVE   "190210810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "47"
      *                地域包括ケア病棟入院料２（特定地域）
                       MOVE   "190211110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "48"
      *                地域包括ケア病棟入院料２（生活療養）（特定地域）
                       MOVE   "190211210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                特殊疾患病棟入院料１（医療区分２の患者に相当するもの）
                       MOVE   "190195010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                特殊疾患病棟入院料１（医療区分１の患者に相当するもの）
                       MOVE   "190195110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "51"
      *                特殊疾患病棟入院料２（医療区分２の患者に相当するもの）
                       MOVE   "190195210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "52"
      *                特殊疾患病棟入院料２（医療区分１の患者に相当するもの）
                       MOVE   "190195310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "53"
      *                地域移行機能強化病棟入院料
                       MOVE   "190195510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "54"
      *                緩和ケア病棟入院料２
                       MOVE   "190212710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                回復期リハビリテーション病棟入院料４
                       MOVE   "190208510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                回復期リハビリテーション病棟入院料４（生活療養）
                       MOVE   "190208610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                回復期リハビリテーション病棟入院料５
                       MOVE   "190208710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                回復期リハビリテーション病棟入院料５（生活療養）
                       MOVE   "190208810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                回復期リハビリテーション病棟入院料６
                       MOVE   "190208910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                回復期リハビリテーション病棟入院料６（生活療養）
                       MOVE   "190209010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                地域包括ケア病棟入院料３
                       MOVE   "190209910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                地域包括ケア病棟入院料３（生活療養）
                       MOVE   "190210010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                地域包括ケア病棟入院料４
                       MOVE   "190210310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "64"
      *                地域包括ケア病棟入院料４（生活療養）
                       MOVE   "190210410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                地域包括ケア病棟入院料３（特定地域）
                       MOVE   "190211510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "66"
      *                地域包括ケア病棟入院料３（生活療養）（特定地域）
                       MOVE   "190211610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                地域包括ケア病棟入院料４（特定地域）
                       MOVE   "190211910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "68"
      *                地域包括ケア病棟入院料４（生活療養）（特定地域）
                       MOVE   "190212010"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    病室
           IF  LNK-TOKNYUIN-KBN   =   "3"
             EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "01"
      *                特殊疾患入院医療管理料
                       MOVE   "190075710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "03"
      *                救命救急入院料１
                       MOVE   "190024510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "06"
      *                救命救急入院料２
                       MOVE   "190024310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "10"
      *                新生児特定集中治療室管理料１
                       MOVE   "190024710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "11"
      *                総合周産期特定集中治療室管理料（母体・胎児）
                       MOVE   "190066710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "12"
      *                総合周産期特定集中治療室管理料（新生児）
                       MOVE   "190066810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "14"
      *                一類感染症患者入院医療管理料
                       MOVE   "190194310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "16"
      *                特定集中治療室管理料３
                       MOVE   "190116310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "19"
      *                脳卒中ケアユニット入院医療管理料
                       MOVE   "190120810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "20"
      *                短期滞在手術基本料１
                       MOVE   "190076710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "21"
      *                短期滞在手術基本料２
                       MOVE   "190076810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "22"
      *                特定入院基本料（一般病棟入院基本料）
                       MOVE   "190799410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "24"
      *                特定入院基本料（特別入院基本料等算定患者）
                       MOVE   "190799510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "25"
      *                療養病棟入院料１（Ａ）
                       MOVE   "190121310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "26"
      *                療養病棟入院料１（Ｂ）
                       MOVE   "190121410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190157610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "27"
      *                療養病棟入院料１（Ｃ）
                       MOVE   "190121510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "28"
      *                療養病棟入院料１（Ｄ）
                       MOVE   "190121610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190162810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "29"
      *                療養病棟入院料１（Ｅ）
                       MOVE   "190121710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190158810"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "30"
      *                療養病棟入院基本料（特別入院基本料）
                       MOVE   "190121810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "31"
      *                有床診療所療養病床入院基本料Ａ
                       MOVE   "190122010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "32"
      *                有床診療所療養病床入院基本料Ｂ
                       MOVE   "190122110"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "33"
      *                有床診療所療養病床入院基本料Ｃ
                       MOVE   "190122210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "34"
      *                有床診療所療養病床入院基本料Ｄ
                       MOVE   "190122310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "35"
      *                有床診療所療養病床入院基本料Ｅ
                       MOVE   "190122410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "36"
      *                有床診療所療養病床入院基本料（特別入院基本料）
                       MOVE   "190122510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "37"
      *                療養病棟入院料１（Ａ）（生活療養）
                       MOVE   "190123710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "38"
      *                療養病棟入院料１（Ｂ）（生活療養）
                       MOVE   "190123810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "39"
      *                療養病棟入院料１（Ｃ）（生活療養）
                       MOVE   "190123910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "40"
      *                療養病棟入院料１（Ｄ）（生活療養）
                       MOVE   "190124010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "41"
      *                療養病棟入院料１（Ｅ）（生活療養）
                       MOVE   "190124110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "42"
      *                療養病棟入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "43"
      *                有床診療所療養病床入院基本料Ａ（生活療養）
                       MOVE   "190124310"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "44"
      *                有床診療所療養病床入院基本料Ｂ（生活療養）
                       MOVE   "190124410"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190155810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "45"
      *                有床診療所療養病床入院基本料Ｃ（生活療養）
                       MOVE   "190124510"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "46"
      *                有床診療所療養病床入院基本料Ｄ（生活療養）
                       MOVE   "190124610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190156610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "47"
      *                有床診療所療養病床入院基本料Ｅ（生活療養）
                       MOVE   "190124710"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "08")   AND
                            (SYS-5001-CRYOYOKBN   =   "1")
                           MOVE   "190157010"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "48"
      *                有床診療所療養病床入院基本料（特別入院基本料）（生活療養）
                       MOVE   "190124810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "49"
      *                診療所後期高齢者医療管理料（生活療養）
                       MOVE   "190125010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "50"
      *                短期滞在手術基本料２（生活療養）
                       MOVE   "190125310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "55"
      *                救命救急入院料３（救命救急入院料）
                       MOVE   "190138110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "56"
      *                救命救急入院料３（広範囲熱傷特定集中治療）
                       MOVE   "190138410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "57"
      *                救命救急入院料４（救命救急入院料）
                       MOVE   "190138710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "58"
      *                救命救急入院料４（広範囲熱傷特定集中治療）
                       MOVE   "190139010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "59"
      *                特定集中治療室管理料４（特定集中治療室管理料）
                       MOVE   "190139810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "60"
      *                特定集中治療室管理料４（広範囲熱傷特定集中治療）
                       MOVE   "190140010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "61"
      *                新生児特定集中治療室管理料２
                       MOVE   "190140410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "62"
      *                新生児治療回復室入院医療管理料
                       MOVE   "190140510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "63"
      *                療養病棟入院料１（Ｆ）
                       MOVE   "190131610"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159210"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163210"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "64"
      *                療養病棟入院料１（Ｆ）（生活療養）
                       MOVE   "190131710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "65"
      *                療養病棟入院料１（Ｇ）
                       MOVE   "190131810"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190159610"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163410"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "66"
      *                療養病棟入院料１（Ｇ）（生活療養）
                       MOVE   "190131910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "67"
      *                療養病棟入院料１（Ｈ）
                       MOVE   "190132010"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160010"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163610"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "68"
      *                療養病棟入院料１（Ｈ）（生活療養）
                       MOVE   "190132110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "69"
      *                療養病棟入院料１（Ｉ）
                       MOVE   "190132210"   TO  LNK-TOKNYUIN-SRYCD
                       IF   (SYS-5001-BTU-SBT     =   "01")
                         IF (SYS-5001-BTU-TOKTEI-NYUIN = "33" OR "34")
                           MOVE   "190160410"   TO  LNK-TOKNYUIN-SRYCD
                         ELSE
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                         END-IF
                       END-IF
                       IF   (SYS-5001-BTU-SBT     =   "06")
                           MOVE   "190163810"   TO  LNK-TOKNYUIN-SRYCD
                       END-IF
               WHEN    "70"
      *                療養病棟入院料１（Ｉ）（生活療養）
                       MOVE   "190132310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "71"
      *                療養病棟入院料２（Ａ）
                       MOVE   "190132410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "72"
      *                療養病棟入院料２（Ａ）（生活療養）
                       MOVE   "190132510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "73"
      *                療養病棟入院料２（Ｂ）
                       MOVE   "190132610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "74"
      *                療養病棟入院料２（Ｂ）（生活療養）
                       MOVE   "190132710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "75"
      *                療養病棟入院料２（Ｃ）
                       MOVE   "190132810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "76"
      *                療養病棟入院料２（Ｃ）（生活療養）
                       MOVE   "190132910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "77"
      *                療養病棟入院料２（Ｄ）
                       MOVE   "190133010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "78"
      *                療養病棟入院料２（Ｄ）（生活療養）
                       MOVE   "190133110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "79"
      *                療養病棟入院料２（Ｅ）
                       MOVE   "190133210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "80"
      *                療養病棟入院料２（Ｅ）（生活療養）
                       MOVE   "190133310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "81"
      *                療養病棟入院料２（Ｆ）
                       MOVE   "190133410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "82"
      *                療養病棟入院料２（Ｆ）（生活療養）
                       MOVE   "190133510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "83"
      *                療養病棟入院料２（Ｇ）
                       MOVE   "190133610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "84"
      *                療養病棟入院料２（Ｇ）（生活療養）
                       MOVE   "190133710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "85"
      *                療養病棟入院料２（Ｈ）
                       MOVE   "190133810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "86"
      *                療養病棟入院料２（Ｈ）（生活療養）
                       MOVE   "190133910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "87"
      *                療養病棟入院料２（Ｉ）
                       MOVE   "190134010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "88"
      *                療養病棟入院料２（Ｉ）（生活療養）
                       MOVE   "190134110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "91"
      *                小児特定集中治療室管理料
                       MOVE   "190149910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "94"
      *                児童・思春期精神科入院医療管理料
                       MOVE   "190151110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9B"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））
                       MOVE   "190178010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9C"
      *                短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
                       MOVE   "190178110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9D"
      *                短手３（小児食物アレルギー負荷検査）
                       MOVE   "190178210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9E"
      *                短手３（小児食物アレルギー負荷検査）（生活）
                       MOVE   "190178310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9F"
      *                短手３（前立腺針生検法）
                       MOVE   "190178410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9G"
      *                短手３（前立腺針生検法）（生活）
                       MOVE   "190178510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9J"
      *                短手３（関節鏡下手根管開放手術）
                       MOVE   "190178810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9K"
      *                短手３（関節鏡下手根管開放手術）（生活）
                       MOVE   "190178910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9L"
      *                短手３（胸腔鏡下交感神経節切除術（両側））
                       MOVE   "190179010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9M"
      *                短手３（胸腔鏡下交感神経節切除術（両側））（生活）
                       MOVE   "190179110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9N"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）
                       MOVE   "190179210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9O"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・片側）（生活）
                       MOVE   "190179310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9R"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
                       MOVE   "190179610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9S"
      *                短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
                       MOVE   "190179710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9T"
      *                短手３（下肢静脈瘤手術（抜去切除術））
                       MOVE   "190179810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9U"
      *                短手３（下肢静脈瘤手術（抜去切除術））（生活）
                       MOVE   "190179910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9V"
      *                短手３（下肢静脈瘤手術（硬化療法））
                       MOVE   "190180010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9W"
      *                短手３（下肢静脈瘤手術（硬化療法））（生活）
                       MOVE   "190180110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9X"
      *                短手３（下肢静脈瘤手術（高位結紮術））
                       MOVE   "190180210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Y"
      *                短手３（下肢静脈瘤手術（高位結紮術））（生活）
                       MOVE   "190180310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "9Z"
      *                短手３（鼠径ヘルニア手術（１５歳未満））
                       MOVE   "190180410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A0"
      *                短手３（鼠径ヘルニア手術（１５歳未満））（生活）
                       MOVE   "190180510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A1"
      *                短手３（鼠径ヘルニア手術（１５歳以上））
                       MOVE   "190180610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A2"
      *                短手３（鼠径ヘルニア手術（１５歳以上））（生活）
                       MOVE   "190180710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A5"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
                       MOVE   "190181010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A6"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
                       MOVE   "190181110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A7"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
                       MOVE   "190181210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A8"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
                       MOVE   "190181310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "A9"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
                       MOVE   "190181410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AA"
      *                短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
                       MOVE   "190181510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AB"
      *                短手３（痔核手術（硬化療法（四段階注射法））
                       MOVE   "190181610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AC"
      *                短手３（痔核手術（硬化療法（四段階注射法））（生活）
                       MOVE   "190181710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AD"
      *                短手３（子宮頚部（腟部）切除術）
                       MOVE   "190181810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AE"
      *                短手３（子宮頚部（腟部）切除術）（生活）
                       MOVE   "190181910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AF"
      *                短手３（子宮鏡下子宮筋腫摘出術）
                       MOVE   "190182010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AG"
      *                短手３（子宮鏡下子宮筋腫摘出術）（生活）
                       MOVE   "190182110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AH"
      *                特定集中治療室管理料１
                       MOVE   "190174410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AI"
      *                特定集中治療室管理料２（特定集中治療室管理料）
                       MOVE   "190174610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AJ"
      *                ハイケアユニット入院医療管理料１
                       MOVE   "190175010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AK"
      *                ハイケアユニット入院医療管理料２
                       MOVE   "190175110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AL"
      *                地域包括ケア入院医療管理料１
                       MOVE   "190209310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AM"
      *                地域包括ケア入院医療管理料１（生活療養）
                       MOVE   "190209410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AN"
      *                地域包括ケア入院医療管理料２
                       MOVE   "190209710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AO"
      *                地域包括ケア入院医療管理料２（生活療養）
                       MOVE   "190209810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AP"
      *                地域包括ケア入院医療管理料１（特定地域）
                       MOVE   "190210910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AQ"
      *                地域包括ケア入院医療管理料１（生活療養）（特定地域）
                       MOVE   "190211010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AR"
      *                地域包括ケア入院医療管理料２（特定地域）
                       MOVE   "190211310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AS"
      *                地域包括ケア入院医療管理料２（生活療養）（特定地域）
                       MOVE   "190211410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AT"
      *                特定集中治療室管理料２（広範囲熱傷特定集中治療室管理料）
                       MOVE   "190174810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AU"
      *                特定一般病棟入院料（地域包括ケア１）
                       MOVE   "190213310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AV"
      *                特定一般病棟入院料（地域包括ケア２）
                       MOVE   "190213410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AY"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）
                       MOVE   "190195910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "AZ"
      *                短手３（水晶体再建術・眼内レンズ挿入・その他・両側）（生活）
                       MOVE   "190196010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B2"
      *                短手３（経皮的シャント拡張術・血栓除去術）
                       MOVE   "190196310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B3"
      *                短手３（経皮的シャント拡張術・血栓除去術）（生活）
                       MOVE   "190196410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B4"
      *                短手３（鼠径ヘルニア手術（３歳未満））
                       MOVE   "190196510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B5"
      *                短手３（鼠径ヘルニア手術（３歳未満））（生活）
                       MOVE   "190196810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B6"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））
                       MOVE   "190196610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B7"
      *                短手３（鼠径ヘルニア手術（３歳以上６歳未満））（生活）
                       MOVE   "190196910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B8"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））
                       MOVE   "190196710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "B9"
      *                短手３（鼠径ヘルニア手術（６歳以上１５歳未満））（生活）
                       MOVE   "190197010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BA"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））
                       MOVE   "190197110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BB"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳未満））（生活）
                       MOVE   "190197410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BC"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））
                       MOVE   "190197210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BD"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（３歳以上６歳未満））（生活）
                       MOVE   "190197510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BE"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））
                       MOVE   "190197310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BF"
      *                短手３（腹腔鏡下鼠径ヘルニア手術）（６歳以上１５歳未満））（生活）
                       MOVE   "190197610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BG"
      *                短手３（体外衝撃波腎・尿管結石破砕術）
                       MOVE   "190197710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BH"
      *                短手３（体外衝撃波腎・尿管結石破砕術）（生活）
                       MOVE   "190197810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BI"
      *                短手３（ガンマナイフによる定位放射線治療）
                       MOVE   "190197910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BJ"
      *                短手３（ガンマナイフによる定位放射線治療）（生活）
                       MOVE   "190198010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BK"
      *                障害者施設等７対１入院基本料（医療区分２）
                       MOVE   "190189410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BL"
      *                障害者施設等７対１入院基本料（医療区分１）
                       MOVE   "190189510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BM"
      *                障害者施設等１０対１入院基本料（医療区分２）
                       MOVE   "190189610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BN"
      *                障害者施設等１０対１入院基本料（医療区分１）
                       MOVE   "190189710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BO"
      *                障害者施設等１３対１入院基本料（医療区分２）
                       MOVE   "190189810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BP"
      *                障害者施設等１３対１入院基本料（医療区分１）
                       MOVE   "190189910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BQ"
      *                障害者施設等１５対１入院基本料（医療区分２）
                       MOVE   "190190010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BR"
      *                障害者施設等１５対１入院基本料（医療区分１）
                       MOVE   "190190110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BT"
      *                特殊疾患入院医療管理料（医療区分２）
                       MOVE   "190194510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BU"
      *                特殊疾患入院医療管理料（医療区分１）
                       MOVE   "190194610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BV"
      *                療養病棟・急性期一般入院料１
                       MOVE   "190198110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BW"
      *                療養病棟・急性期一般入院料７
                       MOVE   "190198210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BX"
      *                療養病棟・地域一般入院料２
                       MOVE   "190198310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BY"
      *                療養病棟・地域一般入院料３
                       MOVE   "190198410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "BZ"
      *                地域包括ケア入院医療管理料３
                       MOVE   "190210110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C0"
      *                地域包括ケア入院医療管理料３（生活療養）
                       MOVE   "190210210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C1"
      *                地域包括ケア入院医療管理料４
                       MOVE   "190210510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C2"
      *                地域包括ケア入院医療管理料４（生活療養）
                       MOVE   "190210610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C3"
      *                地域包括ケア入院医療管理料３（特定地域）
                       MOVE   "190211710"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C4"
      *                地域包括ケア入院医療管理料３（生活療養）（特定地域）
                       MOVE   "190211810"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C5"
      *                地域包括ケア入院医療管理料４（特定地域）
                       MOVE   "190212110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C6"
      *                地域包括ケア入院医療管理料４（生活療養）（特定地域）
                       MOVE   "190212210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C7"
      *                療養病棟・急性期一般入院料２
                       MOVE   "190201910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C8"
      *                療養病棟・急性期一般入院料３
                       MOVE   "190202010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "C9"
      *                療養病棟・急性期一般入院料４
                       MOVE   "190202110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CA"
      *                療養病棟・急性期一般入院料５
                       MOVE   "190202210"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CB"
      *                療養病棟・急性期一般入院料６
                       MOVE   "190202310"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CC"
      *                療養病棟・地域一般入院料１
                       MOVE   "190202410"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CD"
      *                特定一般病棟入院料（地域包括ケア３）
                       MOVE   "190213510"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CE"
      *                特定一般病棟入院料（地域包括ケア４）
                       MOVE   "190213610"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CF"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））
                       MOVE   "190220910"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CG"
      *                短手３（経皮的シャント拡張術・血栓除去術（初回））（生活）
                       MOVE   "190221010"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CH"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）
                       MOVE   "190221110"   TO  LNK-TOKNYUIN-SRYCD
               WHEN    "CI"
      *                短手３（経皮的シャント拡張術・血栓除去術・３月以内）（生活）
                       MOVE   "190221210"   TO  LNK-TOKNYUIN-SRYCD
             END-EVALUATE
           END-IF
      *
      *    特定入院料の算定要件外コードセット
           IF    LNK-TOKNYUIN-SANTEIYOUKENKBN  =  "1"
      *      病棟
             IF  LNK-TOKNYUIN-KBN   =   "2"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "09"
               WHEN    "54"
      *            緩和ケア病棟入院料１
      *            緩和ケア病棟入院料２
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "10"
               WHEN    "22"
               WHEN    "14"
               WHEN    "11"
               WHEN    "12"
               WHEN    "23"
               WHEN    "53"
      *            精神科救急入院料１
      *            精神科救急入院料２
      *            小児入院医療管理料５
      *            精神科急性期治療病棟入院料１
      *            精神科急性期治療病棟入院料２
      *            精神科救急・合併症入院料
      *            地域移行機能強化病棟入院料
      *            精神病棟の時は精神病棟１５対１入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "02"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190083810"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
               WHEN    "31"
               WHEN    "27"
               WHEN    "29"
               WHEN    "55"
               WHEN    "41"
               WHEN    "45"
               WHEN    "43"
               WHEN    "47"
      *            回復期リハビリテーション病棟入院料１
      *            回復期リハビリテーション病棟入院料２
      *            回復期リハビリテーション病棟入院料３
      *            回復期リハビリテーション病棟入院料４
      *            地域包括ケア病棟入院料１
      *            地域包括ケア病棟入院料１（特定地域）
      *            地域包括ケア病棟入院料２
      *            地域包括ケア病棟入院料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "32"
               WHEN    "28"
               WHEN    "30"
               WHEN    "56"
               WHEN    "42"
               WHEN    "46"
               WHEN    "44"
               WHEN    "48"
      *            回復期リハビリテーション病棟入院料１（生活療養）
      *            回復期リハビリテーション病棟入院料２（生活療養）
      *            回復期リハビリテーション病棟入院料３（生活療養）
      *            回復期リハビリテーション病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）
      *            地域包括ケア病棟入院料１（生活療養）（特定地域）
      *            地域包括ケア病棟入院料２（生活療養）
      *            地域包括ケア病棟入院料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "57"
               WHEN    "59"
               WHEN    "61"
               WHEN    "65"
               WHEN    "63"
               WHEN    "67"
      *            回復期リハビリテーション病棟入院料５
      *            回復期リハビリテーション病棟入院料６
      *            地域包括ケア病棟入院料３
      *            地域包括ケア病棟入院料３（特定地域）
      *            地域包括ケア病棟入院料４
      *            地域包括ケア病棟入院料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "58"
               WHEN    "60"
               WHEN    "62"
               WHEN    "66"
               WHEN    "64"
               WHEN    "68"
      *            回復期リハビリテーション病棟入院料５（生活療養）
      *            回復期リハビリテーション病棟入院料６（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）
      *            地域包括ケア病棟入院料３（生活療養）（特定地域）
      *            地域包括ケア病棟入院料４（生活療養）
      *            地域包括ケア病棟入院料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               END-EVALUATE
             END-IF
      *
      *      病室
             IF  LNK-TOKNYUIN-KBN   =   "3"
               EVALUATE    LNK-TOKNYUIN-NYUIN
               WHEN    "94"
      *            児童・思春期精神科入院医療管理料
      *            精神病棟特別入院基本料を算定
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190085010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               WHEN    "AL"
               WHEN    "AP"
               WHEN    "AN"
               WHEN    "AR"
      *            地域包括ケア入院医療管理料１
      *            地域包括ケア入院医療管理料１（特定地域）
      *            地域包括ケア入院医療管理料２
      *            地域包括ケア入院医療管理料２（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料１（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132210"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AM"
               WHEN    "AQ"
               WHEN    "AO"
               WHEN    "AS"
      *            地域包括ケア入院医療管理料１（生活療養）
      *            地域包括ケア入院医療管理料１（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料２（生活療養）
      *            地域包括ケア入院医療管理料２（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料１（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190132310"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "BZ"
               WHEN    "C3"
               WHEN    "C1"
               WHEN    "C5"
      *            地域包括ケア入院医療管理料３
      *            地域包括ケア入院医療管理料３（特定地域）
      *            地域包括ケア入院医療管理料４
      *            地域包括ケア入院医療管理料４（特定地域）
      *            一般病棟の時は一般病棟特別入院基本料を算定
                   IF   SYS-5001-BTU-SBT     =   "01"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                   END-IF
      *            療養病棟の時は療養病棟入院料２（Ｉ）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134010"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "C0"
               WHEN    "C4"
               WHEN    "C2"
               WHEN    "C6"
      *            地域包括ケア入院医療管理料３（生活療養）
      *            地域包括ケア入院医療管理料３（生活療養）（特定地域）
      *            地域包括ケア入院医療管理料４（生活療養）
      *            地域包括ケア入院医療管理料４（生活療養）（特定地域）
      *            療養病棟の時は療養病棟入院料２（Ｉ）（生活療養）を算定
                   IF   SYS-5001-BTU-SBT     =   "04"
                        MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                        MOVE   "190134110"  TO  LNK-TOKNYUIN-GAI-SRYCD
                        MOVE   "1"          TO
                                            LNK-TOKNYUIN-GAI-NYUKSNKBN
                   END-IF
               WHEN    "AU"
               WHEN    "AV"
               WHEN    "CD"
               WHEN    "CE"
      *            特定一般病棟入院料（地域包括ケア１）
      *            特定一般病棟入院料（地域包括ケア２）
      *            特定一般病棟入院料（地域包括ケア３）
      *            特定一般病棟入院料（地域包括ケア４）
      *            一般病棟特別入院基本料を算定
                   MOVE   SPACE        TO  LNK-TOKNYUIN-SRYCD
                   MOVE   "190079010"  TO  LNK-TOKNYUIN-GAI-SRYCD
               END-EVALUATE
             END-IF
           END-IF
      *
           IF   (LNK-TOKNYUIN-SRYCD        =     SPACE)   AND
                (LNK-TOKNYUIN-GAI-SRYCD    =     SPACE)
               MOVE    1              TO     LNK-TOKNYUIN-RC
           END-IF
      *
           .
       206-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
