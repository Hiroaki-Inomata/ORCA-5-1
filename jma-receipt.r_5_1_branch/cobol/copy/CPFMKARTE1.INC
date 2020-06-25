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
        01  FMKARTE1-REC.
            03  FMKARTE1-PCONTEXT               PIC S9(9)   BINARY.
            03  FMKARTE1-OBJECT                 PIC X(36).
            03  FMKARTE1-MODE                   PIC S9(9)   BINARY.
            03  FMKARTE1-RECORDNAME             PIC X(128).
            03  FMKARTE1-DATA.
      *       karte_no1を設定
              05  FMKARTE1-FORM-ID            PIC X(30).
      *       カスタムＩＤ
              05  FMKARTE1-CUSTOM-ID            PIC X(30).
      *       SYS-1031-PRTNM(1)
              05  FMKARTE1-PRINTER              PIC X(20).
              05  FMKARTE1-HAKKOKBN             PIC X(1).
              05  FMKARTE1-PTINF.
                07  FMKARTE1-PTNUM              PIC X(20).
                07  FMKARTE1-NAME               PIC X(50).
                07  FMKARTE1-KANANAME           PIC X(50).
                07  FMKARTE1-BIRTHDAY           PIC X(10).
                07  FMKARTE1-SEX                PIC X(1).
                07  FMKARTE1-TABOO              PIC X(100)  OCCURS  2.
                07  FMKARTE1-ALLERGY            PIC X(100)  OCCURS  2.
                07  FMKARTE1-KANSENSYO          PIC X(100)  OCCURS  2.
                07  FMKARTE1-COMMENT            PIC X(100)  OCCURS  2.
      *         状態名称１～３
                07  FMKARTE1-CONDITION          PIC X(20)   OCCURS  3.
      *         PTINF-CREYMD
                07  FMKARTE1-CREYMD             PIC X(10).
                07  FMKARTE1-JOB                PIC X(20).
                07  FMKARTE1-SETAINUSI          PIC X(50).
                07  FMKARTE1-ZOKUGARA           PIC X(30).
                07  FMKARTE1-HOME-ADDRESS-G.
      *           HOME-ADRS(1):PTINF-HOME-ADRS / (2):PTINF-HOME-BANTI
                  09  FMKARTE1-HOME-ADRS        PIC X(100)  OCCURS  2.
                  09  FMKARTE1-HOME-POST        PIC X(7).
                  09  FMKARTE1-HOME-TEL1        PIC X(15).
                  09  FMKARTE1-HOME-TEL2        PIC X(15).
                07  FMKARTE1-RENRAKU-G.
                  09  FMKARTE1-RENRAKU-NAME     PIC X(50).
                  09  FMKARTE1-RENRAKU-ZOKUGARA PIC X(30).
                  09  FMKARTE1-RENRAKU-ADRS     PIC X(100)  OCCURS  2.
                  09  FMKARTE1-RENRAKU-POST     PIC X(7).
                  09  FMKARTE1-RENRAKU-TEL1     PIC X(15).
                  09  FMKARTE1-RENRAKU-TEL2     PIC X(15).
                07  FMKARTE1-OFFICE-G.
                  09  FMKARTE1-OFFICE-NAME      PIC X(50).
                  09  FMKARTE1-OFFICE-ADRS      PIC X(100)  OCCURS  2.
                  09  FMKARTE1-OFFICE-POST      PIC X(7).
                  09  FMKARTE1-OFFICE-TEL       PIC X(15).
                07  FMKARTE1-KISEI-G.
                  09  FMKARTE1-KISEI-NAME       PIC X(50).
                  09  FMKARTE1-KISEI-ADRS       PIC X(100)  OCCURS  2.
                  09  FMKARTE1-KISEI-POST       PIC X(7).
                  09  FMKARTE1-KISEI-TEL        PIC X(15).
              05  FMKARTE1-HKNCOMBI-G.
                07  FMKARTE1-HKNCOMBI           PIC X(4).
                07  FMKARTE1-GAI-KFTNRATE       PIC X(3).
                07  FMKARTE1-NYU-KFTNRATE       PIC X(3).
                07  FMKARTE1-HKNNUM             PIC X(3).
      *             保険の種類名称
                07  FMKARTE1-HKNNAME            PIC X(20).
      *         保険年齢
                07  FMKARTE1-HOKEN-AGE          PIC X(3).
                07  FMKARTE1-HOKEN-RATE         PIC X(3).
                07  FMKARTE1-HOKEN-G.
                  09  FMKARTE1-HKNJANAME        PIC X(100).
                  09  FMKARTE1-HKNJANUM         PIC X(8).
                  09  FMKARTE1-HKNJA-ADRS       PIC X(100)  OCCURS  2.
                  09  FMKARTE1-HKNJA-TEL        PIC X(15).
                  09  FMKARTE1-KIGO             PIC X(80).
                  09  FMKARTE1-NUM              PIC X(80).
      *           枝番
                  09  FMKARTE1-EDABAN           PIC X(02).
                  09  FMKARTE1-CONTKBN          PIC X(1).
                  09  FMKARTE1-HOJOKBN          PIC X(1).
                  09  FMKARTE1-HONKZKKBN        PIC X(1).
                  09  FMKARTE1-HIHKNJANAME      PIC X(100).
                  09  FMKARTE1-TEKSTYMD         PIC X(10).
                  09  FMKARTE1-TEKEDYMD         PIC X(10).
                  09  FMKARTE1-SKKGETYMD        PIC X(10).
                07  FMKARTE1-KOHI-G              OCCURS  4.
                  09  FMKARTE1-KOH-HKNNUM        PIC X(3).
                  09  FMKARTE1-KOH-TANSEIDONAME  PIC X(20).
                  09  FMKARTE1-KOH-FTNJANUM      PIC X(8).
                  09  FMKARTE1-KOH-JKYSNUM       PIC X(20).
                  09  FMKARTE1-KOH-TEKSTYMD      PIC X(10).
                  09  FMKARTE1-KOH-TEKEDYMD      PIC X(10).
                07  FMKARTE1-ROSAI-G.
                  09  FMKARTE1-RSI-HKNKBN        PIC X(1).
                  09  FMKARTE1-RSI-KOFUNUM       PIC X(14).
                  09  FMKARTE1-RSI-RYOSTYMD      PIC X(10).
                  09  FMKARTE1-RSI-RYOEDYMD      PIC X(10).
                  09  FMKARTE1-RSI-SHOBYOYMD     PIC X(10).
                  09  FMKARTE1-RSI-JIGYOUNAME    PIC X(50).
      *           東京都、北海道等
                  09  FMKARTE1-PREFNAME          PIC X(10).
      *           千代田区、札幌市等
                  09  FMKARTE1-CITYNAME          PIC X(50).
              05  FMKARTE1-HOSP-G.
                07  FMKARTE1-HOSPNAME            PIC X(60).
              05  FMKARTE1-BYOMEI-G              OCCURS  1000.
      *             カルテに記載する（編集後の）病名
                07  FMKARTE1-BYOMEI              PIC X(160).
      *             PTBYO-SYUBYOFLG
                07  FMKARTE1-SYUBYOFLG           PIC X(1).
      *             PTBYO-UTAGAIFLG
                07  FMKARTE1-UTAGAIFLG           PIC X(1).
                07  FMKARTE1-SRYYMD              PIC X(10).
                07  FMKARTE1-TENKIYMD            PIC X(10).
      *             PTBYO-TENKIKBN
                07  FMKARTE1-TENKIKBN            PIC X(1).
      *           備考
              05  FMKARTE1-MEMO                  PIC X(100)
                                                 OCCURS  5.
