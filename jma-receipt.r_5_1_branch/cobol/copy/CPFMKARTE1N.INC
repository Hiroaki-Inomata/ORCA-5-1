      *******************************************************************
      * Project code name "ORCA"
      * ì˙à„ïWèÄÉåÉZÉvÉgÉ\ÉtÉgÅiJMA standard receipt softwareÅj
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
        01  FMKARTE1N-REC.
            03  FMKARTE1N-PCONTEXT               PIC S9(9)   BINARY.
            03  FMKARTE1N-OBJECT                 PIC X(36).
            03  FMKARTE1N-MODE                   PIC S9(9)   BINARY.
            03  FMKARTE1N-RECORDNAME             PIC X(128).
            03  FMKARTE1N-DATA.
      *       karte_no1Çê›íË
              05  FMKARTE1N-FORM-ID            PIC X(30).
      *       ÉJÉXÉ^ÉÄÇhÇc
              05  FMKARTE1N-CUSTOM-ID            PIC X(30).
      *       SYS-1031-PRTNM(1)
              05  FMKARTE1N-PRINTER              PIC X(20).
              05  FMKARTE1N-HAKKOKBN             PIC X(1).
      *       ì¸â@ì˙
              05  FMKARTE1N-NYUINYMD             PIC X(10).
      *       ëﬁâ@ì˙
              05  FMKARTE1N-TAIINYMD             PIC X(10).
              05  FMKARTE1N-PTINF.
                07  FMKARTE1N-PTNUM              PIC X(20).
                07  FMKARTE1N-NAME               PIC X(50).
                07  FMKARTE1N-KANANAME           PIC X(50).
                07  FMKARTE1N-BIRTHDAY           PIC X(10).
                07  FMKARTE1N-SEX                PIC X(1).
                07  FMKARTE1N-TABOO              PIC X(100)  OCCURS  2.
                07  FMKARTE1N-ALLERGY            PIC X(100)  OCCURS  2.
                07  FMKARTE1N-KANSENSYO          PIC X(100)  OCCURS  2.
                07  FMKARTE1N-COMMENT            PIC X(100)  OCCURS  2.
      *         èÛë‘ñºèÃÇPÅ`ÇR
                07  FMKARTE1N-CONDITION          PIC X(20)   OCCURS  3.
      *         PTINF-CREYMD
                07  FMKARTE1N-CREYMD             PIC X(10).
                07  FMKARTE1N-JOB                PIC X(20).
                07  FMKARTE1N-SETAINUSI          PIC X(50).
                07  FMKARTE1N-ZOKUGARA           PIC X(30).
                07  FMKARTE1N-HOME-ADDRESS-G.
      *           HOME-ADRS(1):PTINF-HOME-ADRS / (2):PTINF-HOME-BANTI
                  09  FMKARTE1N-HOME-ADRS        PIC X(100)  OCCURS  2.
                  09  FMKARTE1N-HOME-POST        PIC X(7).
                  09  FMKARTE1N-HOME-TEL1        PIC X(15).
                  09  FMKARTE1N-HOME-TEL2        PIC X(15).
                07  FMKARTE1N-RENRAKU-G.
                  09  FMKARTE1N-RENRAKU-NAME     PIC X(50).
                  09  FMKARTE1N-RENRAKU-ZOKUGARA PIC X(30).
                  09  FMKARTE1N-RENRAKU-ADRS     PIC X(100)  OCCURS  2.
                  09  FMKARTE1N-RENRAKU-POST     PIC X(7).
                  09  FMKARTE1N-RENRAKU-TEL1     PIC X(15).
                  09  FMKARTE1N-RENRAKU-TEL2     PIC X(15).
                07  FMKARTE1N-OFFICE-G.
                  09  FMKARTE1N-OFFICE-NAME      PIC X(50).
                  09  FMKARTE1N-OFFICE-ADRS      PIC X(100)  OCCURS  2.
                  09  FMKARTE1N-OFFICE-POST      PIC X(7).
                  09  FMKARTE1N-OFFICE-TEL       PIC X(15).
                07  FMKARTE1N-KISEI-G.
                  09  FMKARTE1N-KISEI-NAME       PIC X(50).
                  09  FMKARTE1N-KISEI-ADRS       PIC X(100)  OCCURS  2.
                  09  FMKARTE1N-KISEI-POST       PIC X(7).
                  09  FMKARTE1N-KISEI-TEL        PIC X(15).
              05  FMKARTE1N-HKNCOMBI-G.
                07  FMKARTE1N-HKNCOMBI           PIC X(4).
                07  FMKARTE1N-GAI-KFTNRATE       PIC X(3).
                07  FMKARTE1N-NYU-KFTNRATE       PIC X(3).
                07  FMKARTE1N-HKNNUM             PIC X(3).
      *             ï€åØÇÃéÌóﬁñºèÃ
                07  FMKARTE1N-HKNNAME            PIC X(20).
      *         ï€åØîNóÓ
                07  FMKARTE1N-HOKEN-AGE          PIC X(3).
                07  FMKARTE1N-HOKEN-RATE         PIC X(3).
                07  FMKARTE1N-HOKEN-G.
                  09  FMKARTE1N-HKNJANAME        PIC X(100).
                  09  FMKARTE1N-HKNJANUM         PIC X(8).
                  09  FMKARTE1N-HKNJA-ADRS       PIC X(100)  OCCURS  2.
                  09  FMKARTE1N-HKNJA-TEL        PIC X(15).
                  09  FMKARTE1N-KIGO             PIC X(80).
                  09  FMKARTE1N-NUM              PIC X(80).
      *           é}î‘
                  09  FMKARTE1N-EDABAN           PIC X(02).
                  09  FMKARTE1N-CONTKBN          PIC X(1).
                  09  FMKARTE1N-HOJOKBN          PIC X(1).
                  09  FMKARTE1N-HONKZKKBN        PIC X(1).
                  09  FMKARTE1N-HIHKNJANAME      PIC X(100).
                  09  FMKARTE1N-TEKSTYMD         PIC X(10).
                  09  FMKARTE1N-TEKEDYMD         PIC X(10).
                  09  FMKARTE1N-SKKGETYMD        PIC X(10).
                07  FMKARTE1N-KOHI-G              OCCURS  4.
                  09  FMKARTE1N-KOH-HKNNUM        PIC X(3).
                  09  FMKARTE1N-KOH-TANSEIDONAME  PIC X(20).
                  09  FMKARTE1N-KOH-FTNJANUM      PIC X(8).
                  09  FMKARTE1N-KOH-JKYSNUM       PIC X(20).
                  09  FMKARTE1N-KOH-TEKSTYMD      PIC X(10).
                  09  FMKARTE1N-KOH-TEKEDYMD      PIC X(10).
                07  FMKARTE1N-ROSAI-G.
                  09  FMKARTE1N-RSI-HKNKBN        PIC X(1).
                  09  FMKARTE1N-RSI-KOFUNUM       PIC X(14).
                  09  FMKARTE1N-RSI-RYOSTYMD      PIC X(10).
                  09  FMKARTE1N-RSI-RYOEDYMD      PIC X(10).
                  09  FMKARTE1N-RSI-SHOBYOYMD     PIC X(10).
                  09  FMKARTE1N-RSI-JIGYOUNAME    PIC X(50).
      *           ìåãûìsÅAñkäCìπìô
                  09  FMKARTE1N-PREFNAME          PIC X(10).
      *           êÁë„ìcãÊÅAéDñyésìô
                  09  FMKARTE1N-CITYNAME          PIC X(50).
              05  FMKARTE1N-HOSP-G.
                07  FMKARTE1N-HOSPNAME            PIC X(60).
              05  FMKARTE1N-DOCTOR-AREA           OCCURS  3.
                07  FMKARTE1N-DR-CODE             PIC X(5).
                07  FMKARTE1N-DR-NAME             PIC X(50).
                07  FMKARTE1N-DR-KANANAME         PIC X(50).
              05  FMKARTE1N-SRYKA                 PIC X(2).
              05  FMKARTE1N-SRYKANAME             PIC X(10).
              05  FMKARTE1N-BYOMEI-G              OCCURS  1000.
      *             ÉJÉãÉeÇ…ãLç⁄Ç∑ÇÈÅiï“èWå„ÇÃÅjïañº
                07  FMKARTE1N-BYOMEI              PIC X(160).
      *             PTBYO-SYUBYOFLG
                07  FMKARTE1N-SYUBYOFLG           PIC X(1).
      *             PTBYO-UTAGAIFLG
                07  FMKARTE1N-UTAGAIFLG           PIC X(1).
                07  FMKARTE1N-SRYYMD              PIC X(10).
                07  FMKARTE1N-TENKIYMD            PIC X(10).
      *             PTBYO-TENKIKBN
                07  FMKARTE1N-TENKIKBN            PIC X(1).
