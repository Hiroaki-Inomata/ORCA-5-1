      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCDTCHK000.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ǡ��������å�
      *  ����ݡ��ͥ��̾  : �ƽ����ƽ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  05/03/31    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/06/04  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    �ѥ�᡼���ե�����
           SELECT  DTCHKPRM-FILE   ASSIGN          ASS-DTCHKPRM
                                   ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �ѥ�᡼���ե�����
       FD  DTCHKPRM-FILE.
           COPY    "CPORCBSD01PARA.INC"
                       REPLACING  //ORCBSD01PARA// BY //DTCHKPRM-REC//
                                  //ORCBSD01-//    BY //DTCHKPRM-//.
      *
       WORKING-STORAGE             SECTION.
      *
       01  DTCHK-RC                    PIC S9(09).
      *
      *    �ѥ�᡼���ե�����̾
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //ASS-DTCHKPRM//.
      *
      *    ���顼�ե�����ե�����̾���ΰ� 
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //DTCHKERRPARA//.
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-BLOBID     PIC X(36).
      *
      *    �ǡ��������å��������ѥ�᡼��
           COPY    "CPORCBSD01PARA.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ǡ��������å����Ų��
       01  DCACCT-REC.
           COPY    "CPDCACCT.INC".
      *
      *    �ǡ��������å����Ź԰�
       01  DCSRY-REC.
           COPY    "CPDCSRY.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(2000).
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
           DISPLAY "ORCDTCHK000 end"
      *
           EXIT PROGRAM
      *    STOP    RUN
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  SPA-AREA
                                       ORCBSD01PARA
      *
           MOVE       SPACE        TO  ASS-DTCHKPRM
           INITIALIZE                  ASS-DTCHKPRM
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               DTCHKERRPARA
                                               WRK-PARA-BLOBID
                                               ASS-DTCHKPRM-FILE-ID
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT = "RECEPTX"
                   PERFORM 100-DBOPEN-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  ASS-DTCHKPRM-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID TO  ASS-DTCHKPRM-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASS-DTCHKPRM        TO  SGETTEMP-BASENAMES (1)
           MOVE    DTCHKERRPARA        TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-DTCHKPRM
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  DTCHKERRPARA
      *
           OPEN    INPUT                   DTCHKPRM-FILE
           READ    DTCHKPRM-FILE
           END-READ
           MOVE    DTCHKPRM-REC        TO  ORCBSD01PARA
           CLOSE   DTCHKPRM-FILE
      *
      D    DISPLAY "LNK-PRTKANRI-RENNUM[" LNK-PRTKANRI-RENNUM "]"
      D    DISPLAY "LNK-PRTKANRI-TBL-KEY[" LNK-PRTKANRI-TBL-KEY "]"
      D    DISPLAY "LNK-PRTKANRI-TBL-GROUP[" 
      D                                 LNK-PRTKANRI-TBL-GROUP "]"
      D    DISPLAY "LNK-PRTKANRI-SHORI-RENNUM["
      D                                 LNK-PRTKANRI-SHORI-RENNUM "]"
      D    DISPLAY "LNK-PRTKANRI-SRYYM[" LNK-PRTKANRI-SRYYM "]"
      D    DISPLAY "LNK-PRTKANRI-SKYYMD[" LNK-PRTKANRI-SKYYMD "]"
      D    DISPLAY "LNK-PRTKANRI-SHELLID[" LNK-PRTKANRI-SHELLID "]"
      D    DISPLAY "LNK-PRTKANRI-PRIORITY[" LNK-PRTKANRI-PRIORITY "]"
      D    DISPLAY "LNK-PRTKANRI-TERMID[" LNK-PRTKANRI-TERMID "]"
      D    DISPLAY "LNK-PRTKANRI-OPID[" LNK-PRTKANRI-OPID "]"
      D    DISPLAY "LNK-PRTKANRI-PRTNM[" LNK-PRTKANRI-PRTNM "]"
      D    DISPLAY "WRK-PARA-JOBID[" WRK-PARA-JOBID "]"
      D    DISPLAY "WRK-PARA-SHELLID[" WRK-PARA-SHELLID "]"
      D    DISPLAY "WRK-PARA-HOSPNUM[" WRK-PARA-HOSPNUM "]"
      D    DISPLAY "DTCHKERRPARA[" DTCHKERRPARA "]"
      D    DISPLAY "WRK-PARA-BLOBID[" WRK-PARA-BLOBID "]"
      D    DISPLAY "ASS-DTCHKPRM-FILE-ID[" ASS-DTCHKPRM-FILE-ID "]"
      D    DISPLAY "ORCBSD01-IKTKBT[" ORCBSD01-IKTKBT "]"
      D    DISPLAY "ORCBSD01-SRYYM[" ORCBSD01-SRYYM "]"
      D    DISPLAY "ORCBSD01-NYUGAIKBN[" ORCBSD01-NYUGAIKBN "]"
      D    DISPLAY "ORCBSD01-CHKKBN-G[" ORCBSD01-CHKKBN-G "]"
      D    DISPLAY "ORCBSD01-SYAHOPROCKBN[" ORCBSD01-SYAHOPROCKBN "]"
      D    DISPLAY "ORCBSD01-KOKUHOPROCKBN[" ORCBSD01-KOKUHOPROCKBN "]"
      D    DISPLAY "ORCBSD01-RSIPROCKBN[" ORCBSD01-RSIPROCKBN "]"
      D    DISPLAY "ORCBSD01-JBIPROCKBN[" ORCBSD01-JBIPROCKBN "]"
      D    DISPLAY "ORCBSD01-JIHIPROCKBN[" ORCBSD01-JIHIPROCKBN "]"
      D    DISPLAY "ORCBSD01-INGAIPROCKBN[" ORCBSD01-INGAIPROCKBN "]"
      D    DISPLAY "ORCBSD01-PRTSRTKBN[" ORCBSD01-PRTSRTKBN "]"
      D    DISPLAY "ORCBSD01-PRTPTLSTKBN[" ORCBSD01-PRTPTLSTKBN "]"
      D    DISPLAY "ORCBSD01-SRYYMDKBN[" ORCBSD01-SRYYMDKBN "]"
      D    DISPLAY "ORCBSD01-STSRYYMD[" ORCBSD01-STSRYYMD "]"
      D    DISPLAY "ORCBSD01-EDSRYYMD[" ORCBSD01-EDSRYYMD "]"
      D    DISPLAY "ORCBSD01-KGIPROCKBN[" ORCBSD01-KGIPROCKBN "]"
      D    DISPLAY "ORCBSD01-KOUKIPROCKBN[" ORCBSD01-KOUKIPROCKBN "]"
      D    DISPLAY "ORCBSD01-CHIKENPROCKBN[" ORCBSD01-CHIKENPROCKBN "]"
      D    DISPLAY "ORCBSD01-DAISANPROCKBN[" ORCBSD01-DAISANPROCKBN "]"
      D    DISPLAY "ORCBSD01-FROM-API[" ORCBSD01-FROM-API "]"
      D    DISPLAY "ORCBSD01-KARTE-UID[" ORCBSD01-KARTE-UID "]"
      D    DISPLAY "ORCBSD01-ORCA-UID[" ORCBSD01-ORCA-UID "]"
      D    DISPLAY "ORCBSD01-DATA-ID[" ORCBSD01-DATA-ID "]"
      D    DISPLAY "ORCBSD01-PRINT-ID[" ORCBSD01-PRINT-ID "]"
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO                TO  DTCHK-RC
      *
           IF    ( DTCHK-RC            =   ZERO )
               IF    ( ORCBSD01-IKTKBT     =   "2" )
                OR   ( ORCBSD01-SRYYMDKBN  =   "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK009"
                   CALL    "ORCDTCHK009"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK009"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK004"
                   CALL    "ORCDTCHK004"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK004"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK005"
                   CALL    "ORCDTCHK005"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK005"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
      *        ������פϸ��������������ǽ�ʹ��ܤΤ�
      *        ��Ͽ���Ƥ���١������������˸¤�¹Ԥ��롣
               IF    ( ORCBSD01-NYUGAIKBN  =       "1" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK006"
                   CALL    "ORCDTCHK006"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK006"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK001"
                   CALL    "ORCDTCHK001"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK001"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK011"
                   CALL    "ORCDTCHK011"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK011"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK002"
                   CALL    "ORCDTCHK002"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK002"
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
               IF    ( LNK-PRTKANRI-TBL-KEY    =  "RECEPTX" )
                OR   ( ORCBSD01-FROM-API       =  "1" )
                   CONTINUE
               ELSE
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK010"
                   CALL    "ORCDTCHK010"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK001"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK008"
                   CALL    "ORCDTCHK008"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK008"
           END-IF
      *
           IF      LNK-PRTKANRI-TBL-KEY  = "RECEPTX"
             PERFORM 900-DBDISCONNECT-SEC
             PERFORM 100-DBOPEN-SEC
             PERFORM 100-DBSTART-SEC
           END-IF
           IF    ( DTCHK-RC            =   ZERO )
               IF  ( ORCBSD01-PRTPTLSTKBN  =   "2" )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK007"
                   CALL    "ORCDTCHK007"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK007"
               END-IF
           END-IF
      *
           IF    ( DTCHK-RC            =   ZERO )
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ST:ORCDTCHK003"
                   CALL    "ORCDTCHK003"       USING
                                               DTCHK-RC
                                               WRK-PARA
                                               DTCHKERRPARA
                                               ORCBSD01PARA
                   PERFORM 800-MCNDATE-SEC
                   DISPLAY SMCNDATE-HMS "ED:ORCDTCHK003"
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      LNK-PRTKANRI-TBL-KEY  NOT = "RECEPTX"
           PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡��������Ƚ���
      *****************************************************************
       100-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.