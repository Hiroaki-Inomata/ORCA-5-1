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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBD010.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����Ģɼ
      *  ����ݡ��ͥ��̾  : ��Ǽ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/01/23    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/06/04  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCBD010S.INC".
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
      ****************************************************************
       PROCEDURE               DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              SBD010-AREA
                                   SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
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
                                       SBD010-JOBID
                                       SBD010-SHELLID
                                       SBD010-HOSPNUM
                                       SBD010-ERRFLPARA
                                       SBD010-SRYYMD
                                       SBD010-EDSRYYMD
                                       SBD010-NYUGAIKBN
                                       SBD010-SUMKBN
                                       SBD010-MEISAIKBN
                                       SBD010-TOTALPRTKBN
           END-UNSTRING
      *
           MOVE    "0"             TO  SBD010-SIMEKBN
      *
           MOVE    SBD010-HOSPNUM  TO  SPA-HOSPNUM
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           CALL    "ORCBD010S"     USING   WRK-PARA
                                           SBD010-AREA
                                           SPA-AREA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *