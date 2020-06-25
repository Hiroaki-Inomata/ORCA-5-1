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
       PROGRAM-ID.             ORCBD002.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����Ģɼ
      *  ����ݡ��ͥ��̾  : ����ɽ�ʣȣã�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-ƣ��      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01.10.03  ������ץ��̾�Խ�
      *  01.00.02    MCC-ƣ��     01.10.24  ������ץ��̾������
      *                                     �Ǥ��ʤ��Ȥ����顼�ˤ��ʤ�
      *  01.00.03    MCC-ƣ��     02.03.18  ü���ɣ�
      *                                     X(16)����X(32)���ѹ�
      *  01.00.04    MCC-ƣ��     02.03/22  �ѥ�᥿�˥����ƥ����դ��ɲ�
      *  01.00.05    MCC-ƣ��     02/05/13  ���������������ģ¤��ɲ�
      *  01.00.06    NACL-ƣ��    02/07/24  ϫ�ҡ������ա�����ι����ɲ�
      *  01.00.07    NACL-ƣ��    02/08/27  �Կ��򣳣�����ѹ�
      *                                    ��Ģɼ�ɣĤ�ȣã����֣������ѹ���
      *  01.01.01    NACL-ƣ��    02/10/10  �����ȶ�ʬ�ɲ�
      *  01.01.02    NACL-ƣ��    02/10/17  ����ν�����ˡ����
      *  02.00.01    NACL-����    04/01/06  �ޥ��ʥ����θ
      *  02.00.02    NACL-����    05/01/12  �����󽸷׽�����ü��������
      *  02.00.03    NACL-����    06/07/08  �Խ���ORCBD008�ǹԤ��褦����
      *  03.05.00    NACL-����    07/05/28  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  RECEERR                             PIC X(100).
      *
       01  WRK-PARA-ATAI.
           03  WRK-PARA-SYORIYMD   PIC X(08).
           03  WRK-PARA-SORTKBN    PIC X(01).
      *
      *    �ᥤ��У��ѥѥ�᥿
       COPY    "CPORCBD008.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
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
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
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
                                       RECEERR
                                       WRK-PARA-SYORIYMD
                                       WRK-PARA-SORTKBN
           END-UNSTRING
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
           INITIALIZE                  ORCBD008AREA
           MOVE    "3"             TO  ORCBD008-SYORIKBN
           MOVE    WRK-PARA-SYORIYMD
                                   TO  ORCBD008-SYORIYMD
      *
           MOVE    "0"             TO  ORCBD008-NYUGAIKBN
           MOVE    WRK-PARA-SORTKBN TO ORCBD008-SORTKBN
      *
           CALL    "ORCBD008"          USING
                                       WRK-PARA
                                       ORCBD008AREA
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
           .
       300-END-EXT.
           EXIT.
      *