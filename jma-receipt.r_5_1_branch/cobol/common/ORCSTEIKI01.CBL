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
       PROGRAM-ID.                     ORCSTEIKI01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : ������᥵�֣���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/01/23    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/05/30  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX01                   PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-JOBID               PIC 9(07).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSTEIKI01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           ORCSTEIKI01AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
      *    ��λ����
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       STEIKI01-OT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( STEIKI01-JOBID      >   1 )
               PERFORM 2001-CHK-PREJOB-SEC
           END-IF
      *
           IF    ( STEIKI01-STOP       =   ZERO )
               PERFORM 2001-CHK-SELFJOB-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������֥����å�����
      *****************************************************************
       2001-CHK-PREJOB-SEC             SECTION.
      *
           COMPUTE WRK-JOBID   =   STEIKI01-JOBID  -   1
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
            AND  ( JOB-STOPFLG         =   2    )
               MOVE    1                   TO  STEIKI01-STOP
           END-IF
      *
           .
       2001-CHK-PREJOB-EXT.
           EXIT.
      *****************************************************************
      *    ������֥����å�����
      *****************************************************************
       2001-CHK-SELFJOB-SEC             SECTION.
      *
           MOVE    STEIKI01-JOBID          TO  WRK-JOBID
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
            AND  ( JOB-STOPFLG         =   2    )
               MOVE    1                   TO  STEIKI01-STOP
           END-IF
      *
           .
       2001-CHK-PREJOB-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ�������
      *****************************************************************
       800-CHK-ORCSJOB-SEC                 SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-JOBID           TO  JOB-JOBID
           MOVE    STEIKI01-SHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
      *
       800-CHK-ORCSJOB-EXT.
           EXIT.