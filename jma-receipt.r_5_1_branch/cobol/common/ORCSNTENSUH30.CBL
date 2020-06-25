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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSNTENSUH30.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �����������ޥ����ղþ����ʿ������ǯ�����
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  18/03/03    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ʿ������ǯ�ٲ������
           COPY    "CPORCSNTENSUH30.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSNTENSU.INC".
           COPY    "CPTENSU.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SNTENSU-AREA
           TNS-TENSU-REC
           SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  SNTENSU-RETURN-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  SNTENSU-IDX
      *
           PERFORM VARYING SNTENSU-IDX FROM    1   BY  1
                   UNTIL ( SNTENSU-IDX         >   SNTENSUH30-MAX )
                    OR   ( SNTENSU-KBNNUM  NOT =   ZERO )
      *
               IF    ( SNTENSU-SRYCD = SNTENSUH30-SRYCD (SNTENSU-IDX))
                   MOVE    SNTENSUH30-KBNNUM (SNTENSU-IDX)
                                   TO  SNTENSU-KBNNUM
                   MOVE    SNTENSUH30-EDANUM (SNTENSU-IDX)
                                   TO  SNTENSU-EDANUM
                   MOVE    SNTENSUH30-DEKIDAKAKBN (SNTENSU-IDX)
                                   TO  SNTENSU-DEKIDAKAKBN
                   MOVE    SNTENSUH30-TOKUBETSUKBN (SNTENSU-IDX)
                                   TO  SNTENSU-TOKUBETSUKBN
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
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