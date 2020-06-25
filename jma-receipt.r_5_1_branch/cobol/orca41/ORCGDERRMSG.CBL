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
       PROGRAM-ID.                     ORCGDERRMSG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ǡ��������å�
      *  ����ݡ��ͥ��̾  : �ǡ��������å����֡ʥ��顼��å������ֵѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  18/12/27    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "D01COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-D01KYOUTU.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   "0030" OR "0070" )
               CONTINUE
           ELSE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           EXIT PROGRAM
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    SPACE           TO  SPA-D01-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "8900"
               MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               STRING "���롼�װ��ŵ��ؤ�������Ǥ���" DELIMITED BY SIZE
                      "������λ���Ʋ�������"         DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "�ꥯ�������ֹ�˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0097"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0098"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0203"
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0204"
               STRING  "�Żҥ���ƣգɣĤ�Ⱦ�Ѥαѿ�����"
                                                       DELIMITED BY SIZE
                       "�ϥ��ե󡢥�������С��ʳ���ʸ�������Ϥ����"
                                                       DELIMITED BY SIZE
                       "���ޤ���"                      DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-D01-ERRMSG
               STRING  "���륫�գɣĤ�̤���ϤǤ���"    DELIMITED BY SIZE
               INTO  SPA-D01-ERRMSG
               END-STRING
           WHEN    "0001"
               MOVE    "����ǯ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0002"
               MOVE    "����ǯ������Ϥ��Ƥ���������"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0003"
               MOVE    "���ֻ��곫���������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0004"
               MOVE    "���ֻ��꽪λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0005"
               MOVE "���ֻ��곫��������ֻ��꽪λ���ȤʤäƤ��ޤ���" 
                                       TO  SPA-D01-ERRMSG
           WHEN    "0006"
               MOVE "������ʬ�����Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-D01-ERRMSG
           WHEN    "0007"
               MOVE    "������ʬ�����Ϥ��Ƥ���������"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0008"
               MOVE    "�����å��оݤ��ݸ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0009"
               MOVE    "�����å��оݤ��ݸ������Ϥ��Ƥ���������"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0010"
               MOVE    "��ǧ���ܤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0011"
               MOVE    "��ǧ���ܤ����Ϥ��Ƥ���������"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0012"
               MOVE    "���������ν��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0013"
               MOVE    "����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0014"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0015"
               MOVE    "�����δ��Ԥ�¸�ߤ��ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0016"
               MOVE    "����ǯ��ο��ž���¸�ߤ��ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0017"
               MOVE    "�������Ԥ����Ϥ�ʣ��¸�ߤ��Ƥ��ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0040"
               MOVE    "�оݤΥ���ִ���������ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0041"
               MOVE    "�оݤν����Ϥ���ޤ���"
                                       TO  SPA-D01-ERRMSG
           WHEN    "4001" THRU "4003"
           WHEN    "4101" THRU "4104"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-D01-ERRMSG
           WHEN    "0000"
               MOVE    "���ｪλ"
                                       TO  SPA-D01-ERRMSG
           END-EVALUATE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *