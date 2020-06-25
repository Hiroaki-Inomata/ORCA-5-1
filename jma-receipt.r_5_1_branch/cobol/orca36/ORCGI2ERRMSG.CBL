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
       PROGRAM-ID.                     ORCGI2ERRMSG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �������ԾȲ�
      *  ����ݡ��ͥ��̾  : �������ԾȲ񥵥֡ʥ��顼��å������ֵѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/09/19    NACL������    ��������
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
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "�����δ��Ԥ�¸�ߤ��ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "���Ի�̾�����Ѥ����Ϥ��Ʋ�������"
                                                   TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "����ǯ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "��������Ѥ����Ϥ��Ʋ�������"
                                                   TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "���������������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "��λ�����������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "���Ųʤ����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "���֤����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "�ϰϳ����ֹ椬���Ϥ���Ƥ��ޤ�"
                                                   TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "�����ౡ�������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "��λ�ౡ�������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "�ౡ��ͳ�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "��˴��ʬ�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "��λǯ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "������ǯ���������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "��λ��ǯ���������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "�ƥ��ȴ��Զ�ʬ�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "�¤ӽ磱�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0022"
               MOVE    "�¤ӽ磲�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0023"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0024"
               MOVE    "�¤ӽ磳�����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0025"
               MOVE    "����������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0026"
               MOVE    "������֤����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0027"
               MOVE    "�������ǯ������Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0028"
               MOVE    "���������̤����Ϥ˸��꤬����ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE    "������郎���Ϥ���Ƥ��ޤ���"
                                                   TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "�����������佪λ�������ȤʤäƤ��ޤ���" 
                                                   TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "�����ౡ���佪λ�ౡ���ȤʤäƤ��ޤ���" 
                                                   TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "������ǯ�����佪λ��ǯ�����ȤʤäƤ��ޤ���" 
                                                   TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "�оݤδ��Ԥ����򤵤�Ƥ��ޤ���" 
                                                   TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "�������˳������봵�Ԥ�¸�ߤ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE    "���Ϸ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "��λ��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE    "���Ϸ����������̤��������Ķ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "���Ϸ���佪λ����ȤʤäƤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0201"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0202"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "���륫�գɣĤ�̤���ϤǤ���"    DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
                      "�����³����ԤäƤ���������"   DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0204"
               MOVE  "¾��ü�����Ʊ������ƣգɣĤǤ���³������ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "4001"      THRU    "4002"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-ERRMSG
           WHEN    "8900"
               MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "���롼�װ��ŵ��ؤ�������Ǥ���" DELIMITED BY SIZE
                      "������λ���Ʋ�������"         DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "�ꥯ�������ֹ�˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0097"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-ERRMSG
           WHEN    "0000"
               MOVE    "���ｪλ"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           EXIT PROGRAM
           .
      *