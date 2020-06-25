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
       PROGRAM-ID.                     ORCGSERRMSG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��Ǽ
      *  ����ݡ��ͥ��̾  : ��Ǽ���֡ʥ��顼��å������ֵѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  16/10/04    NACL������    ��������
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
      *    ��¾���極�֥ѥ�᥿
           COPY    "CPORCSLOCK.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSLOCKAREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "���Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "�����ä���ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "�����ä���ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "�ֶ⤬��ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "���⤬��ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "�������ϤǤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "�ʥǥӥåȤǤ�����ϤǤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0009" 
               MOVE    "���ŷ�������������Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "�����úѤߤΰ١���ȯ�ԤϤǤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "������������Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "������������Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "�оݤο���ǯ����������Ϥ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0030"
               MOVE    "���Ųʤ����Ϥ˸��꤬����ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0031"
               MOVE    "�����δ��Ԥ�¸�ߤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0033"
               MOVE    "����ǯ������Ϥ˸��꤬����ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0034"
               MOVE    "������ʬ�����Ϥ˸��꤬����ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0050"
               MOVE "�����ֹ桦����ǯ�̤���ϤǤ���" 
                                               TO  SPA-ERRMSG
      *
           WHEN    "0060"
               MOVE "�������볰�����᤬���Ĥ���ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0061"
               MOVE "�����úѤߤΰ١��Ʒ׻��ϤǤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0062"
               MOVE    SPACE                   TO  SPA-ERRMSG
               STRING  "�Ʒ׻��Ǥ��ޤ���"    DELIMITED BY SIZE
                       "���Ź԰ٶ�̳��������ԤäƤ���������"
                                               DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0063"
               MOVE "�������򤬸��Ĥ���ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0064"
               STRING  "���Ʒ׻��Ǥ��ޤ���"
                                               DELIMITED BY SIZE
                       "���Ź԰ٶ�̳��������ԤäƤ���������"
                                               DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0065"
               MOVE    "�����ֹ�����Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "0071"
               MOVE "�����ֹ椬̤���ϤǤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0072"
               MOVE "�����ֹ�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0073"
               MOVE "����������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0074"
               MOVE "�������δ��ֻ��곫���������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0075"
               MOVE "�������δ��ֻ��꽪λ�������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0076"
               MOVE "�ֵ����ƻؼ������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0077"
               MOVE "�¤��ؤ����ܤ����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0078"
               MOVE "�¤ӽ�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0079"
               MOVE    "���Ϸ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0080"
               MOVE    "��λ��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0081"
               MOVE    "���Ϸ����������̤��������Ķ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0082"
               MOVE    "���Ϸ���佪λ����ȤʤäƤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0083"
               MOVE "���ŷ�δ��ֻ��곫�Ϸ�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0084"
               MOVE "���ŷ�δ��ֻ��꽪λ������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0085"
               MOVE "�������¸�ߤ��ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0086"
               MOVE    "������ʬ�����Ϥ��Ƥ���������"
                                               TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE "��ɼ�ֹ�����Ϥ��Ƥ���������" 
                                               TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE "��ɼ�ֹ�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE "���ֶ��ʬ�����Ϥ��Ƥ���������" 
                                               TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE "���ֶ��ʬ�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE "��������ĥե饰�����Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE "������������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "�ֶ�ۤ����Ϥ��Ƥ���������"
                                               TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE "������ˤʤ�ޤ�������Ǥ��ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE "������Τʤ�������Ф����ֶ�ϤǤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE "�ֶ�ۤ����Ϥ˸��꤬����ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE "�������Ķ�����ֶ�ϤǤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0111"
               MOVE "Ʊ����������ʣ�����ϤϤǤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE "���ֶ��������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0113"
               MOVE "�������δ��ֻ��곫���������Ϥ��Ƥ���������" 
                                               TO  SPA-ERRMSG
           WHEN    "0114"
               MOVE "���ŷ�δ��ֻ��곫�Ϸ�����Ϥ��Ƥ���������" 
                                               TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "�����ֹ�����Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "0116"
               MOVE    "���پ���¸�ߤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0117"
               MOVE    "���ٿ�����¤�ã���Ƥ��뤿������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0118"
               MOVE    "����ʬ�������äϤǤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0119"
               MOVE    "���������úѤߤǤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0120"
               MOVE    "����ǯ������Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "0121"
               MOVE    "�������μ��������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0122"
               MOVE    "���������ٽ������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0123"
               MOVE    "Ģɼ�ΰ����ؼ�������ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0125"
               MOVE "���ֻ��곫���������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0126"
               MOVE "���ֻ��꽪λ�������Ϥ˸��꤬����ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0127"
               MOVE "���ֻ��곫��������ֻ��꽪λ���ȤʤäƤ��ޤ���" 
                                               TO  SPA-ERRMSG
           WHEN    "0128"
               MOVE "�����оݤ��������¸�ߤ��ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0129"
               MOVE "�����������μ���ȯ����ˡ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0130"
               MOVE    "����ǯ������Ϥ��Ƥ���������"
                                               TO  SPA-ERRMSG
           WHEN    "0131"
               MOVE    "�����⡼�ɤ����Ϥ˸��꤬����ޤ���"
                                               TO  SPA-ERRMSG
           WHEN    "0132"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�����⡼�ɤ�ͭ���ˤ����硢"
                                                       DELIMITED BY SIZE
                       "�ߥɥ륦�����򹹿����Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "���륫�գɣĤ�̤���ϤǤ���"    DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0206"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
                      "�����³����ԤäƤ���������"   DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0207"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0208"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0209"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�����³���ȴ����ֹ椬�ۤʤ�ޤ���"
                                                       DELIMITED BY SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0210"
               MOVE    "�����³����λ���Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "����������������¸�ߤ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "1038"
               MOVE  "¾��ü�����Ʊ������ƣգɣĤǤ���³������ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3004"
               MOVE    "������ˡ�����򤷤Ʋ�������"
                                       TO  SPA-ERRMSG
           WHEN    "3005"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3006"
               MOVE    "���٤����򤷤Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3007"
               MOVE    "����ۤ����Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3008"
               MOVE    "�����׳ۤ����ߤ򲼲��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3009"
               MOVE    "����Τ������٤����򤷤Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3010"
               MOVE    "�����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3011"
               MOVE    "������ˡ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3012"
               MOVE    "�����������Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3013"
               MOVE    "�ޥ��ʥ�������ۤ����Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3014"
               MOVE    "����ۤ����ߤ򲼲��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3016"
               MOVE    "�����ۤ����ߤ򲼲��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3017"
               MOVE    "Ĵ���⣱�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3018"
               MOVE    "Ĵ���⣲�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3019"
               MOVE    "����ۤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3020"
               MOVE    "Ĵ��������Ϥ���ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3021"
               MOVE    "Ĵ��������Ϥ���ǽ������ǤϤ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3022"
               MOVE    "�ޥ��ʥ�������ۤ����Ϥ��Ƥ���������"
                                       TO  SPA-ERRMSG
           WHEN    "3023"
               MOVE    "���������ۤ���¤�Ķ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3024"
               MOVE    "�����ۤ���¤�Ķ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "3025"
               MOVE    "����ۤ���¤�Ķ���ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1001"  THRU    "1007"
           WHEN    "4000"  THRU    "4011"
           WHEN    "4900"  THRU    "4911"
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
           WHEN    "0092"
               MOVE    "�ꥯ�����ȼ��̤˸��꤬����ޤ���"
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
           WHEN    "9999"
               MOVE    "¾ü���ǻ�����Ǥ���"
                                               TO  SPA-ERRMSG
               MOVE    SLOCK-MSG               TO  SPA-ERRMSG(21:)
           WHEN    "0000"
               MOVE    "���ｪλ"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           EXIT PROGRAM
           .
      *