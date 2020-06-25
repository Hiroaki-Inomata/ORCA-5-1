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
       PROGRAM-ID.                     ORCS02MSG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ��ô��׻����֥��顼��å������Խ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  13/01/28    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       DATA                            DIVISION.
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-MSG                 PIC X(100)  OCCURS 3.
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCS02MSG.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           S02MSG-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  S02MSG-MSG-AREA
      *
           EVALUATE    S02MSG-S02-RC
           WHEN    ZERO
               CONTINUE
           WHEN    13
               STRING  "�ౡ�׻���ɬ�פʲ�׾��󤬺�������Ƥ��ޤ���"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    14
               STRING  "�������Ȱۤʤ��ݸ����������û�������ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    15
               STRING  "���������������û������ꤵ��Ƥ��ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    16
               STRING  "�������λ��̵꤬�������������û���"
                                           DELIMITED   BY  SIZE
                       "���ꤵ��Ƥ��ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    17
               STRING  "��������ͭ�����֤��ڤ�Ƥ��ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    18
               STRING  "�ݸ��ȹ礻��ͭ�����֤��ڤ�Ƥ��ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    19
               STRING  "��������μ�������ʬ��������"
                                           DELIMITED   BY  SIZE
                       "��ǧ���Ƥ�������"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    20
               STRING  "���������˸��꤬����ޤ���"
                                           DELIMITED   BY  SIZE
                       "��ư�����򤷤Ƥ�������"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    21
               STRING  "�ߵ������������λ�������"
                                           DELIMITED   BY  SIZE
                       "�������ʲû��ˤλ��꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    22
               STRING  "¾����ǡʳ���ˤ����Ϥ˸��꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    23
               STRING  "Ʊ�����������˼��������ꥳ����"
                                           DELIMITED   BY  SIZE
                       "�����Ϥ��Ƥ�������"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    24
               STRING  "Ʊ�����������κ�����ʬ���������û�"
                                           DELIMITED   BY  SIZE
                       "�����ꤵ��Ƥ��ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    25
               STRING  "���������������ǧ���Ƥ�������"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    26
               STRING  "��������ౡ�����ʳ���ˤ����Ϥ˸��꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    27
               STRING  "�����������ౡ�����ʳ���ˤ����Ϥ�"
                                           DELIMITED   BY  SIZE
                       "���꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    29
               STRING  "�������������ܡʣ��˻���������̿�"
                                           DELIMITED   BY  SIZE
                       "�û��λ��꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    30
               STRING  "�ݸ��ȹ礻�ο���¿�����ޤ�"
                                           DELIMITED   BY  SPACE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    31
               STRING  "�������������ܡʣ��˻������ήư��"
                                           DELIMITED   BY  SIZE
                       "�����Ϥ�����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    33
               STRING  "��������������������������ܡפ�̤����Ǥ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    34
               STRING  "Ŭ�ѤǤ��ʤ�������������Ϥ�����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    35
               STRING  "Ʊ�����˿����ݸ����ꥳ���ɤ����Ϥ�"
                       "����ʾ夢��ޤ�"  DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    36
               STRING  "�����ݸ����ꥳ���ɤ��������˿����λ���"
                       "������ޤ���"      DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    37
               STRING  "�ݸ��ȹ礻��¸�ߤ��ޤ���"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    38
               STRING  "���������ι�פ���¤�Ķ���ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    39
               STRING  "�ݸ�����ۤι�פ���¤�Ķ���ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    40
               STRING  "��ô���׻��ۤ���¤�Ķ���ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    41
               STRING  "���������ۤ���¤�Ķ���ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    42
               STRING  "�������¾��ۿ�����ô�����֤��оݥ����ɤ�"
                                           DELIMITED   BY  SIZE
                       "�������Ϥ��ǧ"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    43
               STRING  "���������ʬ�׻����Ƚ�����ʬ���ۤʤ�ޤ�"
                                           DELIMITED   BY  SIZE
                       "���׳�ǧ��"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           WHEN    101
               STRING  "����������֤˻���Ǥ��ʤ��������û�"
                                           DELIMITED   BY  SIZE
                       "�λ��꤬����ޤ�"
                                           DELIMITED   BY  SIZE
               INTO    WRK-MSG (1)
               END-STRING
           END-EVALUATE
      *
           MOVE    WRK-MSG (1)         TO  S02MSG-MSG (1)
                                           S02MSG-MSG (2)
                                           S02MSG-MSG (3)
      *
      *
           .
           EXIT PROGRAM
           .