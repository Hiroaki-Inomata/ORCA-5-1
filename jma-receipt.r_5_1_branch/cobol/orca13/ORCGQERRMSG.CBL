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
       PROGRAM-ID.                     ORCGQERRMSG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ���ԾȲ񥵥֡ʥ��顼��å������ֵѡ�
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
           COPY    "Q01SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SPA-Q01FREE.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
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
           MOVE    SPACE               TO  SPA-Q01-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "���Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0002"
               MOVE    "�����ֹ�η�����㤤�ޤ���" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0003"
               MOVE    "�оݷ������������򥪡��С����ޤ�����" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0004"
               MOVE    "���Υ����ɤ���̾����Ͽ����Ƥ��ޤ���" 
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0005"
               MOVE    "���Υ����ɤι԰�̾����Ͽ����Ƥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0006"
               MOVE    "̤���������ϤǤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0007"
               MOVE    "���åȥ����ɤ����ϤǤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0008"
               MOVE    "���ܤ����ꤷ�Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0009"
               MOVE    "����ʸ���ǤΤ����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0101"
               MOVE    "����ǯ���������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0102"
               MOVE    "��ǯ���������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0103"
               MOVE    "ǯ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0104"
               MOVE    "����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0105"
               MOVE    "�ǽ��������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0106"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0107"
               MOVE    "��̾�����ɤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0108"
               MOVE    "���Ź԰٥����ɤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0109"
               MOVE    "���̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0110"
               MOVE    "�ݸ��μ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0111"
               MOVE    "����μ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0112"
               MOVE  "����̾�����Ϥ˸��꤬����ޤ��ʸ�����̡����ܡˡ�"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0113"
               MOVE    "�ܿͲ�²�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0114"
               MOVE    "���֣������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0115"
               MOVE    "���֣������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0116"
               MOVE    "���֣������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0117"
               MOVE    "���Ųʤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0118"
               MOVE    "������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0119"
               MOVE    "������̾��ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0120"
               MOVE    "ž����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0121"
               MOVE    "����ž���������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0122"
               MOVE    "��λž���������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0123"
               MOVE    "���Ȼ�ͳ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0124"
               MOVE    "����������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0125"
               MOVE    "��������ֳ��Ϥ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0126"
               MOVE    "��������Ϸ�̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0127"
               MOVE    "��������ֽ�λ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0128"
               MOVE    "��˴��ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0129"
               MOVE    "���Ŷ�ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0130"
               MOVE    "���ż��̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0131"
               MOVE    "��������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0132"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�쥻�ץȥ����ȤΥ������"  DELIMITED BY SIZE
                       "�ϣ���ޤǤˤ��Ƥ���������"    DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0133"
               MOVE    "����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0134"
               MOVE    "������̾��ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0135"
               MOVE    "����̾��ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0136"
               MOVE    "�õ������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0137"
               MOVE    "�õ����ॳ���ɤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0138"
               MOVE    "�õ�����̾�Τ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0139"
               MOVE  "����̾�����Ϥ˸��꤬����ޤ��ʸ�����̡������ˡ�"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0140"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    "���̡���������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                 MOVE "���Ź԰ٿ��͹��ܻ����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0141"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE   "���̡ʲ�����Ϲ�סˤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                   MOVE    "���Ź԰ٿ��ͤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0142"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE "���Ź԰٤���Ӿ���ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                 MOVE "���Ź԰ٿ��͹�����Ӿ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "0143"
               MOVE    "�ҳ����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0144"
               MOVE    "��̾��Ӿ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0145"
               MOVE    "�����ѥ쥻�ץȤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0146"
               MOVE    "���Ͽ��ų����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0147"
               MOVE    "��λ���ų����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0148"
               MOVE    "���ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0149"
               MOVE    "�ƥ��ȴ��Ԥ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0150"
               MOVE    "�ݸ�ͭ�����ֽ�λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0151"
               MOVE    "����Ŭ�Ѵ��ֽ�λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0152"
               MOVE    "��ô�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0153"
               MOVE    "�ϰ�Ϣ�Ⱦ����󶡤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0154"
               MOVE    "����ݸ����Ϸ�̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0155"
               MOVE    "����ݸ����ֳ��Ϥ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0156"
               MOVE    "����ݸ����ֽ�λ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0157"
               MOVE    "����ݸ��ݸ����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0158"
               MOVE    "���ǧ�곫�Ϸ�̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0159"
               MOVE    "���ǧ����ֳ��Ϥ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0160"
               MOVE    "���ǧ����ֽ�λ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0161"
               MOVE    "���ǧ���ײ����֤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0162"
               MOVE    "�ɥ��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0163"
               MOVE    "�������ͭ�����ּ��̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0164"
               MOVE    "�����ֹ漱�̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0165"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0166"
               MOVE    "�����ȼ��̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0167"
               MOVE    "������Գ��Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0168"
               MOVE    "������Խ�λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0169"
               MOVE    "�������ͭ�����Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0170"
               MOVE    "�������ͭ����λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0171"
               MOVE    "�������ͭ�������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0172"
               MOVE    "�������ͭ����λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0173"
               MOVE    "�������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0174"
               MOVE    "������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0175"
               MOVE    "�ݸ����ֹ渡����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0176"
               MOVE    "�ݸ����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0177"
               MOVE    "�ݸ��μ��ม����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0178"
               MOVE    "����μ��ม����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0179"
               MOVE    "��ô���ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0180"
               MOVE    "�ݸ����񸡺���ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0181"
               MOVE    "����ݸ�ͭ�����ּ��̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0182"
               MOVE    "����ݸ����Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0183"
               MOVE    "����ݸ���λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0184"
               MOVE    "����ݸ�ͭ�����Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0185"
               MOVE    "����ݸ�ͭ����λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0186"
               MOVE    "����ݸ�ͭ�������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0187"
               MOVE    "����ݸ�ͭ����λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0188"
               MOVE    "����ݸ��ݸ����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0189"
               MOVE    "���ǧ��ͭ�����ּ��̤����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0190"
               MOVE    "���ǧ�곫�Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0191"
               MOVE    "���ǧ�꽪λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0192"
               MOVE    "���ǧ��ͭ�����Ϸ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0193"
               MOVE    "���ǧ��ͭ����λ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0194"
               MOVE    "���ǧ��ͭ�������������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0195"
               MOVE    "���ǧ��ͭ����λ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0196"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0197"
               MOVE    "�¤ӽ磱�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0198"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0199"
               MOVE    "�¤ӽ磲�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0200"
               MOVE    "�¤��ؤ����ܣ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0201"
               MOVE    "�¤ӽ磳�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0202"
               MOVE    "���Ź԰ٸ�����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0203"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0205"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "���륫�գɣĤ�̤���ϤǤ���"    DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0206"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
                      "�����³����ԤäƤ���������"   DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0207"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "��³���֤μ����˼��Ԥ��ޤ�����" DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0208"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�Żҥ���ƣգɣĤ�̤���ϤǤ���"
                                                       DELIMITED BY SIZE
                       "�����³������ľ���Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "0209"
               MOVE    "�ΣϣԸ�����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0210"
               MOVE    "����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0211"
               MOVE    "�����ֹ�η�����㤤�ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0212"
               MOVE    "������̥ǡ���������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0213"
               MOVE    "������̥ꥹ�Ȱ�����ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0214"
               MOVE    "ɽ�������ʸ���Τߤ����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0215"
               MOVE    "�о�̾������ʸ���Τߤ����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1001"
               MOVE    "���Ͽ���ǯ�����佪λ����ǯ�����ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1002"
               MOVE    "������ǯ�����佪λ��ǯ�����ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1003"
               MOVE    "����ǯ��佪λǯ��ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1004"
               MOVE    "��ǯ������ǯ���Ʊ�������ϤǤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1005"
               MOVE    "ǯ������Ϥ����硢�������ɬ�ܹ��ܤǤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1006"
               MOVE    "���ϴ����ֹ�佪λ�����ֹ�ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1007"
               MOVE    "������郎���Ϥ���Ƥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1008"
               MOVE    "���Ϻǽ��������佪λ�ǽ��������ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1009"
               MOVE    "����ž�����佪λž�����ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1010"
               MOVE    "�������ͭ�����֤λ���˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1011"
               MOVE    "������γ��Ϸ�����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1012"
               MOVE    "������ν�λ������Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1013"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���������Ȳ񤹤��硢"
                                                       DELIMITED BY SIZE
                           "���Ͽ���ǯ�����Ƚ�λ����ǯ������"
                                                       DELIMITED BY SIZE
                           "Ʊ��������դ����Ϥ��Ƥ���������"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "����������Ԥ���硢"      DELIMITED BY SIZE
                           "���ŷ�ܥ�������ŷ����ꤷ�Ƥ���������"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1014"
               MOVE    "��Ӿ������򤷤Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1015"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    "���̡���������ʬ�����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
               ELSE
                   MOVE    "���ͻ����Ԥ����ܤ����򤷤Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
               END-IF
           WHEN    "1016"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��̡���������Ʊ�����Ź԰٤�"
                                               DELIMITED BY SIZE
                           "ʣ�����Ϥ���Ƥ��ޤ���"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��ͻ����Ʊ�����Ź԰٤�"
                                               DELIMITED BY SIZE
                           "ʣ�����Ϥ���Ƥ��ޤ���"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1017"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��̡���������Ԥ���硢"
                                               DELIMITED BY SIZE
                           "�оݤȤʤ���Ź԰٤����Ϥ��Ƥ���������"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��ͻ����Ԥ���硢"
                                               DELIMITED BY SIZE
                           "�оݤȤʤ���Ź԰٤����Ϥ��Ƥ���������"
                                               DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1018"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��̡���������Ԥ���硢"
                                           DELIMITED BY SIZE
                           "����ǯ�����δ��ֻ���򤷤Ƥ�������"
                                           DELIMITED BY SIZE
                           "�ʺ��磱������ˡ�"
                                           DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "���Ź԰٤ο��ͻ����Ԥ���硢"
                                           DELIMITED BY SIZE
                           "����ǯ�����δ��ֻ���򤷤Ƥ�������"
                                           DELIMITED BY SIZE
                           "�ʺ��磱������ˡ�"
                                           DELIMITED BY SIZE
                   INTO    SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1019"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�����ȿ��Ź԰٤ο��ͻ����"
                                           DELIMITED BY SIZE
                       "�����줫�Τ����ꤷ�Ƥ���������"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1020"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�ΣϣԸ�����Ԥ���硢"
                                           DELIMITED BY SIZE
                       "����ǯ�����δ��ֻ���򤷤Ƥ�������"
                                           DELIMITED BY SIZE
                       "�ʺ��磱������ˡ�"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1021"
               MOVE    "���Ͽ��ų������佪λ���ų������ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1022"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "���ʬ��ޤޤʤ�������Ԥ���硢�оݤȤʤ�"
                                           DELIMITED BY SIZE
                       "���Ź԰٤����Ϥ��Ƥ���������"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1023"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "�ݸ�ͭ�����ֽ�λ��θ�����Ԥ���硢"
                                           DELIMITED BY SIZE
                          "�ݸ����ֹ�ޤ����ݸ��μ����"
                                           DELIMITED BY SIZE
                          "���Ϥ��Ƥ���������"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1024"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "�ݸ�ͭ�����ֽ�λ��θ�����Ԥ���硢"
                                           DELIMITED BY SIZE
                          "�ΣϣԸ����ϤǤ��ޤ���"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1025"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "����Ŭ�Ѵ��ֽ�λ��θ�����Ԥ���硢"
                                           DELIMITED BY SIZE
                          "����μ���ޤ�����ô���ֹ��"
                                           DELIMITED BY SIZE
                          "���Ϥ��Ƥ���������"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1026"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING     "����Ŭ�Ѵ��ֽ�λ��θ�����Ԥ���硢"
                                           DELIMITED BY SIZE
                          "�ΣϣԸ����ϤǤ��ޤ���"
                                           DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1027"
               IF    ( SPA-MOTOPG      =   "API" )
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "��ô��縡����Ԥ���硢"  DELIMITED BY SIZE
                           "���Ͽ���ǯ�����Ƚ�λ����ǯ������"
                                                       DELIMITED BY SIZE
                           "Ʊ��������դ����Ϥ��Ƥ���������"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               ELSE
                   MOVE    SPACE           TO  SPA-Q01-ERRMSG
                   STRING  "��ô��縡����Ԥ���硢"  DELIMITED BY SIZE
                           "���ŷ�ܥ�������ŷ����ꤷ�Ƥ���������"
                                                       DELIMITED BY SIZE
                   INTO  SPA-Q01-ERRMSG
                   END-STRING
               END-IF
           WHEN    "1028"
               MOVE    "����ݸ���ͭ�����֤λ���˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1029"
               MOVE    "����ݸ��γ��Ϸ�����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1030"
               MOVE    "����ݸ��ν�λ������Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1031"
               MOVE    "���ǧ���ͭ�����֤λ���˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1032"
               MOVE    "���ǧ��γ��Ϸ�����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1033"
               MOVE    "���ǧ��ν�λ������Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1034"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "�ɥ������λ����Ԥ���硢"
                                           DELIMITED BY SIZE
                       "����ǯ�����δ��ֻ���򤷤Ƥ�������"
                                           DELIMITED BY SIZE
                       "�ʺ��磱������ˡ�"
                                           DELIMITED BY SIZE
               INTO    SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1036"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "���Ź԰پ����Ȳ񤹤��硢"
                                                       DELIMITED BY SIZE
                       "���̡���������ʬ�����Ϥ��Ƥ���������"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1037"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "���������Ȳ񤹤��硢"
                                                       DELIMITED BY SIZE
                       "���������Ϥ��Ƥ���������"      DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1038"
               MOVE  "¾��ü�����Ʊ������ƣգɣĤǤ���³������ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1039"
               MOVE  "������郎¿�����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1040"
               MOVE    "������Ǥ������Ф餯���Ԥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1041"
               MOVE    "�������˳������봵�Ԥ�¸�ߤ��ޤ���Ǥ�����"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1042"
               MOVE    "���Ϸ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1043"
               MOVE    "��λ��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1044"
               MOVE    "���Ϸ����������̤��������Ķ���Ƥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1045"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING  "���̡���������ʬ�����Ϥ���Ƥ������"
                                                       DELIMITED BY SIZE
                       "���Ź԰پ���ξȲ�Τ߲�ǽ�Ǥ���"
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1046"
               MOVE    "���Ϸ���佪λ����ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1047"
               MOVE    "ž����ʬ�����Ϥ��Ƥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1048"
               STRING  "�ΣϣԸ�����ʬ�����Ϥ���Ƥ����硢"
                                                       DELIMITED BY SIZE
                  "���̡ʲ�����Ϲ�סˤ������Ӿ������ϤǤ��ޤ���"
                                                       DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1049"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1050"
               MOVE    "�����������������ȤʤäƤ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1051"
               STRING  "������̥ꥹ�Ȱ�����ʬ�ޤ���"  DELIMITED BY SIZE
                       "������̥ǡ���������ʬ"        DELIMITED BY SIZE
                       "�����Ϥ��Ƥ���������"          DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
           WHEN    "1052"
               MOVE    "�����¹���Ǥ������Ф餯���Ԥ���������"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1053"
               MOVE    "�оݤν����Ϥ���ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "1054"
               MOVE    "�ե�����κ����˼��Ԥ��ޤ�����"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "4001"      THRU    "4016"
               MOVE    "���������˼��Ԥ��ޤ�����"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "8900"
               MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8901"
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8902"
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8903"
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8905"
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-Q01-ERRMSG
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "8915"
               MOVE    SPACE           TO  SPA-Q01-ERRMSG
               STRING "���롼�װ��ŵ��ؤ�������Ǥ���" DELIMITED BY SIZE
                      "������λ���Ʋ�������"         DELIMITED BY SIZE
               INTO  SPA-Q01-ERRMSG
               END-STRING
               MOVE    "0089"          TO  SPA-ERRCD
           WHEN    "0091"
               MOVE    "�ꥯ�������ֹ�˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0097"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0098"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-Q01-ERRMSG
           WHEN    "0000"
               MOVE    "���ｪλ"
                                       TO  SPA-Q01-ERRMSG
           END-EVALUATE
      *
           .
      *
       200-MAIN-EXT.
           EXIT.
      *