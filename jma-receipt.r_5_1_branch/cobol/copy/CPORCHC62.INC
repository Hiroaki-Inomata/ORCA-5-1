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
      *----------------------------------------------------------*
      *    ������Ģ�ѥ�᥿�����ѥ�        COPY (CPORCHC62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.00    NACL-¿��Ǽ  10/06/22  ��ɼ�ֹ��ɲ�
      *  04.06.00    NACL-¿��Ǽ  10/03/09  �����ɲ�
      *  04.07.00    NACL-¿��Ǽ  12/09/0X  ����̾�������ɲ�
      *  04.07.00    NACL-¿��Ǽ  13/07/XX  �ѣҡ��ãӣֶ�ʬ�ɲ�
      ************************************************************
      *
       01  ORCHC62AREA.
      *    ������
           03  ORCHC62-SRYYMD          PIC X(08).
      *    �������
           03  ORCHC62-CNT             PIC 9(03).
      *   �ɥ�����
           03  ORCHC62-DRCD            PIC X(05).
      *    ��������
           03  ORCHC62-DATA-REC.
      *        ������
               05  ORCHC62-ZAI-MAX         PIC 9(03).
      *        �ޥơ��֥�
               05  ORCHC62-REC-TBL         OCCURS   100.
      *            �޲��
                   07  ORCHC62-ZAIKAIKEI       PIC 9(03).
      *            ���Ŷ�ʬ
                   07  ORCHC62-SRYKBN          PIC X(02).
      *            ���ż���
                   07  ORCHC62-SRYSYUKBN       PIC X(03).
      *            ���ֹ�
                   07  ORCHC62-ZAINUM          PIC  9(08).
      *            ������
                   07  ORCHC62-NAI-TBL         OCCURS   50.
      *                ������
                       09  ORCHC62-SRYCD           PIC  X(09).
      *                ����
                       09  ORCHC62-SRYSURYO        PIC  9(05)V9(05).
      *                ����ʳ�������
                       09  ORCHC62-SRYKAISU        PIC  9(03).
      *                ������̾��
      *R02.4           09  ORCHC62-INPUTCOMENT     PIC  X(80).
                       09  ORCHC62-INPUTCOMENT     PIC  X(140).
      *                ����̾������
                       09  ORCHC62-SHO-SRYCD       PIC  X(09).
      *                �����Ϳ���
                       09  ORCHC62-KANSURYO        PIC  9(05)V9(05).
      *                ���̡ʣ���ī�������롢�����롢������������������
                       09  ORCHC62-YOURYOU-TBL.
                           11  ORCHC62-YOURYOU     PIC 9(05)V9(05)
                                                   OCCURS   5.
      *            ��ˡ�����ɡʶ��򤢤��
                   07  ORCHC62-YSRYCD          PIC X(09).
      *
      *    ��ɼ�ֹ�ʼ����ɼ�ֹ��
           03  ORCHC62-DENPNUM         PIC 9(07).
      *    ��ɼ�ֹ�ʽ������Ƥ��о���ɼ�ֹ��
           03  ORCHC62-GRP-DENPNUM-G.
               05  ORCHC62-GRP-DENPNUM     PIC 9(07)
                                           OCCURS   10.
      *
      *    �ѣҽ��϶�ʬ
           03  ORCHC62-QRKBN           PIC X(01).
      *    �ãӣֽ��϶�ʬ
           03  ORCHC62-CSVKBN          PIC X(01).
      *    �ãӣֽ��ϤΤ߶�ʬ
           03  ORCHC62-PRTKBN          PIC X(01).
      *    ȯ�Բ��̶�ʬ��1:�����ǧ��2:���ӽ�λ��3���������
           03  ORCHC62-PGKBN           PIC X(01).
      *
