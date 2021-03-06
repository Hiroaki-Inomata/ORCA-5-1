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
      *    ������Ģ�������˥ѥ�᥿�����ѥ�   COPY (CPORCHCN62.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 04.05.00     NACL-¿��Ǽ  09/10/07  �����Ϳ����ɲ�
      * 04.06.00     NACL-¿��Ǽ  10/03/09  �����ɲ�
      * 04.07.00     NACL-¿��Ǽ  12/09/0X  ����̾�������ɲ�
      * 04.07.00     NACL-¿��Ǽ  13/07/XX  �ѣҡ��ãӣֶ�ʬ�ɲ�
      * 05.00.00     NACL-¿��Ǽ  17/05/16  �����ֹ��ɲ�
      ************************************************************
      *
       01  ORCHCN62AREA.
      *    ������
           03  ORCHCN62-SRYYMD          PIC X(08).
      *    �������
           03  ORCHCN62-CNT             PIC 9(03).
      *    �ɥ�����
           03  ORCHCN62-DRCD            PIC X(05).
      *    ����
           03  ORCHCN62-BTUNAME         PIC X(20).
      *    �¼�
           03  ORCHCN62-ROOMNO          PIC X(06).
      *H29.5
      *    �����ֹ�
           03  ORCHCN62-BTUNUM          PIC X(02).
      *    ��������
           03  ORCHCN62-DATA-REC.
      *        ������
               05  ORCHCN62-ZAI-MAX         PIC 9(03).
      *        �ޥơ��֥�
               05  ORCHCN62-REC-TBL         OCCURS   100.
      *            �޲��
                   07  ORCHCN62-ZAIKAIKEI       PIC 9(03).
      *            ���Ŷ�ʬ
                   07  ORCHCN62-SRYKBN          PIC X(02).
      *            ���ż���
                   07  ORCHCN62-SRYSYUKBN       PIC X(03).
      *            ���ֹ�
                   07  ORCHCN62-ZAINUM          PIC  9(08).
      *            ������
                   07  ORCHCN62-NAI-TBL         OCCURS   50.
      *                ������
                       09  ORCHCN62-SRYCD           PIC  X(09).
      *                ����
                       09  ORCHCN62-SRYSURYO        PIC  9(05)V9(05).
      *                ����ʳ�������
                       09  ORCHCN62-SRYKAISU        PIC  9(03).
      *                ������̾��
      *R02.4           09  ORCHCN62-INPUTCOMENT     PIC  X(80).
                       09  ORCHCN62-INPUTCOMENT     PIC  X(140).
      *                ����̾������
                       09  ORCHCN62-SHO-SRYCD       PIC  X(09).
      *                �����Ϳ���
                       09  ORCHCN62-KANSURYO        PIC  9(05)V9(05).
      *                ���̡ʣ���ī�������롢�����롢������������������
                       09  ORCHCN62-YOURYOU-TBL.
                           11  ORCHCN62-YOURYOU     PIC 9(05)V9(05)
                                                   OCCURS   5.
      *            ��ˡ�����ɡʶ��򤢤��
                   07  ORCHCN62-YSRYCD          PIC X(09).
      *
      *    �ѣҽ��϶�ʬ
           03  ORCHCN62-QRKBN          PIC X(01).
      *    �ãӣֽ��϶�ʬ
           03  ORCHCN62-CSVKBN         PIC X(01).
      *    �ãӣֽ��ϤΤ߶�ʬ
           03  ORCHCN62-PRTKBN         PIC X(01).
      *
      *
